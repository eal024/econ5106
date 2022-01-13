


# 
library(tidyverse)

mte_data <- haven::read_dta("data/mtedata.dta") %>% mutate( z1 = if_else( z > 0, 1, 0 ))


# a) complute the LATE
# About z, u
mte_data %>% skimr::skim()


mte_data %>%  select( u,z) %>% pivot_longer( everything()) %>% 
    group_by( name) %>% 
    summarise( mean = mean(value),
               sd = sd(value),
               min = min(value)
               )

# uniform var = (1/12)*(b-a)^2


# MTE: 
df_potential <- mte_data %>% 
    mutate( u_r = round(u, 2)) %>% 
    select(u_r, y1,y0) %>% 
    pivot_longer( -u_r) %>% 
    group_by(u_r, name) %>%
    summarise( value = mean(value)
               ) %>% 
    arrange( name, u_r)

df_potential %>% ggplot( aes(x = u_r, y = value, fill = name, color = name)) + geom_line()

df_potential %>%
    pivot_wider( names_from = name, values_from = value) %>% 
    mutate( diff = y1-y0) %>% 
    ggplot( aes( x = u_r, y = diff)) + geom_line()
    
    
df_potential %>% ggplot( aes(x = u_r, y = value, fill = name, color = name)) + geom_line()


## Groups:
mte_data %>% 
    mutate( z_i = ifelse( z > 0, 1, 0)) %>% 
    group_by(z_i, d) %>% 
    count( sort = T) %>% 
    pivot_wider( names_from = z_i, values_from = n)

# a) Potential outcome for different groups:
mte_data %>%
    mutate( z1 = if_else( z > 0, 1, 0 ),
            yd = y*d,
            y1_d = y*(1-d)
            ) %>% 
    summarise( 
        b_wald = ( mean( y[z1 == 1])   - mean( y[z1 == 0]) )  /( mean(d[z1 == 1])-mean(d[z1 == 0] ) ) ,
        y1c =    ( mean(yd[z1 == 1])   - mean(yd[z1 == 0]) )  /( mean(d[z1 == 1])-mean(d[z1 == 0] ) ),
        y0c =    ( mean(y1_d[z1 == 1]) - mean(y1_d[z1 == 0]) )/( mean((1-d)[z1 == 1])-mean((1-d)[z1 == 0])),
        y_late = y1c - y0c
        )
    

# Always takers, and never takers
mte_data %>%  summarise( y1a = mean(y[d == 1 & z1 == 0]),
                         y0n = mean(y[d == 0 & z1 == 1])
                         )



# Alternativ, Wald
mte_data %>%
    mutate( z1 = ifelse( z > 0, 1, 0)) %>% 
    estimatr::iv_robust( y ~ d|z1, data = .)


# b) Estimate PO, using control function approach

# The switching regression model
# 1) control function estimation:

model_control   <- mte_data %>% glm( d ~  z1, data = ., family = binomial( "probit"))

#model_control_0 <- mte_data %>% mutate( z1 = ifelse(z > 0, 1, 0)) %>% glm( d ~ I(-z1), data = ., family = binomial( "probit"))

# summary
summary(model_control)


# Mills
zg <- predict(model_control)
imr    <- dnorm(zg)/pnorm(zg)


mte_data_1 <- mte_data %>% mutate( imr = ifelse( d == 1,
                                     dnorm(zg)/pnorm(zg),
                                     -dnorm(zg)/pnorm(-zg)
                                     )
                       )

# Eq 1
model_eq1 <- mte_data_1 %>% filter( d == 1) %>%  lm( y ~ imr, data = .) 

# Eq 0
model_eq0 <- mte_data_1 %>% filter( d == 0) %>%  lm( y ~ imr, data = .) 

# ATE
mte_data_1 %>% 
    mutate( ate = 1*(model_eq1[[1]][[1]] - model_eq0[[1]][[1]] )  
            ) %>% 
    summarise( ate = mean(ate))

# LATE from these equations:
summary(model_control)
summary(model_eq1)
summary(model_eq0)

# From models:
a0 <- -0.5807
a1 <- -0.5807 + 1.1295
b1 <- 2
b0 <- -0.065
imr1 <- -2.115
imr0 <- -0.035

# y1c
y1c <- b1 + imr1*( dnorm(a1) - dnorm(a0) )/(pnorm(a1) -  pnorm(a0) )

# y0c
y0c <- b0 + imr0*( dnorm(a1) - dnorm(a0) )/(pnorm(a1) -  pnorm(a0) )

# LATE
y1c - y0c

# y1a
b1 + imr1*dnorm(a0)/pnorm(a0)

# y0n
b0 - imr0*dnorm(a1)/pnorm(-a1)


# c)  ATE, ATT, ATU---------------------------------------------------------------

# ATE
model_eq1[[1]][[1]]-model_eq0[[1]][[1]]

# ATT
(1/d_n_1)*(d_n_1*(model_eq1[[1]][[1]]-model_eq0[[1]][[1]] ) ) + (imr1-imr0)*dnorm(a1)/pnorm(a1)


# Alternativ
tibble( d = mte_data$d,
        yc1 = predict(model_eq1, newdata = mte_data_1),
        yc0 = predict(model_eq0, newdata = mte_data_1),
        y   = yc1 - yc0
        ) %>% 
    summarise( ate = mean(y),
               att = mean(y[d == 1]),
               atn = mean(y[d == 0]),
               late = y1c - y0c
               )

# LATE
mte_data %>% estimatr::iv_robust( y ~ d|z1, data = .)



# d) Use selectionModels and method = "LM" --------------------------------

# Seem to get the y1 but not y0

library(sampleSelection)

selectionModel_ml <- selection(  mte_data$d ~  mte_data$z1,
                                 mte_data$y ~ 1, 
                                method = 'ml')


summary(selectionModel_ml)


# e) # 3rd degree poly in step 1 Propensity score--------------------------


# MTE using 3 rd degree poly in the Prop.score

# Prop score
model_prop_score   <- mte_data %>% glm( d ~  z , data = ., family = binomial( "probit"))

# Probit 
summary( model_prop_score)

model_ps_poly <- mte_data %>%
    mutate( ps = predict(model_prop_score, type = "response"),
            ps2 = ps^2,
            ps3 = ps^3
            ) %>%
    lm( y ~ ps + ps2 + ps3, data = .)

summary(model_ps_poly)

# Marginal 
graf_1 <- tibble( pr    = seq(from =0, to = 1, by =0.001), 
        dydpr =  model_ps_poly[[1]][2] + 2*model_ps_poly[[1]][3]*pr + + 3*model_ps_poly[[1]][4]*pr^2
        ) %>% 
    ggplot( aes( x = pr, y = dydpr)) +
    geom_line() +
    geom_hline( yintercept = c(0,5,10), linetype = 3)


# f) Use non-parameteric to estimate the 1 stage, reduce form -------------

loess_y_z <- loess( data = mte_data, y ~ z, degree = 2)
loess_d_z <- loess( data = mte_data, d ~ z, degree = 2, )

library(locpol)

with(mte_data, {
    plot(d, z)
    lines(ksmooth(d, z, "normal"), col = 2)
    lines(ksmooth(d, z, "normal" ), col = 3)
})


ksmooth( mte_data$d, pr)

y_z <- locpol( data = as.data.frame(mte_data), y ~ z, deg = 2)
d_z <- locpol( data = as.data.frame(mte_data), d ~ z, deg = 2, kernel = "Epanechnikov" )

tibble( dd = predict(loess_d_z ) )

tibble( fitted(loess_y_z)/fitted(loess_d_z), u = mte_data$u)

fitted(y_z ,newdata = pr)/fitted(d_z, newdata = pr)

tibble(  pr  = seq(from =0, to = 1, by =0.1), 
         mte2 = predict(loess_y_z, newdata = pr)/predict(loess_d_z, newdata = pr)
         ) %>% 
    ggplot( aes( x = pr, y = mte2)) + 
    geom_line()


#
yy <- predict(loess_y_z, newdata = pr)
dd <- predict(loess_d_z, pr)

mte_p <- yy/dd

tibble( dd = predict(loess_d_z, newdata = pr), pr = pr ) %>% ggplot( aes( x = pr, y = dd)) + geom_line()


tibble( mte_p = mte_p, u = mte_data$u) %>%
    group_by( u = round(u, 1)) %>% 
    summarise( mte = mean(mte_p)) %>% 
    ggplot( aes( x = u, y = mte)) + 
    geom_point( ) +
    geom_line()


graf_1 +
    geom_point( data  =mte_data, aes( x = z, y = y ), inherit.aes = F)
    geom_smooth( method = "loess")














