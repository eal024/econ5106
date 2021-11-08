

## Solution go Through LATE
library(tidyverse)

# Lottery data
lottery <- haven::read_dta("data/lottery.dta") %>% 
    mutate( 
        # Create dummy/factor
        year_lotcat = ifelse( year == 1989 & lotcateg != 3, lotcateg*1, 0),  
        lotcateg    = factor(lotcateg)
        )

lottery %>% distinct(year_lotcat)
#  Discuss instrument exogeneity, exclusion and monotonicity.

# (b) Relevance
# correlation
lottery %>% summarise(corr = cov(z, d) / ((var(d) * var(z))) ^ 0.5)

lottery %>%lm( d ~ z , data = ., )

# Robust with controls
lottery %>% estimatr::lm_robust( d ~  z  + I(factor(year)) + I(factor(lotcateg)) + I( factor(year_lotcat)) ,
                                 data = . ) %>% broom::tidy() %>% as_tibble()


# c) Estimate the return of medical schooling with IV

# First stage
model_fs <- lottery %>% lm( d ~ z, data = .)

# Return to medical schooling OLS
model1 <- lottery %>% lm( lnw ~ d, data = .) %>% summary()

lottery$predict_d <- predict(model_fs, data = lottery %>% select(d,z))

lottery %>% lm( lnw ~ predict_d, data =.)


## Estimatr
lottery %>% estimatr::iv_robust( formula = lnw ~ d|z, data = .)


## With controls
model_iv <- lottery %>% estimatr::iv_robust( formula = lnw ~ + I(factor(year)) + I(factor(year_lotcat) ) + I(factor(lotcateg)) + d|z + I(factor(year)) + I(factor(year_lotcat) ) + I(factor(lotcateg)), data = .)

tibble(names =model_iv$term, coeff = model_iv$coefficients, std.error = model_iv$std.error, t = coeff/std.error)

# d) Bootstrap

mod <- function(data){data %>% estimatr::iv_robust( formula = lnw ~ + I(factor(year)) + I(factor(year_lotcat) ) + I(factor(lotcateg)) + d|z + I(factor(year)) + I(factor(year_lotcat) ) + I(factor(lotcateg)), data = .)}

data_liste <- list()

for(i in 1:50){
    data_liste[[i]] <- lottery[sample(nrow(lottery), size = nrow(lottery), replace = T),]
}

df_bootstrap <- tibble( index = 1:length(data_liste), data = data_liste) %>% 
    mutate( 
        model = map(data,  function(x) mod(x) ),
        coeff = map(model, function(x)  x$coefficients[x$term == "d"] )
        )  %>% 
    select(index, coeff) %>% 
    unnest( coeff)

## Boostrap standard errors.
df_bootstrap %>% summarise( mean = mean(coeff), sd = sd(coeff), t = mean/sd)


# d) Count number of compliers
lottery %>% 
    group_by( d,z) %>% 
    count() %>% 
    pivot_wider( names_from = d, values_from = n) %>% janitor::clean_names() %>% 
    mutate( sum_z = x0 + x1)


# Always takers P( d = 1, z = 0)    
lottery %>% filter( d == 1, z == 0) %>% count() %>% pull(n)/lottery %>% filter(  z == 0) %>% count() %>% pull(n)
# Never P(d = 0, z = 1)
lottery %>% filter( d == 0, z == 1) %>% count() %>% pull(n)/lottery %>% filter(  z == 1) %>% count() %>% pull(n)
# Compliers
1-0.41-0.069

# P(z) depend on year and grades
lottery %>% lm( z ~ I(factor(year)) + I(factor(lotcateg)) , data = .) %>% summary()

# Compliers therefor: approx 50%
model <- lottery %>% lm( d ~ z +  I(factor(year)) + I(factor(lotcateg) ) + I( factor(year_lotcat)) , data = .) 

model$8/[2]
# numbers
nrow(lottery)*0.5013


## By gender
df <- tibble( female = c(0,1), data = list(lottery)) %>% 
    mutate( compliers_by_gender = map2(female, data , function(x, y) y %>% 
                                           filter(female == x ) %>% 
                                           lm( d~ z +  I(factor(year)) + I(factor(lotcateg) ) + I( factor(year_lotcat)) , data = .)
                                           ),
            compliers = map(compliers_by_gender, ~.x$coefficients[2] )
            ) %>% 
    unnest( compliers)


# by gender vs. for totalt 
df
model$coefficients[[2]]


# e) Is IV an estaimte of AAT?

# If one lottery = yes, but several -> people can try again. -> Alwaystakers 

# f) Potential outcome Y(0) and Y(1). Estimate Y condtional on w = 0,1 : E( Y(w)|X = x)

# i) E[Y1|c = c] = E[Y*D| z = 1]- E[Y*D| z = 0]/E[D| z = 1]- E[D| z = 0]

lottery_2 <- lottery %>% mutate( y_1 = lnw*d,
                                 y0 = lnw*(1-d),
                    en_d = (1-d)
                    ) 
# Simple soltion
lottery_2 %>% 
    summarise( y1 = (mean(y_1[z == 1]) - mean(y_1[z == 0]))/(mean(d[z == 1]) - mean(d[z == 0])),
               y0 = (mean(y0[z == 1]) - mean(y0[z == 0]))/(mean(en_d[z == 1]) - mean(en_d[z == 0]))
               )


# With controls
# Estimated y1
lottery_2 %>% 
    estimatr::iv_robust( formula = y_1 ~ + I(factor(year)) + I(factor(year_lotcat) ) + I(factor(lotcateg)) + d|z + I(factor(year)) + I(factor(year_lotcat) ) + I(factor(lotcateg)), data = .
                         )


# Estimated y0
lottery_2 %>% 
    estimatr::iv_robust( formula = y0 ~ + I(factor(year)) + I(factor(year_lotcat) ) + I(factor(lotcateg)) + en_d|z + I(factor(year)) + I(factor(year_lotcat) ) + I(factor(lotcateg)), data = .
    )

# The results: y1-y0 =  3.276-3.077


# f) 2 The distribution

# Never and always
ggplot(  ) +
    geom_density(data = lottery %>% filter( d == 0, z == 1), aes(x = lnw) ) +
    geom_density(data = lottery %>% filter( d == 1, z == 0), aes(x = lnw), linetype = 2 )
    

# Y1 and Y0
pa <- 187/456
pn <- 71/1020
pc <- 1-pa-pn

f1a <- lottery %>% filter( d == 1, z == 0) %>% pull(lnw)*(-pa/pc)
gc1 <- lottery %>% filter( d == 1, z == 1) %>% mutate( lnw = lnw*((pc+pa)/pc) ) %>% pull( lnw)

# Denne er ikke fulstending-
tibble( x= gc1) %>% ggplot( aes( x = x)) + geom_density()


ggplot( data = df_f1c,  aes( x =  f1c)) + geom_density()



# h) Instrument only exogenous within groups (year and category)

# Estimate year specific LATEs. Combine these in one estimate. Compare this estimate to LATE with controls

# Stratified LATEs B(x) = E[y| z = 1, X = x] - E[y| z = 0, X = x]/E[D| Z = 1, X =x] - E[D|Z =0, X = x] = E[EY1-Y0 |d1 - d0 = 1, X = x] 

# Is done by:

# 1) y = BD + ax + e
# 2) D = cZ + ax + u

# Dummies in both stages and x-specifiic first stage: B = E[B(x)f(x)]

# B(x) is the x-specific LATE, and f(x) ? 

df <- tibble( year = unique(lottery$year), data = list(lottery)) %>%
    expand_grid( lo = unique(lottery$lotcateg)) %>% 
    mutate( model = pmap( list( x = data, y = year, l = lo), function(x,y,l){ estimatr::iv_robust(data = x %>% filter(year == y, lotcateg == l), formula =  lnw ~ d|z) } ),
            coeff = map( model, function(x) x$coefficients[2] ),
            # First stage
            N        = pmap( list( x = data, y = year, l = lo ), function(x,y,l) {x %>% filter( year == y, lotcateg == l) %>% count() %>% pull(n)}),
            vz       = pmap( list( x = data, y = year, l = lo ), function(x,y,l) {x %>% filter( year == y, lotcateg == l) %>% summarise( varz = var(z)) %>% pull(varz) }),
            model2   = pmap( list( x = data, y = year, l = lo), function(x,y,l){ lm(data = x %>% filter(year == y, lotcateg == l), formula =  d ~ z) } ),
            fs = map( model2, function(x) x$coefficients[2] ),
            sum_z    = pmap( list(x = data, y = year, l = lo) , function(x,y,l)  x %>% filter( year == y, lotcateg == l) %>% summarise( sum_z = sum(z) ) %>% pull(sum_z) )   
            ) %>%
    select( -matches("data|model") ) %>% 
    unnest( cols = c( coeff, fs, sum_z, N,vz ))


## Result weigh 1
df %>%
    mutate( aw = fs*N/sum(fs*N) ) %>% 
    summarise( b = sum(coeff*aw), min = min(coeff), max = max(coeff), weigth = sum(fs*N) )
            
## Result weigh 2
df %>%
    mutate( aw = fs^2*vz/sum(fs^2*vz) ) %>% 
    summarise( b = sum(coeff*aw), min = min(coeff), max = max(coeff), weigth = sum(fs*N) )


# ALtern
df %>%
    mutate( aw = vz*fs^2/sum(vz*fs^2) ) %>% 
    summarise( b = sum(coeff*aw), min = min(coeff), max = max(coeff), weigth = sum(vz*fs^2) )


# -------------------------------------------------------------------------









































