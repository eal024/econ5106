

# Seminar IV/LATE
library(tidyverse)

lottery <- haven::read_dta("data/lottery.dta")

# You plan to estimate the return to attending medical school (d) on earnings in 2007
# (lnw) using instrumental variables using the lottery outcome (z) as your instrument.

tibble( n = names(lottery), label = map_chr(lottery, function(x) if( is.null(attr(x, "label"))){ return("NA")} else{ attr(x, "label")} )   )



# a) Instrument exogeneity: 2 assumption need to fulfill 1) random assignment 2) Exclusion restriction: Any effect from z on y is through d 


# b) Relevance (first stage)
lottery %>% lm( d ~ z + female + year, data = .) %>% summary() # Relevant
lottery %>% lm( d ~ z , data = .) %>% summary() # Relevant

# Equal 
mean(lottery$d[lottery$z == 1]) - mean(lottery$d[lottery$z == 0])


# c) IV
iv_model1 <- estimatr::iv_robust( data = lottery, lnw ~ d|z , se_type = "HC1")
iv_model2 <- estimatr::iv_robust( data = lottery %>% mutate(
    lot_year = case_when( (year == 1989 & lotcateg == 4)  ~ 1,
                          (year == 1989 & lotcateg == 5)  ~ 1,
                          T~ 6
                          ), lotcateg = factor(lotcateg),year = factor(year) ),
    lnw ~   lotcateg + lot_year + year +  d|z+  lotcateg + year + lot_year, se_type = "HC1")
#
summary(iv_model1)
summary(iv_model2)


# Alternativ model
lm( data = lottery, lnw ~ d + female)


# d) Find compliers, never takers and allways takers
df_id <- lottery %>% 
    group_by(z,d) %>% 
    count() %>% 
    pivot_wider( names_from = d, values_from = n ) %>%  
    ungroup() %>% 
    add_row( 
        summarise(., 
                  z = 100,
                  across(where(is.numeric), sum ))) %>% 
    mutate( sum = `0` +  `1`)


# Allways
df_id$`1`[df_id$z == 0]/df_id$sum[df_id$z == 0]
# Never
df_id$`0`[df_id$z == 1]/df_id$sum[df_id$z == 1]
# Compliers
(mean(lottery$d[lottery$z == 1]) - mean(lottery$d[lottery$z == 0]))*1476

## Compliers depend on yaer ad lot lot_cat_eg
lottery %>%
    mutate( lot_year = case_when( (year == 1989 & lotcateg == 4)  ~ 1,
                                  (year == 1989 & lotcateg == 5)  ~ 1,
                                  T~ 6
    )) %>%
    lm( d ~ lot_year + I(factor(year)) + I(factor(lotcateg)) + z , data = .)%>% summary()

# 0.5 for all.



## By gender
## Conditional distribution of gender
lottery %>% filter(female == 1 ) %>% lm( d ~ z + I(factor(year)) + I(factor(lotcateg)), data = .)%>% summary()

0.485-0.501




# e) Is LATE == ATT?

# If only those assigned to treatment can take treatment, (or control), then the LATE is ATT
# E[ d =1 | z = 0] = 0
# If there was only 1 lottery

y1_compliers <- tibble( navn = c("dz_1", "dz_0"),
        teller = c(lottery %>% mutate( yd = lnw*d) %>% filter( z == 1) %>% summarise( yd_z1 = mean(yd)) %>% pull(yd_z1),
                    lottery %>% mutate( yd = lnw*d) %>% filter( z == 0) %>% summarise( yd_z0 = mean(yd)) %>% pull(yd_z0)
                    ),
        nevner = c( mean(lottery$d[lottery$z == 1]), mean(lottery$d[lottery$z == 0]) )
        )


y0_compliers <- tibble( navn = c("dz_1", "dz_0"),
                        teller = c(lottery %>% mutate( y_1_d = lnw*(1-d) ) %>% filter( z == 1) %>% summarise( y_1_d = mean(y_1_d)) %>% pull(y_1_d),
                                   lottery %>% mutate( y_1_d = lnw*(1-d) ) %>% filter( z == 0) %>% summarise( y_1_d = mean(y_1_d)) %>% pull(y_1_d)
                        ),
                        nevner = c( lottery %>% mutate( `1_d` = 1-d ) %>% filter( z == 1) %>% summarise( `1_d` = mean( `1_d`)) %>% pull(`1_d`),
                                    lottery %>% mutate( `1_d` = 1-d ) %>% filter( z == 0) %>% summarise( `1_d` = mean( `1_d`)) %>% pull(`1_d`)
                                        )
)
                        
y0_compliers

# Condtional mean
(0.209-1.81)/(0.069-0.59)

## Marginal distribution.

# Allways
p_a <- df_id$`1`[df_id$z == 0]/df_id$sum[df_id$z == 0]
# Never
p_n <- df_id$`0`[df_id$z == 1]/df_id$sum[df_id$z == 1]
# Compliers
p_c <- mean(lottery$d[lottery$z == 1]) - mean(lottery$d[lottery$z == 0])


f_00_p_j <- lottery %>% filter( d == 0, z == 0) %>% mutate( lnw = lnw*(p_c+p_n)/p_c  ) %>% pull( lnw)
f_10_p_j <- lottery %>% filter( d == 1, z == 0) %>% mutate( lnw = -lnw*(p_n)/p_c ) %>% pull(lnw)



f_11_p_j <- lottery %>% filter( d == 1, z == 1) %>% mutate( lnw = lnw*((p_c+p_a)/p_c) ) %>% pull( lnw)
f_01_p_j <- lottery %>% filter( d == 0, z == 1) %>% mutate( lnw = -lnw*(p_a)/p_c ) %>% pull(lnw)


ggplot( ) +
    geom_density( data = tibble( a = rowSums( expand.grid(f_00_p_j,f_10_p_j) ) ),aes( x = a),  linetype = 1 , color = "black") +
    annotate( x = 3, y = 1, geom = "text", label = "gc0", color = "black" ) +
    geom_density(data = tibble( a = rowSums( expand.grid(f_11_p_j ,f_01_p_j ) ) ), aes(x = a), linetype = 2 , color = "red") +
    annotate( x = 4, y = 0.5, geom = "text", label = "gc1", color = "red" ) +
    geom_density(data = lottery %>% filter( d == 1, z == 0) , aes(x = lnw), linetype  = 2  ) +
    annotate( x = 4, y = .1, geom = "text", label = "Alwaystakers" ) +
    geom_density(data = lottery %>% filter( d == 0, z == 1) , aes(x = lnw), linetype  = 4  , color = "red") +
    annotate( x = 3, y = 1, geom = "text", label = "Never", color = "red" ) 


# f) Regression

# y1 
lottery %>% mutate( y1 = lnw*d ) %>% 
    estimatr::iv_robust( y1 ~ d | z, data = .) %>% summary()

# y0
lottery %>% mutate( y1 = lnw*(1-d),
                    md = (1-d)) %>% 
    estimatr::iv_robust( y1 ~ md | z, data = .) %>% summary()

# Result
y1_compliers
(2.98-1.28)/(0.93-0.41)
y0_compliers

# or from the regression
3.26-3.077

# g) What can you say about Marginal distribution for Never and allways takers
ggplot(  ) +
    geom_density(data = lottery %>% filter( d == 1, z == 0) , aes(x = lnw), linetype  = 2  ) +
    annotate( x = 4, y = .1, geom = "text", label = "Alwaystakers" ) +
    geom_density(data = lottery %>% filter( d == 0, z == 1) , aes(x = lnw), linetype  = 4  , color = "red") +
    annotate( x = 3, y = 1, geom = "text", label = "Never", color = "red" ) 



lottery %>% filter( d ==1 , z ==0) %>% summarise( n = n(), mean = mean(lnw), sd = sd(lnw))

# h) Stratified LATEs
df <- lottery %>%
    mutate( lotcatYear =  str_c(year, "_", lotcateg)%>% as.factor() 
    )

df %>% distinct(lotcatYear)

estimatr::iv_robust( lnw ~ lotcatYear + d | z + lotcatYear ,  data = df) %>% summary()



# Exercise 2 --------------------------------------------------------------

# We are interested in how health insurance affects out-of-pocket expenditure on
# drugs, and have access to an extract from the Medical Expenditure Panel Survey of
# individuals over the age of 65 years.

# data: 
mus06 <- haven::read_dta("data/mus06data.dta")

names(mus06)
tibble( n = names( mus06), label = map_chr( mus06, function(x) attr(x, "label")) ) %>% head(20)
# log expenditure ~ prescricpted medical drugs. hi_empunion: 1 (if individ has supl. health insurance)
# ldrugeexp = a + b_1 hi_empunion + XB + u

# Equation
mus06 %>%
    lm( ldrugexp ~ hi_empunion + age + female + linc + totchr + blhisp , data = .) %>%
    stargazer::stargazer( type = "text")

# a) 
# Interpretation: (log~linear model): hi_empunion: per cent increase, from 1 delta x.
# Supplm. health inscu. lead to 7.4 per cent increase in expenditure on pre scr. drogs

# Why worry: Those who have insurance are different from those who dont have. And differenct in ways we cant
#            observe/control for. The estimate is therefor most likely suffering for OVB.
#            the estimate of B is bias.


# b)
# multlc: Large firm operator and multiple location -> good or bad instrument
    # Randomized            : Not randomized
    # Exclusion restriction : Effect of empl. at Large firm on y (expend.) only through insurance -> will like to think no () 
    # Weak                  : suplm. insurance correlated with large firm/multi location firm


mus06 %>% 
    lm(hi_empunion ~ multlc + age + female + linc + totchr + blhisp, data = .) %>% 
    stargazer::stargazer( type = "text")


# c) 
# Indirect least squared 

# Structural bias equation.
mus06 %>%
    lm( ldrugexp ~ hi_empunion + age + female + linc + totchr + blhisp , data = .) 

# Reduce form equations
# (1)
lm(data = mus06, formula = hi_empunion ~ multlc + age + female + linc + totchr + blhisp) 

# (2)
lm(data = mus06, formula = ldrugexp    ~ multlc + age + female + linc + totchr + blhisp) 

# Indirect
(-0.200219)/0.14876


lm(data = mus06, ldrugexp ~ hi_empunion + age + female + linc + totchr + blhisp ) %>%stargazer::stargazer( type = "text")

mus06 %>% lm( ldrugexp ~hi_empunion , data = .)

estimatr::iv_robust(formula = ldrugexp ~  hi_empunion|multlc , data = mus06)

# Estimate
cov( mus06$ldrugexp, mus06$multlc)/cov( mus06$hi_empunion, mus06$multlc)
# health insurance lead to 15 % increase 

# Wald
(mean(mus06$ldrugexp[mus06$multlc == 1])- mean(mus06$ldrugexp[mus06$multlc == 0]))/(mean(mus06$hi_empunion[mus06$multlc == 1])-mean(mus06$hi_empunion[mus06$multlc == 0]))


## c) share of compliers that is female:
mus06 %>% summarise( c = mean(hi_empunion[multlc == 1]) - mean(hi_empunion[multlc == 0]) )

mus06 %>%
    group_by( female) %>% 
    summarise( c = mean(hi_empunion[multlc == 1]) - mean(hi_empunion[multlc == 0]) )




mus06 %>% 
    group_by( z = multlc, d = hi_empunion) %>% 
    count() %>% 
    pivot_wider( names_from = d, values_from = n) %>% 
    mutate( sum_z = `0`+ `1` ) 


# a
mus06 %>% filter( multlc == 0, hi_empunion == 1) %>% count()
mus06 %>% filter( multlc == 0, hi_empunion == 1, female == 1) %>% count()
# n
mus06 %>% filter( multlc == 1, hi_empunion == 0) %>% count()


#Over all population:
mus06 %>% group_by(female) %>% count() %>% ungroup() %>% mutate( andel = n/sum(n))

## e) Using the moments:
cov(mus06$ldrugexp,mus06$multlc)/cov(mus06$hi_empunion,mus06$multlc) 

## f) share A, N and C male/female
tibble( female= c(0,1) ) %>% 
    mutate( t = map(female, function(x) mus06 %>% 
                        filter( female == x) %>% 
                        group_by( z = multlc, d = hi_empunion) %>% 
                        count() %>% 
                        pivot_wider( names_from = d, values_from = n) %>% 
                        mutate( sum_z = `0`+ `1` ) 
                        )
            ) %>% 
    unnest( t)

#mus06 %>% filter( female == 1) %>% group_by( d = hi_empunion, z = multlc) %>% count()



## Mean
mus06 %>% 
    group_by( z = multlc, d = hi_empunion) %>% 
    summarise( mean_y = mean(ldrugexp)) %>% 
    pivot_wider( names_from = d, values_from = mean_y) %>% 
    rowwise() 



# # # Description of the data
# mus06 %>%
#     select( !matches("_est") ) %>%
#     pivot_longer(everything()) %>%
#     group_by( name) %>%
#     summarise(  mean = mean(value),
#                 min  = min(value),
#                 max  = max(value),
#                 sd   = sd(value)
#     ) %>% left_join()
# 
