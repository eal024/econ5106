
# Seminar 8: Selection models

# Data and packages

library(tidyverse)

# mus18 <- haven::read_dta("http://fmwww.bc.edu/ec-p/data/mus/mus18data.dta")
mus18 <- readRDS("data/mus18.rds")

# Data structure and Demand for Medical Care
# Study year 2 and everybody who is 18 years and older
mus18_1 <- mus18 %>% filter( year == 2 , age >= 18) %>% 
    mutate( site2 = ifelse(site == 2, 1, 0),
            site3 = ifelse(site == 3, 1, 0),
            site4 = ifelse(site == 4, 1, 0),
            site5 = ifelse(site == 5, 1, 0),
            site6 = ifelse(site == 6, 1, 0)
    )

# a) Medical expenditures on X

model <- paste0("med ~ ", paste0( c("coins", "income", "age", "female", "educdec", "num", str_c("site",2:6)), collapse =  " + "  ))

ols <- mus18_1 %>%
       lm( data    = ., 
           formula = med ~  coins  + income + age + female + educdec + num +  site2 + site3 + site4+ site5 + site6
           )

summary(ols)
sigma(ols)

var      <- 12
N        <- nrow(mus18_1)
y_yhat_2 <- sum( (mus18_1$med-fitted(ols))^2 ) 

# Sigma
sqrt(y_yhat_2/(N-var))

a$sigma

# coins: %-health cost borne by the insured, paid by the patient
# -0.73 delta 1% borne by the insured, decreases medical expenditures with 0,7 

# b) 20% has med = 0.
# Estimate censored regression
library(survival)


tobit <- AER::tobit(
                data    = mus18_1 %>% select(med ,coins, income , age ,female, educdec, num ,site , site2,site3, site4, site5, site6), 
                formula = med ~  coins  + income + age + female + educdec + num +  site2 + site3 + site4+ site5 + site6,
                left = 0,
                right = Inf
)

summary(tobit)

length(tobit$y)
nrow(mus18_1)

# Sigma Tobit
#sigma_tobit <- sqrt(sum((mus18_1$med - fitted(tobit))^2)/(N-10))
sigma_tobit <- exp(7.04)
# What is P(y > 0 | coins = 0)

newdata <- mus18_1 %>% select(med, age ,female, num, income, educdec, site2:site6, coins) %>%
    summarise_all(mean)

# # P( | coins = 0) : Steps:
# pred_coins_0 <- predict(tobit, newdata = data_new_mus18)
# pnorm(pred_coins_0/sigma_tobit) 

# Propability
tibble_predict <- tibble( data = list(newdata,newdata), coins = c(0,100)) %>%
    mutate( data    = map2(data, coins, function(x,y) x %>% mutate(coins = y)),
            predict = map(data,         function(x) predict(tobit, newdata = x)),
            prob    = map(predict,      function(x) pnorm(x/sigma_tobit))
            ) %>%
    #select( coins, prob) %>% 
    unnest( prob)

tibble_predict

# c) The average partial effect, increasing from 0 to 100 -----------------

library(censReg) 

tobit_censReg <- censReg(med ~  coins  + income + age + female + educdec + num +  site2 + site3 + site4+ site5 + site6,
                         data = mus18_1,
                         left = 0,
                         right = Inf)

summary(tobit_censReg)

predict_censReg <- function( data, censRregModel , output = "yhat"){
    
    data <- data %>%  mutate( `(Intercept)` = 1 ) %>% pivot_longer( names_to = "var", values_to = "mean", everything() )
    
    
    df <- tibble( var = names(tobit_censReg$estimate), coef = tobit_censReg$estimate) %>% left_join( data, by = "var") %>%
        filter( var != "logSigma") %>%
        mutate( pred = mean*coef)
    
    if(output == "yhat"){ df %>% filter( var != "logSigma") %>% summarise( yhat = sum(pred)) } else{ df}

}

predict_censReg( data = newdata %>% mutate(coins = 0) , censRregModel = tobit_censReg, output = "yhat")

pnorm(104/exp(7.04))
pnorm(-120/exp(7.04))

# c)  ---------------------------------------------------------------------

# Calculate the average partial effect of increasing coins from 0 to 100

xb_mean <- predict_censReg( newdata, tobit_censReg)

100*tobit_censReg$estimate[names(tobit_censReg$estimate) == "coins" ]*pnorm(xb_mean$yhat/exp(7.04))



# d) ----------------------------------------------------------------------

# Estimate a truncated regression (ie only using positive expenditures) using maximum likelihood.

ols_truncted <- mus18_1 %>%  filter( med > 0) %>%  lm( formula =  model)

summary(ols_truncted)    

truncreg::truncreg(data = mus18_1 %>%   filter( med > 0) ,
                   formula = med ~  coins  + income + age + female + educdec + num +  site2 + site3 + site4+ site5 + site6, 
                   direction = "left",
                   point = 0)


# e) Hackman two-steps model ---------------------------------------------

library(sampleSelection)

sampleSelection::selection(
    data =  mus18_1,
    coins  ~ income + age + female + educdec + num +  site2 + site3 + site4+ site5 + site6,
    med    ~  coins  + income + age + female + educdec + num +  site2 + site3 + site4+ site5 + site6
    method = "2step"
)

selection_2step = selection(observed_index ~ educ + age + z, wearnl ~ educ + age, 
                            method = '2step')







