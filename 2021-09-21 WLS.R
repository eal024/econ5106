

library(tidyverse)
library(wooldridge)
# Example Weighted Least Squares ------------------------------------------

# 1: OLS model, 2: the e_2 3:reg(log(e2)~ x) 4: model, weigh = 1/exp(log2$fitted) 

data( smoke, package = "wooldridge")

smoke <- as_tibble(smoke)

# Model 
olsreg <- smoke %>% lm( cigs ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, data = .) 

# The BP-test
library(lmtest)
summary(olsreg)$r.squared*nrow(olsreg$model)
bptest(olsreg)

# Estimate the variance function
smoke$log2 <- log((olsreg$residuals)^2)

var_reg <- smoke %>% lm( log2 ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, data = .)

w <- 1/exp(var_reg$fitted.values)

efgls <- smoke %>% lm( cigs ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, data = ., weights = w) 

summary(efgls)

# Example seminar
lalonde <- haven::read_dta("data/lalonde.dta")

lalonde_exp <- lalonde %>% filter( sample == 3)

# 1)
model1_ols <- lalonde_exp %>% 
    lm( earnings78 ~age  + married + black + hispanic + education + earnings75 + nodegree , data = .)


# 2)
lalonde_exp$log_e2 <- log( (model1_ols$residuals)^2)

var_reg_re78 <- lalonde_exp %>% lm( log_e2  ~ age  + married + black + hispanic + education + earnings75 + nodegree , data = .)

wre78 <- 1/(exp(var_reg_re78$fitted.values))^0.5

model1_efgl <- lalonde_exp %>%
    mutate( c = 1) %>% 
    lm( earnings78  ~ 0 + c +   age  + married + black + hispanic + education + earnings75 + nodegree, data = ., weights =  wre78)

summary(model1_efgl)


# The Labor Demand example ----------------------------------------------



# wage2 <- data.table::fread("data/data_verbeek/labour2.prn") %>% janitor::clean_names()
# 
# 
# # Model
# model2 <- wage2 %>%
#   mutate_all( ~log(.x)) %>%
#   rename_all( ~str_c("log_", .x)) %>%
#   lm( log_labour ~ ., data = .)
# 
# model2 %>% summary()