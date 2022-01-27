

## Machine Learning

# Model Y = a + bx1 + .... + b_p*x_p + e

# 
library(tidyverse)
library(broom)

credit <- ISLR::Credit %>%  as_tibble() %>% janitor::clean_names()

## 1) Best Subset Selection

# Algorithm
 # 1. M0, null model
 # 2.   a) fit (p k) model contain p = k predictors
 #      b) The best for each = Mk-k model: the larges R2 
# 3. Select single model among M0,..Mp, CV-> AIC,BIC or R2.adj

# Basic example
df1 <- credit %>%  select( balance, age, cards )

# Number of models: 2^p = eks. p =2 2^2 = 4

#credit %>% lm( "balance ~cards" , data = .) %>% broom::glance()

# Set tibble with models
trb1 <- tibble( models = c("balance  ~ 1",
                           "balance ~ age",
                           "balance ~ cards",
                           "balance ~ age + cards"), 
        data = list(df1),
        index = 1:4
        )


trb2 <-  trb1 %>% 
    # Step 1: Calculate R2
    mutate( r2 = map2(data, models, function(x,y) { 
        lm( formula = y, data = x ) %>% glance() %>% select( r.squared)} ) 
        ) %>% 
    unnest(r2) 

trb2

# Step 2: Select model with k variables, that has the best R2
trb3 <-  trb2 %>% 
    # use the data on training set, choose model with best r.2-adjustes
    filter( index %in% c(3,4)) %>% 
    mutate( r2.adj = map2(data, models, function(x,y) { 
        lm( formula = y, data = x ) %>% glance() %>% select( adj.r.squared)} )
    ) %>% 
    unnest( r2.adj)

# Model to select:
trb3 %>% filter( adj.r.squared == max(adj.r.squared))


# 2) Stepwise Selection ---------------------------------------------------
# Computation limts and course of dim. that can lead to overfitting -> stepwise selection 

# Forward Stepwise Selection







# Choosing the optimal model ----------------------------------------------

# First some basic

# The example model
model_ex2 <- credit %>%  lm( balance ~ income + limit + rating + cards + age + student, data = .)


# RSS = 3 821 620

# Get it from:
deviance(model_ex2)
anova(model_ex2)

# Or
sum( (credit$balance- fitted(model_ex2))^2 )

# Residual sum of error
RSS <- deviance(model_ex2)

residual_variance <- RSS/( nrow(credit) - ncol(model_ex2$model) )

RSE <- residual_variance^0.5

# See Residual standard error:
summary(model_ex2)


## Three measures for choosing the optimal model:

# C_p
# AIC
# R2-adjusted

n      <- nrow(credit)                                 # Observations
d      <- model_ex2$model %>% ncol( ) -1               # Number of predictors
RSS    <- sum( (credit$balance- fitted(model_ex2))^2 ) # Residuals sum of squared 
TSS    <- sum((fitted(model_ex2)-mean(credit$balance))^2 ) 


# Sigma seems to be wrong
sigma2  <- RSS/( nrow(credit) - ncol(model_ex2$model) )
sigma2  <- summary(model_ex2)$sigma^2
sigma2  <- sum(resid(model_ex2)^2)


glance(model_ex2)
loglike <- logLik(model_ex2)[[1]]

# C_p
(1/n)*(RSS + 2*d*sigma2) 


# AIC
AIC(model_ex2)
# AIC = 2K – 2ln(L)

# (1/(n*sigma2))*(RSS + 2*d*sigma2)
2*6-2*loglike
(1/(n*sigma))*(RSS + 2*d*sigma)

n + n*log(2*pi) + n * log(sum(resid(model_ex2) ^2) / n) + 2 * (d+1)



# BIC
BIC(model_ex2)

#
(1/n*sigma2)*(RSS + log(n)*d*sigma2)  # Seem to be wrong

# R2
1- RSS/TSS

# R2-adj
1- (RSS/(n-d-1))/(TSS/(n-1))














    
