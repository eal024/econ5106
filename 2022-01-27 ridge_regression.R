

library(tidyverse)
library(glmnet)

# Data baseball
hitters <- ISLR::Hitters %>% janitor::clean_names() %>%  na.omit()

x <- model.matrix( salary ~., data = as.data.frame(hitters) )[,-1]
y <- hitters$salary


# Ridge regression.
2^c(3,2,1)

# Select different lamda ( /) ) in grid.
grid <- 10^seq(10, -2, length = 100)

# ridge when alpha = 0
# The glmnet standardizing the variables to the same scale
ridge_mod <- glmnet( x, y, alpha = 0, lambda = grid)

# 263 obs, 20 varaibels
dim(hitters)

# For each Lamda, there is a set of coeffs.   .
length( grid)

# There should be 100 different sets (column), 20 variables (rows) each:
dim( coef(ridge_mod) )

# Coeff. should be smaller for large value of lamda

# Big Lamda
grid[10]
ridge_mod$lambda[10]
# Samller
ridge_mod$lambda[50 ]

# As expeted
coef(ridge_mod)[,10]
coef(ridge_mod)[,50]



# Cross validation --------------------------------------------------------

set.seed(1)

train <- sample( x = 1:nrow(x), size = nrow(x)/2 )
test  <- (-train)

#
c(1:10)[-c(1:2)]
c(1:10)[c(1:2)]
y_test <- y[test]


## 2) Fit the ridge regressio model on training set

# git
ridge_mod <- glmnet( x[train,], y[train], alpha = 0, lambda = grid, thresh =  1e-12)

# Evaluate MSE, using Lamda = 4
ridge_pred <- predict( ridge_mod, s = 4, newx = x[test,])

# MSE:
mean( (ridge_pred-y_test)^2 )

# MSE of sample_mean
mean( (mean(y[train]) - y_test)^2)

# This can also be done with large value of lamda 
ridge_pred_exm <- predict( ridge_mod, s = 1e10, newx = x[test,])

mean( (ridge_pred_exm-y_test)^2 )

# MSE lower with Lamda 4 than large. MSE < MSE (just the intercept)

# Ridge better than OLS?
ridge_pred_ols <- predict.glmnet( ridge_mod, s = 0, newx = x[test,])

ridge_pred_ols

mean( (ridge_pred_ols-y_test)^2 )

# OLS from LM
(lm( y ~x, subset = train) %>% residuals())^2 %>% mean()


# Fin the best Model CV, by lamda -----------------------------------------















































