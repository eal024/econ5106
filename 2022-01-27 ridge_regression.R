

library(tidyverse)
library(glmnet)

# Data baseball
hitters <- ISLR::Hitters %>% janitor::clean_names() %>%  na.omit()

dim(hitters)

x <- model.matrix( salary ~., data = as.data.frame(hitters) )[,-1]
y <- hitters$salary


# Ridge regression.
2^c(3,2,1)

# Select different lamda ( /) ) in grid.
grid <- 10^seq(10, -2, length = 100)

# ridge when alpha = 0
# The glmnet standardizing the variables to the same scale
ridge_mod <- glmnet( x, y, alpha = 0, lambda = grid )

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


## 2) Fit the ridge regression model on training set

# Alpha = 0 -> Ridge regression.
ridge_mod <- glmnet( x[train,], y[train], alpha = 0, lambda = grid, thresh =  1e-12)

# Evaluate MSE, using Lamda = 4
ridge_pred <- predict( ridge_mod, s = 4, newx = x[test,])

# MSE: Lamda = 4
mean( (ridge_pred-y_test)^2 )

# MSE of sample_mean - fit the model with just the intercept
mean( (mean(y[train]) - y_test)^2)

# MSE lower with Lambda = 4, than just the intercept.

# This can also be done with large value of lamda 
ridge_pred_exm <- predict( ridge_mod, s = 1e10, newx = x[test,])

mean( (ridge_pred_exm-y_test)^2 )

# MSE lower with Lamda 4 than large. MSE < MSE (just the intercept)

# Ridge better than OLS?
ridge_mod <- glmnet( x[train,], y[train], alpha = 0, lambda = grid, thresh =  1e-12 )
ridge_pred_ols <- predict( ridge_mod, s = 0, newx = x[test,] )

ridge_pred_ols

mean( (ridge_pred_ols-y_test)^2 )

# OLS from LM -> Missing input = excat in glmnet
(lm( y ~x, subset = train) %>% residuals() )^2 %>% mean()


# Fin the best Model CV, by lamda -----------------------------------------

# In general use CV to find the Lamda (tuning parameter)

set.seed(1)
cv_out <- cv.glmnet( x[train,], y[train], alpha = 0)

cv_out %>% str()

plot(cv_out)

best_lamda <- cv_out$lambda.min

modl_fit_best <- predict(  ridge_mod, s = best_lamda, newx = x[test,])

# min
mean( (modl_fit_best-hitters$salary[test])^2)

# Vs. Lamda 4, which is higer
mean( (ridge_pred-hitters$salary[test])^2)


# Final, prediction using the full dataset, Lamda equal min, and look at the variables
ridge_fit_full_data <- glmnet( x, y, alpha = 0)

# Look at the coeff, with best Lamda. Non B is 0.
predict(ridge_fit_full_data, s = best_lamda, type = "coefficients")[1:20,]



# 1) Lasso -------------------------------------------------------------------

lasso_mod <- glmnet( x[train,], y[train], alpha = 1, lambda = grid)

# Plot coeff. To |B_lambda|/|B_ols|
plot( lasso_mod)

# Best Lamda with CV, 10-fold
set.seed(1)
best_lan <- cv.glmnet(x[train,], y[train], alpha = 1)

plot(best_lan)
best_lan$lambda.min

lass_pred <- predict(lasso_mod, s = best_lan$lambda.min, newx = x[test,])

# Lower than Null model and OLS. Near Ridge
mean( (lass_pred-hitters$salary[test])^2)

# The advanteage: The resulting coeffisients estimates are sparse: easy to interpret. Exs. Ridge -> Lasso, 12 of 19 are 0 in lasso.


















