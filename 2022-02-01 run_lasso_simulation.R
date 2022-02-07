

library(tidyverse)

## Simulate data for Lasso regression
# n: number of obs, K:number of predictors, nose_e = 0
sim_data <- function(n, k,noise_e) {

# Obs (n) and predictors (k)
n <- n
k <- k

# Number of variables c,e,u,y
kc <- round(k/4)
ke <- round(k/4)
ku <- round(k/4)
ky <- round(k/4)

# Standard deviation
sd.xc <- 1
sd.xe <- 1
sd.xy <- 1
sd.d   <- 1
sd.y   <- 1
sd.xu  <- 1
sd.beta  <- 0
sd.noise <- 0

# Coefficient
alpha <- 1
beta  <- 1


beta.e  <- noise_e ## input in function
beta.cd <- beta 
beta.ey <- beta
beta.y  <- beta 
beta.cy <- beta

# Number variables vector
k_vec <- c(kc = kc, ke = ke, ku = ku, ky =ky )    

# Matrices, row = obs, col = P (x)
xc <- rnorm(kc*n, 0, sd.xc ) %>% matrix( nrow = n)
xe <- rnorm(ke*n, 0, sd.xe ) %>% matrix( nrow = n)
xu <- rnorm(ku*n, 0, sd.xu ) %>% matrix( nrow = n)
xy <- rnorm(ky*n, 0, sd.xy ) %>% matrix( nrow = n)

# Variables

# Endogenous X (d): xc con-founder for y and d, xe: Effect y only through d 
d <- xe %*% rnorm(ke, beta.e, sd.beta) + xc %*% rnorm(kc, beta.cd, sd.beta) + rnorm(n,0,sd.d)

# Outcome (y) 
y <- alpha*d + xy %*% rnorm(ky, beta.y, sd.beta) + xc %*% rnorm(kc, beta.cy, sd.beta) + xe %*% rep(beta.ey, ke) + rnorm(n, 0, sd.y)

eps <- y*alpha*d

## X combined
X <- cbind(xc, xe, xu,xy)

## Return data
# sd.noise can be 0
noise <- rnorm( length(X), 0, sd.noise ) 
X     <- X+noise 

colnames(X) <- c( paste0("xc",1:kc), paste0("xe",1:ke), paste0("xu",1:ku), paste0("xy",1:ky))

## Return matrix
mat <- cbind(y,d,X)

colnames(mat)[1:2] <- c("y", "d") 

mat

}



## Default model

short_ols <- list( type = "short_ols")

mat <- sim_data(n = 1000, k = 4, noise_e = 0)

y  <- mat[,1]
d  <- mat[,2]
x  <- mat[,-c(1:2)]
dX <- mat[,-1]

library(gamlr)
library(hdm)

# Short OLS: Get the residuals
#       coef(lm.fit(y=y,x=cbind(1,d)))[2] # Alternative
coef(lm(y ~ 1 + cbind(d) ))[2]  # Look at the summary: summary(lm( y  ~1 + cbind(d)))

lm( y ~ x)

# RLasso
coef(hdm::rlasso(x = dX, y = y))

# Double selection

# Post Lasso





