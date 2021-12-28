
library(tidyverse)
library(data.table)

# Truncation and Censoring.



## 1) Simulation

# Labor supply where desired annual hours worked y*, hourly wage:
    # y* = -2.5 + ln(wage) + E
    # E ~N(0,1) 
    # ln(wage) ~N(2.75, 0.6^2)
    ## Tobit model    

set.seed(125)
n <- 1000

df <- tibble(
    n = 1:n,
    e = rnorm(n = n, mean = 0, sd = 1),
    lnw = rnorm(n = n, mean = 2.75, sd = 0.6 ^ 2)
    ) %>% 
    mutate( y = -2.5 + 1*lnw + e,
            # Truncation: More data loss than censoring       
            yt = ifelse( y > 0, y, NA_real_),
            # Censoring: 
            yc = ifelse(y > 0 , y, 0)
            )

df2 <- df %>% select(yt, yc, lnw) %>% 
    pivot_longer( -lnw, "model", "value") 

# Graph 
df2 %>% 
    ggplot(aes( y = value, x = lnw, color = model, fill = model) ) +
    geom_point( alpha = 0.4,  shape = 3) +
    geom_smooth( method = "lm", se = F, alpha = 0.5) +
    geom_point( data = df, aes( y = y, x = lnw), alpha = 0.1, color = "black", inherit.aes = F) +
    facet_wrap( ~ model) +
    geom_smooth( data = df, aes( y = y, x = lnw), se = F,  inherit.aes = F, color = "black", linetype = 2)


## estimated resulat

model_true <- lm( data =df, y ~ lnw)
model_c    <- lm( data =df, yc ~ lnw)
model_t    <- lm( data =df, yt ~ lnw)

stargazer::stargazer( list(model_true, model_c, model_t), type = "text")


# Maximum Likehood  -------------------------------------------------------

initmod <- lm(y ~ lnw, data = df)
    
    #1 +lnw_i
X    <- model.matrix(initmod)
init <- c(coef(initmod), log_sigma = log(summary(initmod)$sigma)) 

tobit_ll <- function(par, X, y, ul = -Inf, ll = Inf) {
    
    sigma <- exp(par[length(par)])
    beta  <- par[-length(par)]
    
    # create indicator depending on chosen limit
    if (!is.infinite(ll)) {
        limit = ll
        indicator = y > ll
    } else {
        limit = ul
        indicator = y < ul
    }
    
    # linear predictor
    lp = X %*% beta
    
    # log likelihood
    ll = sum(indicator * log((1/sigma)*dnorm((y-lp)/sigma)) ) + 
        sum((1-indicator) * log(pnorm((lp-limit)/sigma, lower = is.infinite(ll))))
    
    -ll
}

# Optim. the function
fit_tobit <-
    optim(
        par = init,
        tobit_ll,
        y = df$yc,
        X = X,
        ll = 0,
        method = "BFGS",
        control = list(maxit = 2000, reltol = 1e-15)
    )


library(survival)

tobit_model <- AER::tobit(
    yc ~ lnw ,
    data = df,
    left = 0,
    right = Inf
) 


# Comparing ML and tobit_modellnig
fit_tobit
summary(tobit_model)


