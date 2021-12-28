

# Lecture notes. Trunction and Censoring
library(tidyverse)


# Example
# labor supply. Desired annual hours worked y*

# w: wage
# obs
n   <- 1000
# error term
e   <- rnorm(n = n, mean = 0, sd = 1)
# log wage
lnw <- rnorm(n, mean = 2.75, sd = 0.6^2) 

# Y hours worked
y   <- -2.5 + 1*lnw + e # True y
yt  <- ifelse( y > 0, y, NA_real_) # Truncated
yc  <- ifelse( y > 0, y, 0) # censored

# Tibble
df <- tibble( y, yt, yc, lnw, e)

# 
df_long <- df %>% pivot_longer(-c(lnw,e) )  

df_long %>%  ggplot( aes( y = value, x = lnw, color = name, alpha = 0.1) ) +
    geom_point( ) + 
    geom_smooth( method = "lm", se = F) +
    facet_wrap(~name, ncol = 1)

df_long %>% ggplot( aes(y = value, x = lnw, fill = name, color = name)) + 
    geom_smooth( se = F, method = "lm")

# Regression results
m_latent <- lm( data = df, y ~lnw)
m_yc     <- lm( data = df, yc ~lnw)
m_yt     <- lm( data = df, yt ~lnw)

list(m_latent, m_yc, m_yt) %>% stargazer::stargazer( type = "text")

df_long %>% group_by(name) %>% summarise( mean_y = mean(value, na.rm = T))


# Estimating with ML  -----------------------------------------------------

## Estimating B from censored and truncated data
# https://m-clark.github.io/models-by-example/tobit.html#censoring-with-an-upper-limit

# Censored data
tobit_fun <- function(par, X, y, ul = -Inf, ll = Inf) {
    
    # Only lower or upper limits
    simga = exp( par[length(par)])
    beta  = par[-length(par)] 
    
    if(! is.infinite(ll)) {
        limit = ll
        indicator = y > ll
    } else { limit = ul
    indicator = y < ul}
    
    # Linear predictor
    lp = X %*% beta
    
    # Log like
    ll = sum( indicator*log( (1/sigma)*dnorm( (y-lp)/sigma)) )+ sum( (1-indicator)* log(pnorm(1-indicator)*log(pnorm(lp-limit)/sigma, lower =is.infinite(ll)))) 
    
    -ll
}

# Estimate via optimum
initmod <-  lm( data = df, y ~ lnw)
x       <-  model.matrix(initmod)
init    <-  c(coef(initmod), log_sigma = log(summary(initmod)$sigma) ) 

fit_tobit <- optim(
    
    par = init, 
    tobit_ll, 
    X = x,
    y = df$yc,
    ll = 0, 
    method = "BFGS",
    control = list(maxit = 2000, reltol = 1e-15)
)

fit_tobit

# OLS
df %>% lm( y ~ lnw , data = .)

# Censored regression
library(survival)
AER::tobit( yc ~ lnw, data = df, left = 0, right = Inf)

# Truncated regression
truncreg::truncreg(yt~ lnw, data =df, direction = "left")

# Truncated with oLS
df %>% na.omit() %>% lm( y ~ lnw, data = .)

# Example Tobit, truncation and Heckman 2-step (selection model) -------------------------------------------------------------

# Example showing inbuilt pacakges for estimating differnet models

# 1) data
mroz <- wooldridge::mroz %>%
    select(hours, wage, nwifeinc, educ, exper, expersq, age, kidslt6, inlf) %>% 
    as_tibble()
    
# 2) Description data 
skim_fun <- function(x) {list(
    n = n(),
    m_ = mean(x, na.rm = T) ,
    sd_ = sd(x, na.rm = T)  ,
    min = min(x, na.rm = T) ,
    max = max(x,na.rm = T) 
    )
    }

# descriptive data executed
mroz %>% summarise( across( .cols = everything(), .fns = skim_fun )) %>%
    mutate( var = c("n", "mean", "sd", "min", "max")) %>% 
    pivot_longer(-var) %>%
    unnest(value) %>% 
    pivot_wider( names_from = var, values_from = value)


# 3) The estimation:
# Want to estimate the labor supply (female)

# a) OLS
library(sandwich)
# sqrt(diag(vcovHC(model, type = "HC1")))

ols <- mroz %>%
    na.omit() %>% 
    lm( data = .,hours ~ wage + nwifeinc + educ + exper + expersq)

# b) Tobit
library(survival)
tobit <- AER::tobit(hours ~ wage + nwifeinc + educ + exper + expersq,
                    left = 0,
                    right = Inf, 
                    data = mroz
                    ) 

# c) Truncreg
truncreg <- truncreg::truncreg(hours ~ wage + nwifeinc + educ + exper + expersq,
                   data = mroz,
                   direction = "left")

# d) 
# OLS truncation
mroz %>% na.omit( ) %>% lm( hours ~ wage + nwifeinc + educ + exper + expersq, data = .) %>% 
    summary()

# e) 
# Heckmans 2-steps
selmodel <- sampleSelection::heckit(
    inlf  ~ nwifeinc + educ + exper + expersq + age + kidslt6,
    hours ~ wage + nwifeinc + educ + exper + expersq,
                        data = mroz,
                        method = "2step")

# The results
summary(ols) # OLS
tobit_fun() # Censored
summary(tobit)# Tobit
length(tobit$linear.predictors)
summary(selmodel) # Heckman 2-step




















