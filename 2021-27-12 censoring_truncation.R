

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
tobit_fun <-  function(par, X, y, ul = -Inf, ll = Inf) {
    
    # this function only takes a lower OR upper limit
    
    # parameters
    sigma = exp(par[length(par)]) 
    beta  = par[-length(par)]
    
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

# Estimate via optimum
initmod <-  lm( data = df, y ~ lnw)
x       <-  model.matrix(initmod)
init    <-  c(coef(initmod), log_sigma = log(summary(initmod)$sigma) ) 

fit_tobit <- optim(
    
    par = init, 
    tobit_fun, 
    X = x,
    y = df$yc,
    ll = 0, 
    method = "BFGS",
    control = list(maxit = 2000, reltol = 1e-15)
)


fit_tobit

AER::tobit(
    formula = yc ~lnw,
    left = 0,
    right =  Inf
)

censReg::censReg(data = df, formula = yc ~ lnw, left = 0, right = Inf)



# Truncation model --------------------------------------------------------

truncreg::truncreg( df = df, formula =  yt ~lnw) %>% summary()



#  Example: Modeling female labor supply:  ------------------------------

mroz <- read_rds("data/")

# ols
model_ols <- mroz %>% lm( data = ., formula = hours ~wage + nwifeinc + educ + exper + expersq )

# Hours worked from X
summary(model_ols)
model_ols$model %>% nrow()

# If wage = 0, data is censored
model_censored <- censReg::censReg( data = mroz %>% mutate( wage = ifelse( is.na(wage), 0, wage)  ),
                  hours ~wage + nwifeinc + educ + exper + expersq,
                  left = 0
                  )
#
summary(model_censored)



model_truncated <- truncreg::truncreg(data = mroz ,
                                      hours ~ wage + nwifeinc + educ + exper + expersq,
                                      direction = "left",
                                      point = 0
                                      )

summary(model_truncated)
nrow(model_truncated$gradientObs)


# Heckman

# Step1 
mroz_1 <- mroz %>% mutate( obs_index = ifelse( !is.na(wage), 1, 0))

probit <- mroz_1 %>% glm( obs_index ~ nwifeinc + educ + exper + expersq + age + kidslt6,
                          data = .,
                          family = binomial( link = "probit"))


summary(probit)

# Mills ratio
mills0 <- dnorm( predict(probit) )/pnorm( predict(probit) )

# distribtion Mills
ggplot2::qplot( mills0[mroz_1$obs_index == 1 ], geom = "histogram") + theme_light()


mroz_2 <- mroz_1 %>% mutate( imr = ifelse(obs_index == 1, mills0, 0))

# Heckman
selmodel <- lm( data = mroz_2,
    hours ~ wage + nwifeinc + educ + exper + expersq + imr 
    )

#
summary(selmodel)












