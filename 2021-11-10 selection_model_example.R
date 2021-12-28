
# Examples of Hackmans two steps model.

# 1)

# from: https://m-clark.github.io/models-by-example/heckman-selection.html#maximum-likelihood

library(tidyverse)

# Example womans wage by covariats

# Sim.data
set.seed(123456)
N      <- 10000
educ   <- sample(1:16, size  = N, replace =  T) 
age    <- sample(18:64, size = N, replace =  T) 


# 
covmat <- matrix( c(.46^2, .25*.46, .25*.46, 1), ncol = 2)
errors <- mvtnorm::rmvnorm(N, sigma = covmat)

z <- rnorm(N)
e <- errors[ , 1]
v <- errors[ , 2]


# Wage given by
wearn  <- 4.49 + 0.08*educ+ 0.012*age + e

# Missing variables
d_star <- -1.5 + .15*educ + 0.01*age + 0.15*z+v

obs_index <- d_star > 0

data <- tibble( wearn, educ, age, z , obs_index)

# Observe only those where Z > 0, index by obs_index.
data

# OLS all data
model_all <- data %>% lm( wearn ~educ + age, data = .)

# OLS on observed data
model_obs <- data %>% filter( obs_index == T) %>%  lm( wearn ~educ + age, data = .) 

stargazer::stargazer( list(model_all, model_obs), type = "text", column.labels = c("All data", "Observed"))


## The 2-step approach

# 1) Probit model (the selection I* (z in the example))

probit <- data %>% glm( obs_index ~ educ + age + z, data =. , family = binomial("probit") )

stargazer::stargazer(probit, type = "text")

# 
predict <- predict(probit )

mills0  <- dnorm(predict)/pnorm(predict)

# identical formulation: From symetri of norm 
# probit_lp = -predict(probit)
# imr = dnorm(probit_lp)/(1-pnorm(probit_lp))


summary(mills0)
imr <- mills0[obs_index]


qplot( imr, geom = "density")

## Step two: 
model_two_step <- 
    data %>% 
    filter( obs_index == TRUE) %>% 
    mutate( imr = imr) %>% 
    lm( wearn ~ educ + age + imr, data = .)



stargazer::stargazer( list(model_all, model_obs,
                           probit, 
                           model_two_step), type = "text", column.labels = c("All data", "Observed", "probit", "two-step")
                      )


# Packages sampleSelection
library(sampleSelection, quietly = T)

selec_model <- selection( data = data, obs_index ~ educ + age + z, wearn ~educ + age , method = "2step")

summary(selec_model)

# comparing:
# Calculated
coef(model_two_step)["imr"]/summary(model_two_step)$sigma

summary(imr)


# Maximum Likehood --------------------------------------------------------

select_ll <- function(  par, X, Z, y, observed_index){
    
    # init[1:4]: Coeff. from model first step
    gamma     = par[1:4]
    #
    lp_probit = Z %*%gamma
    
    # init[5:7] coeff. from model secound step
    beta      = par[5:7]
    lp_lm     = X%*%beta
    
}

# Observation data
X    <- model.matrix(model_two_step)
Z    <- model.matrix(probit)

init <- c( coef(probit) , coef(model_two_step)[-4], 1, 0 )

X %*% init[5:7]






# Example Two The Wooldridge data -------------------------------------------

# data
# mroz <- wooldridge::mroz
mroz <- haven::read_dta("http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta")

# Description:
mroz %>% select(hours, wage, nwifeinc, educ, exper, age, kidslt6) %>%  skimr::skim()

# Modeling female labor supply: 
# OLS
model_ols <- mroz %>% lm( data = ., formula = hours ~wage + nwifeinc + educ + exper + expersq )

# Hours worked from X
summary(model_ols)

# For some wage is not observed. Do not work

mroz %>% select( hours, nwifeinc, wage) %>% sample_n(20)



probit_mroz <- 
    mroz %>%
    mutate( obs =  ifelse(is.na(wage), 0, 1)) %>% 
    select(obs ,wage, nwifeinc , educ , exper , expersq , age , kidslt6 ) %>% 
    glm( obs ~ nwifeinc + educ + exper + expersq + age + kidslt6, family = binomial("probit"), data = .)


# Mills
mills <- dnorm( predict(probit_mroz) )/pnorm( predict(probit_mroz) ) 

mroz$mills <- mills

# Heckman two-step
model_two_step_hours <- mroz %>% lm( hours ~ wage + nwifeinc + educ + exper + expersq + mills , data = .)


stargazer::stargazer( list(model_ols,model_two_step_hours, probit_mroz), type = "text")














