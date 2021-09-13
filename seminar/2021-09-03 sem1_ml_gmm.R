

# 
library(tidyverse)
set.seed(123)

# Creating a dataset.
df <- tibble( 
              educ = 6 + 14*runif(n = 1000),
              xb   = (-2+0.2*educ),
              y    = 1/(1+exp( -xb ) ),
              u    = runif( min = 0, max = 1, n = 1000),
              work = ifelse( y > u, 1, 0 ) 
              )


# Manuel
df %>% summarise_all(mean)

# Summarize alternativ
df %>% skimr::skim()


# Implemeting the ML-function
df %>% glm( work ~ educ, data = ., family = binomial("logit") ) %>% summary()


# Function
ML <- function( B0, B1, x, y) {
    xb <- B0 + B1*x
    P  <- 1/(1+exp(-xb)) 
    y*log(P) + (1-y)*log(1-P) 
   
}

# Test: Where is max?
tibble( a = seq(from = -10, to = 2, by = 0.1), log_lik = map_dbl( seq(from = -10, to = 2, by = 0.1), function( i ) sum(ML(B0 = i, B1 = 0.2 )) )) %>% 
    filter( log_lik == max(log_lik))

tibble( a = seq(from = -.5, to = .5, by = 0.05), log_lik = map_dbl( seq(from = -.5, to = .5, by = 0.05), function( i ) sum(ML(B0 = -2, B1 = i )) )) %>% 
    filter( log_lik == max(log_lik))


# Alternative ML-function
ML2 <- function(b0, b1, x, y) {

    ML <- pmap_dbl( list(y = y, x = x), function(y,x){ y*log( 1/(1+exp(-b0-b1*x))) + (1-y)*log( exp(-b0-b1*x)/(1+exp(-b0-b1*x)) ) } )

    return( sum(ML))
    }


ML2(b0 = -2, b1 =  .2, x = df$educ, y = df$work)


# Alternative test
df_sim <- tibble( b0 = seq(from = -4, to = 4), by = 0.5) %>% 
    expand_grid(b1 = seq( from = -.2, to = .3, by = .1) ) %>% 
    mutate( x = list(df$educ),
            y = list(df$work)
            ) %>% 
    mutate( ML_calc = pmap( list( b0 = b0, b1 = b1, x = x, y = y), function(b0 = b0, b1  = b1, x = x, y = y){ ML2(b0 =b0, b1 = b1 , x = x, y = y)} ) ) %>% 
    select(b0, b1, ML_calc) %>% 
    unnest(ML_calc)

# Control
df_sim %>% filter( ML_calc == max(ML_calc))

#
df %>% glm( work  ~ educ, data= ., family = binomial("logit")) %>% summary()


# gmm ---------------------------------------------------------------------

library(gmm)

df_sim <- df %>% 
    mutate( c = 1) %>% 
    select(work , c,  educ) %>% 
    as.data.frame()
    
    
mm <- function(beta, df) {
    # Return n*q matrix
    
    y <- as.numeric(df_sim[,1])
    x <- data.matrix(df_sim[,2:ncol(df_sim)])
    
    # Moment condition
    m <- x*as.vector(y - x%*%beta)
    
    return(cbind(m))
}

set.seed(123)
gmm( mm, 
     df_sim,
     rnorm(2),
     wmatrix = "optimal",
     vcov = "MDS",
     optfct = "nlminb",
     control = list(eval.max = 10000)
) 


# Probit ------------------------------------------------------------------

tibble( x = rnorm(n = 1000, mean = 0, sd = 1)) %>% ggplot( aes(x = x)) + geom_density() + geom_histogram( aes(y=..density..), alpha = 0.3 )

# Probability Probit: 
pnorm(  q = (-2+0.2*20), 0, 1)

tibble( educ  = seq(from = 6, to = 20, by =1),
        y = map_dbl( seq(from = 6, to = 20, by =1) , ~pnorm(.x*0.2 -2))
        ) %>% ggplot( aes(x = educ, y = y)) + geom_point()

# Creating a dataset.
df2 <- tibble( 
    educ = 6 + 14*runif(n = 1000),
    xb   = (-2+0.2*educ),
    y_logit    = 1/(1+exp( -xb ) ),
    y_probit   = pnorm(xb),
    u    = runif( min = 0, max = 1, n = 1000),
    work_p = ifelse( y_probit > u, 1, 0 ) 
)


# By filter
df2
ML_pro <- function(b0, b1, x, y) {
    
    ML <- pmap_dbl( list(y = y, x = x), function(y,x){ 
        P <- pnorm(b0 + b1*x)
        y*log(P) + (1-y)*log(1-P ) } )
    
    return( sum(ML))
}

ML_pro(b0 = -2, b1 = 0.2, x =  df2$educ, df2$work_p)

df_test <- tibble( b0 = seq(from = -4, to = 4), by = 0.5) %>% 
    expand_grid(b1 = seq( from = -.2, to = .3, by = .1) ) %>% 
    mutate( x = list(df2$educ),
            y = list(df2$work_p)
    ) %>% 
    mutate( ML_calc = pmap( list( b0 = b0, b1 = b1, x = x, y = y), function(b0 = b0, b1  = b1, x = x, y = y){ ML_pro(b0 =b0, b1 = b1 , x = x, y = y)} ) ) %>% 
    select(b0, b1, ML_calc) %>% 
    unnest(ML_calc)

df_test %>% filter( as.integer(ML_calc) == -457)


# Viz. of ML
df_test %>% filter( b0 == -2) %>%  ggplot(aes( x = b1, y = ML_calc)) + geom_line()
df_test %>% filter( b1 == .2) %>%  ggplot(aes( x = b0, y = ML_calc)) + geom_line() 



# Hypt. testing -----------------------------------------------------------

library(lmtest)

model_logit <- glm( data = df, work ~ educ, family = binomial("logit") )
model_logit_r <- glm( data = df, work ~ 1, family = binomial("logit") )

# Wald test.
summary(model_logit) 
(0.23094-0)*(0.01907^2)^-1*(0.23094-0)
waldtest(model_logit, model_logit_r)

# Probit
model_probit <- glm( data = df2, work_p ~ educ, family = binomial("probit") )
model_probit_r <- glm( data = df2, work_p ~ 1, family = binomial("probit") )
summary(model_probit) 
(0.228-0)*(0.01446^2)^-1*(0.228-0)
waldtest(model_probit, model_probit_r)

# LR: Check the difference in the loglikehood estimat (from restricted vs. unrestriced)
logLike_unrestriced <- ML2(b0 = -2, b1 =  .2, x = df$educ, y = df$work)

test <- tibble( b0 = seq(from = -100, to = 100, by = 0.5) ) %>% mutate( ML = map(b0, function(x) ML2(b = x, b1 = 0, x = df$educ, y = df$work) )) %>% 
    unnest( ML) %>% 
    filter( ML == max(ML))

logLike_restriced <- ML2(b0 = test$b0, b1 =  .0, x = df$educ, y = df$work)

# The model need to be "nested".  
2*(logLike_unrestriced - logLike_restriced)

lmtest::lrtest(model_logit, model_logit_r)


# LM:










