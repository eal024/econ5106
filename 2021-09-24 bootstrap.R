

library(tidyverse)
library(data.table)


# Non parametric bootstrap ------------------------------------------------

# 1. r-the random sample with replacement
# 2. calculate B_hat_r
# 3. Repeat 1. and 2. R-times

# Beta_r* (bootstrap statistics) distribution approx to sampling distribution of Beta 

set.seed(123)

# Education 0-14. 
data <- tibble( 
    educ = 0 + 14*runif(n = 1000),
    xb   = (.1 + .1*educ),
    P    = 1/(1+exp(-xb) ),
    u    = runif( min = 0, max = 1, n = 1000),
    work = ifelse( P > u, 1, 0 ) 
)

# One year education:
1/(1 + exp(-.1-.1*1))

# data
as.data.table(data)[  , .N, by = work][]


# ML
# The model
glm( work ~ educ, data = data, family = binomial("logit")) %>% summary()

tbl <- tibble( index = 1:1000) %>% 
    mutate(  data = map( index, function(x,y) data[sample(nrow(data), size = nrow(data), replace = T),])) %>% 
    mutate(  b    = map(data,   ~glm( work ~ educ, data = .x, family = binomial("logit"))$coefficients["educ"]  )) %>% 
    unnest( b)

tbl %>% summarise( b = mean(b))

tbl %>% ggplot( aes( x = b ) ) + geom_histogram() + geom_vline( xintercept = .1, color = "red", linetype = 2)




