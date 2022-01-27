
# 
library(tidyverse)
library(boot)


set.seed(1)
auto <- ISLR::Auto %>% janitor::clean_names() %>% as_tibble() %>% mutate( index = row_number() )


# Selecting 196 of 392 as training. Spliting set into two
train <- sample(392, 196)


# Step 1
# fit model, training set
lm_fit <-  auto %>% lm( mpg ~ horsepower, data = . , subset = train )

# equal to
auto %>% filter( index %in% train) %>% lm( mpg ~ horsepower, data = . ) 

# Step 2
# predict - for for all 392
# MSE calcualted for 192 observation
mean( (auto$mpg - predict(lm_fit, auto) )[-train]^2 )

# Alternativ: # 1) predict for all 392, 
tibble( predicted =  predict(lm_fit, newdata = auto), mpg = auto$mpg , index = 1:nrow(auto)) %>% 
    # Filter the 196 observation = validation set
    filter(! index %in% train) %>% 
    # calcualte the MSE
    summarise( MSE = mean((mpg - predicted)^2 ) )


## poly( ) testing functional form
lm_fit2 <-  auto %>% lm( mpg ~ poly(horsepower, 2), data = . , subset = train )
mean( (auto$mpg - predict(lm_fit2, auto) )[-train]^2 )

# Polynomic power 3
lm_fit3 <-  auto %>% lm( mpg ~ poly(horsepower, 3), data = . , subset = train )
mean( (auto$mpg - predict(lm_fit3, auto) )[-train]^2 )



# Leave one out CV --------------------------------------------------------

glm_fit <- glm( mpg ~ horsepower, data = auto)

# Linear model (glm)
summary(glm_fit)


# 
library(boot)

glm_fit

# cv.glm: return list with CV-result
# delta[1] = result from Cross validation
# Equal to LOOCV
cv.glm(data = auto, glm_fit)$delta[1]

# Some difference, from set.seed?
mean( (auto$mpg - predict(lm_fit, auto) )[-train]^2 )


## Several models, using loops:

string_models <- fun_string_power_model(y = "mpg", x = "horsepower", power = 5, recursion = T)


cv_error <- map_dbl(1:5, function(x){ cv.glm(data = auto, glm( mpg~poly(horsepower, x), data = auto))$delta[1] } )

cv_error



# K-fold Cross-validation -------------------------------------------------

# Model
auto %>% glm( mpg ~ poly( horsepower,2), data =.) %>% summary()

# K-fold
set.seed(123)
map_dbl( 1:2, function(x) cv.glm( data = auto, K = 10,
    glm( mpg ~ poly( horsepower, x), data = auto )
                                 )$delta[1]
)













