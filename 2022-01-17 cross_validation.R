

library(tidyverse)

auto <- ISLR::Auto %>% janitor::clean_names() %>% as_tibble() %>% mutate( index = row_number() )

# Split
obs <- 1:nrow(auto)

# Select the training data 
train_obs <- sample( x = obs, size =  nrow(auto)/2, replace = 0)

# map the data
data <- tibble( cat = c("Train", "test"), data = list( auto %>% filter(index %in% train_obs),
                                               auto %>% filter(! index %in% train_obs)
                                               )
        ) 

# Control the data
# data$data[[2]] %>% arrange(index) %>%  pull(index) %>% head(20)

# Create polynomic function:
str_c( str_c("I(var^",1:3, collapse = ")+"), ")")

fun_string_power_model <- function( y, x,  power, recursion = T){
    
    streng_full <- str_c( y,"~ " , str_c( str_c("I(",x,"^",1:power, collapse = ")+"), ")")   )
    
    if(recursion == T){ 
        return(  map_chr( 1:power, function(p){ str_c( y,"~ " , str_c( str_c("I(",x,"^",1:p, collapse = ")+"), ")")   )  } ))  }
                       else { str_c( y,"~ " , str_c( str_c("I(",x,"^",1:power, collapse = ")+"), ")")   ) }
}


# Mean square error
model <- lm( data = mtcars, cyl ~disp + wt + gear )

sigma <- ( sum(model$residuals^2)/(nrow(mtcars)-4) )^0.5

sigma 

glance(model)

streng_model <- fun_string_power_model(y = "mpg", x = "horsepower", power = 10, recursion = T)

data2 <- data %>% 
    expand_grid( streng_model ) %>% 
    filter( cat == "Train") %>% 
    mutate( model = map2( data, streng_model , function( dat,streng) { lm( formula = streng, data = dat)  }),
            pol   = 1:nrow(.) 
            ) %>% 
    select( pol, model) %>% 
    mutate( valid = map2(model, pol, function(x, p) augment(x, newdata = data$data[[2]] ) %>% summarise( see = (sum(.resid^2)/(nrow(.)-p-1))^0.5) )  ) %>% 
    unnest( valid)



#
data2

model <- lm( data = mtcars, cyl ~disp + wt + gear ) 

model %>% summary()

# MSE
mean( residuals(model)^2)
mean((mtcars$cyl-fitted(model))^2)


model %>% augment() %>% summarise( see = (sum(.resid^2)/(nrow(.)-3-1))^0.5 )

# CV: leave one out -------------------------------------------------------












