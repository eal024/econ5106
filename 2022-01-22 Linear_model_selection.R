

# 
library(tidyverse)

# Data
hitters <- ISLR::Hitters %>% as_tibble() %>% janitor::clean_names()

# Goal: predict the salary to baseball players, on the basis of various statistics.

## Cleaning

# remove na: simplifying 
dim(hitters)

hitters %>% select_if( is.numeric ) %>%  map_df( function(x) sum(is.na(x)) )

# Without NAs
hitters_1 <- na.omit(hitters)
dim(hitters_1)

# Subset Selection Methods
library(leaps)

# Best sub-set selection. Using RSS - regsubsets:
regfit_full <-  regsubsets( salary ~ ., data = as.data.frame(hitters_1) )

# Summary
summary(regfit_full)

# All models in tibble. Long indexed with index
df_models <- summary( regfit_full)[[1]] %>% as.matrix() %>% as.data.frame() %>% as_tibble() %>%
    mutate( index = 1:nrow(.)) %>%     
    pivot_longer(-index) %>% 
    filter( value == T)  

# Arrange models and corresponing R2, R2-adj.

names( summary(regfit_full))

df_models_1 <- df_models %>% group_by(index) %>% nest() %>% ungroup() 
    
df_models_2 <- 
    df_models_1 %>% 
        mutate( rsq       = summary(regfit_full)$rsq,
                rsq_adj   = summary(regfit_full)$adjr2,
                bic       = summary(regfit_full)$bic,
                variables = map(data, function(x) x %>% pull(name))
        )


# See R2 that is highest, give the best model.
df_models_2 %>% filter( bic == -max(abs(bic)) ) %>% unnest( variables) %>% pull(variables)

# Index = 6 ( 6 variables and intercept)

coef(regfit_full,6)



# Forward and backward selection ------------------------------------------

# Backward 
regsubsets( salary ~. , data = as.data.frame(hitters_1), method = "backward" )

# Forward
regfit_forward <- regsubsets( salary ~. , data = as.data.frame(hitters_1), method = "forward" , nvmax = 19)

summary(regfit_forward)




# Choosing models from CV -------------------------------------------------














