



## 
library(tidyverse)

# Generate data
# 1000 firms. 25 per state, 40 states, 4 groups, 250 for each group, 30 years
data <- tibble( firm = 1:1000, state = rep(c(1:40), each = 25), g = rep(c(1:4), times = 250)) %>% 
    expand_grid( year = seq(from=1980, to = 2009, by = 1 ) ) %>% 
    group_by( firm ) %>%
    mutate( i = 1:30) %>% 
    ungroup() %>% 
    mutate( treat = case_when( 
        (g == 1 & year >= 1986) ~ 1, 
        (g == 2 & year >= 1992) ~ 1,
        (g == 3 & year >= 1998) ~ 1,
        (g == 4 & year >= 2004) ~ 1,
        T ~0),
        e   =  rnorm( n = nrow(.) ,mean = 0,  sd = 0.5^2),
        te1 =  rnorm( n = nrow(.), mean = 10, sd = 0.5^2),
        te2 =  rnorm( n = nrow(.),mean = 8,  sd = 0.2^2),
        te3 =  rnorm( n = nrow(.),mean = 6,  sd = 0.2^2),
        te4 =  rnorm( n = nrow(.),mean = 4,  sd = 0.2^2),
        te =  case_when( g == 1 ~ te1,
                         g == 2 ~ te2,
                         g == 3 ~ te3,
                         g == 4 ~ te4,
                         T ~0),
        te_const = te1
        ) %>% 
    mutate( treat_effect = te_const*treat,
            y_c = firm  + i +  treat_effect + e
    ) %>% 
    select(firm:treat, te,treat_effect, te_const, y_c ) 
    



# Constant treatment effect both over time and over chorts
data %>% filter( firm == 2) %>% head(20)
