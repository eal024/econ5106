

# General method of moments
library(tidyverse)
set.seed(1234)

df <- tibble(x = rnorm( n = 418, mean = 1, sd = 0.005))

skimr::skim(df)

# MM: just identified
g0 <- function( tet, x) { 
    m1 <- (tet[1] - x)
    m2 <- (tet[2]^2-(x-tet[1])^2)
    
    f <- cbind(m1, m2)
    
    f
    }

g0(df$x, df$x) %>% as_tibble() %>% summarise_all( mean )
gmm::gmm(g = g0,
         df$x,
         c(mu = 0, sig = 0),
         type = c("iterative")) %>% 
    summary()

# The simple example
gmm::gmm( g = function( tet, x) { f <- cbind((tet[1] - x));f},
          df$x,
          c(mu = 1),
          type = c("iterative") ) %>%
    summary()


# Example 2 ---------------------------------------------------------------






