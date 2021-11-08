
library(tidyverse)
library(patchwork)

# The histogram
df <- tibble( x = c(4,8,12,14,16,18, 25) )

df2 <-df %>% mutate( b  =  plyr::round_any(x, 10, f = ceiling),
               a  =  plyr::round_any(x, 10, f = floor),
               x0 = a + (b-a)/2 ) %>% 
    group_by( b) %>% 
    add_count(  ) %>% 
    ungroup() %>% 
    select( -x) %>% 
    distinct() %>% 
    mutate( den = (1/sum(n))*(1/(b-a))*n*1 )
    
        

graf <- ggplot(data = df,  aes( x = x))  

g_freq        <- graf + geom_histogram( boundary = 0, binwidth = 10 , color = "white")
g_density     <- graf + geom_histogram( boundary = 0, binwidth = 10 , color = "white", aes( y = ..density..))

g_density + geom_hline( yintercept =  c(df2$den), color = "red", linetype = 2, size = 1.) 



# 
