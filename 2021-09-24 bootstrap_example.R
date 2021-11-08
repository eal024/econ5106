
# Bootstrap
# Resourses at: http://users.stat.umn.edu/~helwig/notes/npboot-notes.html

# Example 1: The mean
# Ø = E(X), Ø_hat = s(X) = (1/n) sum(xi)
# x assumed normal

tibble( size = c(10,50,100,1000) ) %>%
    mutate( data = map(size, ~replicate(10, rnorm(n = .x, mean = 0, sd = 1)  ) %>%
                           as.data.frame() %>%  
                           pivot_longer( everything() ) 
                           ) 
            ) %>% 
    unnest(data) %>% 
    ggplot( aes( x = value) ) + geom_histogram( aes( y = ..density..) ) +
    geom_density( color = "red") +
    geom_vline( xintercept = 0)  +
    facet_wrap( ~size, scales = "free") 
    

as.data.frame(replicate(10000, rnorm(100))) %>%
    pivot_longer( everything()) %>% 
    ggplot( aes(x = value) ) + geom_histogram(aes( y = ..density..) )
    
    
replicate(3, rnorm(5)) %>% as.data.frame() %>% pivot_longer(everything()) %>%  
    group_by(name) %>% summarise( m = mean(value) )




# Non paramterical boostraping --------------------------------------------

# The empirical CDF

# The ECDF make a jump equal (1/n)*nr.obs, at each point (x) -- see the graph
# As the number of obs. increase, the ECDF converge to the true CDF

set.seed(123)
n <- c(25,50,100,1000)

tibble( n = n, x = map(n, ~rnorm(.x) ) ) %>%
    unnest(x) %>% 
    ggplot( aes(x = x, y = pnorm(x) )) +
    stat_ecdf( geom = "step") +
    facet_wrap(~n)
    ggplot( aes(x = x, y = pnorm(x) )) + 
    stat_ecdf( geom = "step")

nr <- n[2] 
    
tibble( x = rnorm(  nr)) %>% 
    ggplot( aes( x = x)) +
    stat_ecdf( geom = "step") + 
    geom_line( data = tibble( x = seq(from = -2, to = 2, length.out = 100)),
               aes( x = x, y = pnorm(x ) ),
               color = "red"
               ) +
    labs( title = str_c("ECDF colored black, sample n: ", nr ," red = true Normal CDF"))
    
        

# Bivariat model ----------------------------------------------------------





























