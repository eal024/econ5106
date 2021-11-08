
# 1. X ~ random variables, with unknown pdf

data <- tibble( x = c(-1.75, -0.93, 0.1, 0.24, -0.09),
                y = c(12.6, -0.38, 0.32, 0.77, -0.26)
                )

g_hist <- data %>% ggplot( aes( x = x)) +
    geom_histogram( binwidth = 1,
                    aes( y = ..density..),
                    boundary = 0,
                    color = "white") +
    scale_x_continuous( breaks = c(-2,-1,0,1,2))


data_2 <- data %>% mutate( b  =  plyr::round_any(x, 1, f = ceiling),
               a  =  plyr::round_any(x, 1, f = floor),
               x0 = a + (b-a)/2 ) %>% 
    group_by( b) %>% 
    add_count(  ) %>% 
    ungroup() %>% 
    select( -c(x,y) ) %>%
    arrange( a) %>% 
    distinct() %>% 
    mutate( den = (1/sum(n))*(1/(b-a))*n*1 )


g_hist +geom_point( data = data_2, aes( x = x0, y = den), inherit.aes = F, color ="red")

## Epanehinikov
df_point_ex_x <- tibble( X = seq(from = -2, to = 2, by = 1 ) ) %>% 
    expand_grid( xi = data$x) %>%
    mutate( u = (X-xi)/2,
            u2 = u^2,
            epan = (3/4)*(1-u^2),
            criteria = abs(u) < 1
            ) %>% 
    group_by(X) %>% 
    # Remember formula  f(x) = 1/n*bandw *K(u)
    summarise( antall = n(),
               point = (1/(2*n()) )*sum(epan[criteria == T]),
               point_a = (1/(2*n()) )*sum(epan[criteria == T])
               )


# Plot the calculation:
df_point_ex_x %>% ggplot( aes(x = X, y = point)) +
    geom_point( color = "red") +
    geom_line( alpha = 0.5)




# e) ----------------------------------------------------------------------

sd_x <- data %>% 
    mutate( x_hatx = (x-mean(x))^2/(n()-1) ) %>% 
    summarise( var = sum(x_hatx),
               sd = var^0.5 
               ) %>%
    pull(sd)

var(data$x)
sd(data$x)

iqr <- IQR(data$x)

silverman <- 1.3643*1.7188*nrow(data)^(-1/5)*min(sd(data$x),iqr)

## Epanehinikov
df_point_ex_x <- tibble( X = seq(from = -2, to = 2, by = 1 ) ) %>% 
    expand_grid( xi = data$x) %>%
    mutate( u = (X-xi)/2,
            u_alt = (X-xi)/silverman,
            u2 = u^2,
            u_alt_2 = u_alt^2,
            epan = (3/4)*(1-u^2),
            epan_alt = (3/4)*(1-u_alt_2^2),
            criteria = abs(u) < 1,
            c_2 = abs(u_alt_2) < 1 
    ) %>% 
    group_by(X) %>% 
    # Remember formula  f(x) = 1/n*bandw *K(u)
    summarise( antall = n(),
               point = (1/(2*n()) )*sum(epan[criteria == T]),
               point_a = (1/(silverman*n()) )*sum(epan_alt[c_2 == T])
    )


data_ephan_h <- function( h) {
    df_point_ex_x <- tibble( X = seq(from = -2, to = 2, by = 1 ) ) %>% 
        expand_grid( xi = data$x) %>%
        mutate( u = (X-xi)/h,
                u2 = u^2,
                epan = (3/4)*(1-u^2),
                criteria = abs(u) < 1,
        ) %>% 
        group_by(X) %>% 
        # Remember formula  f(x) = 1/n*bandw *K(u)
        summarise( antall = n(),
                   point = (1/(h*n()) )*sum(epan[criteria == T])
                   )
    return(df_point_ex_x)
}

data_ephan_h(h = silverman)
data_ephan_h(h = 2)

ggplot( ) +
    geom_point( data = data_ephan_h(h = 2), aes( x = X, y  = point), color ="red") +
    geom_line( data = data_ephan_h(h = 2),  aes( x = X, y  = point), color ="red") +
    geom_point( data = data_ephan_h(h = silverman),  aes( x = X, y = point) , color ="blue", shape = 2) +
    geom_line(data = data_ephan_h(h = silverman),  aes( x = X, y = point) , color ="blue") +
    geom_line(data = data_ephan_h(h = 3),  aes( x = X, y = point) , color ="green") +
    scale_y_continuous( breaks = seq(from = 0, to =0.5, by = 0.1)) +
    coord_cartesian( ylim = c(0, 0.4))


## Nadaraya Watson estimator for E [Y|X = x]

# The basic idea.
# h = 2
# Criteria |u| < 1

u <- (data$x-(-2))/2
k <- map_dbl( u, ~if( (0.75)*(1-(.x^2) ) < 0) {0} else{(0.75)*(1-(.x^2) )})

# The WLS for point -2
sum(data$y*k)/sum(k)

# Alternativ
tibble(y = data$y, w = k ) %>% lm( y  ~ 1, weights = w, data = .)

# Alternativ approach
tibble( y = data$y, x = k) %>% lm( y ~ 1, weights = abs(x), data = .)

# For all:
interval <- seq(from = -2, to = 2, by = 1 )

NW_estimat_at <- function(vec_x, atx, h, vec_y) {
    
    u <- (vec_x-(atx))/h
    k <- map_dbl( u, ~if( (0.75)*(1-(.x^2) ) < 0) {0} else{(0.75)*(1-(.x^2) )})
    
    # The WLS at point atx
    sum(vec_y*k)/sum(k)
}

# Data
data_NW <- tibble( NW_estimate = map_dbl( seq(from = -2, to = 2, by = 1), function(x) NW_estimat_at(vec_x = data$x, vec_y = data$y, h = 2, atx = x) ),
        x = seq(from = -2, to = 2, by = 1),
        xi = data$x,
        yi = data$y
        )


ggplot(data = data_NW  ) +
    geom_point( aes(x = x, y = NW_estimate)) +
    geom_point( aes(x = x, y = y), color = "red") +
    geom_line(  aes(x,y ), alpha = 0.4, linetype = 2)

# Whit weighted linear regression, included slope term, then predict
tibble(y = data$y, w = k ) %>% lm( y  ~ 1, weights = w, data = .)
NW_estimat_at_degree1 <- function(vec_x, atx, h, vec_y) {
    
    u <- (vec_x-(atx))/h
    k <- map_dbl( u, ~if( (0.75)*(1-(.x^2) ) < 0) {0} else{(0.75)*(1-(.x^2) )})
    
    # The WLS at point atx
    model_degree1 <- tibble( y = vec_y, x = vec_x, k = k) %>% lm( y~ x , weights = k, data = .)
    predict(model_degree1, newdata = tibble( x = atx))
    
        
}

# The basic idea
NW_estimat_at_degree1(vec_x = data$x, vec_y = data$y, h = 2, atx = -2)

# For the hole interval:
map_dbl( seq(from = -2, to = 2, by = 1), function(x) NW_estimat_at_degree1(vec_x = data$x, vec_y = data$y, h = 2, atx = x) )








