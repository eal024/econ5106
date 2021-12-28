

## The log likelihood function
#'  The conceptual motivation behind parameter estimation is to pick
#'  that value of the parameter that is “most likely", given the data. 

# Example 1 binomial model: P(y) = p^y + (1-p)(n-y)

# log likelihood function L(p|n, y) = log(n y) + y*log(p) + (n-y)*log(1-p)
# Example flip (11 times) the unfair coin, y appear 7 times. 

# ex.
c  <- 1



ll <- function(p) { -sum( log(1) + 7*log(p) + (3)*log(1-p) ) }

# -> 7/10

# What is the value for P, most likely given the data? 
tibble( P = seq(from =.1, to = 0.9, by = 0.001) ) %>% 
    rowwise( ) %>% 
    mutate( ll  = ll(c = 0.1, p = P)) %>% 
    ungroup() %>% 
    # Consept: where the ll is max
    filter( ll == max(ll))

# using the function Optimum
ll <- function(p) { -sum( log(1/1 ) + 5*log(p) + (5)*log(1-p) ) }
optim( par = c(p = 0.5), ll)


## Example Two

dat <- tibble( x = c(1:6), y = c(1,3,5,6,8,12))

dat %>% lm( y ~x , data =.) %>% residuals() %>% sum()

min.Rss <- function(data, par){
    with( data, sum((par[1] + par[2]*x -y)^2) )
}

test <- function(data, par){
    with( data, (par[1] == 8 & par[2] > 350))
}



with(mtcars, mpg[cyl == 8  &  disp > 350])
mtcars$mpg[mtcars$cyl & mtcars$disp > 350]

















