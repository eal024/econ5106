# Maximum Likelihood alt  -------------------------------------------------------

mlc <- function(B0, B1, x, y ,sigma) {
    
    xb = B0 + B1*x
    #L  =  ifelse( y > 0, log(rnorm(1, mean = xb, sd = sigma)), log( pnorm( -xb/sigma) ))
    map2_dbl( y, xb , function(y, xb)  
    {
        if( y > 0) { log( rnorm( n = 1, mean =  xb, sd = 1) ) }else{ log( pnorm(-xb/1)) } 
    } )
    
}

sum(mlc(B0 = -3, B1 = 0, y = df$y, x = df$lnw, sigma = 0.6), na.rm = T)

df_calc <- tibble( B0 = seq(from =-3, to = -1, by =0.5 )) %>% 
    expand_grid( B1  = seq(from =0, to = 2, by = 0.1)) %>% 
    mutate( index = 1:nrow(.)) %>% 
    mutate( calc = map2(  B0, B1, function(x1,x2) {  sum( mlc(B0 = x1, B1 = x2, y = df$yc, x = df$lnw, sigma = 0.6), na.rm = T) } )
    ) %>% 
    unnest(  cols = calc)

df_calc_1 <- df_calc %>% select(B0, B1, calc)

df_calc_1 %>% filter( calc == min(calc))

## Tobit model
library(VGAM)
lm( y ~ lnw, data = df) %>% summary()
vglm( yc ~ lnw, data =df , tobit( Lower = 0)) %>% summary()
# This seems not calculate correct
vglm( yt ~ lnw, data =df , tobit( Lower = 0)) %>% summary()
lm( data = df, y ~ lnw ) %>% summary()
