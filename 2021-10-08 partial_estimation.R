


library(tidyverse)

df_hs <- haven::read_dta("data/headstart.dta")

df_hs %>% skimr::skim()

# 1) Estimate the effect of participation in Head Start and the prop. of obtaining a high school degree OLS

model_ols   <- df_hs %>% lm( highschool_child  ~ headstart, data = .)
model_ols_c <- df_hs %>% lm( highschool_child  ~ headstart + I(factor(edu_parent) ) , data = .)

# OLS model summary
stargazer::stargazer(model_ols,model_ols_c , type = "text")

# Participated in HS increases prop. by 25 per cent.
# Should be interpreted with caution.Even though we control for parents education, there is still OVB and selection issues.

# 2 The share of children that obtained a high school degree

df_share <- df_hs %>% group_by(headstart, highschool_child ) %>% count() %>% group_by(headstart) %>% mutate( andel = n/sum(n))
# a) The share of child that obtain a high school degree

df_share$n %>% sum()
df_share$n[df_share$headstart == 1] %>% sum()
#
mean(df_hs$headstart)
mean(df_hs$highschool_child[df_hs$headstart == 0 ])
mean(df_hs$highschool_child[df_hs$headstart == 1])

df_share %>%  filter( headstart == 1, highschool_child == 1)

df_hs %>% filter( headstart == 1) %>% summarise( E_degree = mean(highschool_child) )

# Non participants
df_share %>%  filter( headstart == 0, highschool_child == 1)
df_hs %>% filter( headstart == 0) %>% summarise( E_degree = mean(highschool_child) )

# Share of non participants: 78 per cent.
df_hs %>% group_by(headstart) %>% count() %>% ungroup() %>% mutate( andel = n/sum(n))

# Can also be calculated with OLS
df_hs %>% lm( highschool_child ~headstart,data = .)
df_hs %>% filter( headstart == 1) %>% lm(highschool_child ~ 1, data = .)  

# 3) The worst case bounds around
df_p <- ungroup(df_share) %>% summarise( p = sum(n[headstart == 1])/sum(n),
                                         non_p = sum(n[headstart == 0])/sum(n)
                                      )

df_share

df_p
y_min <- 0
y_max <- 1
y_00 <- mean(df_hs$highschool_child[df_hs$headstart == 0])
y_11 <- mean(df_hs$highschool_child[df_hs$headstart == 1])
p <- df_p$p
non_p <- df_p$non_p

# Worst case bound E[y0]
E_y0 <- c( low = y_min*p + y_00*(1-p), upper =  y_max*p + y_00*(1-p))

# Worst case bound E{y1}
E_y1 <- c( low = y_min*(1-p) + y_11*p, upper =  y_max*(1-p) + y_11*p)


# 4) Worst case lower and uypper B. of ATE
# ATE = {LB(E[y1]) - UB(E[y0])} -{UB(E[y1]) - LB(E[y0])}
ATE <- c( low = (E_y1["low"]-E_y0["upper"] ) , upper = (E_y1["upper"]-E_y0["low"]) )


# 5) Monotonic treatment selection assumption

# MTS assumption : E[High school degree_1 | HS = 0]  > E[ High school degree_1 | HS = 1]

# E[High school degree_treatment =1 | HS = 0]: Non observable




# 7 MTS lower and upper ---------------------------------------------------

df_hs %>% group_by(headstart) %>% summarise( mean_y = mean(highschool_child))

# Lower bound 
df_ystat <- df_hs %>%
    summarise( 
        p = sum(headstart)/nrow(.)  ,
        non_p = 1-p,
        mean_y_nohs = mean(highschool_child[headstart == 0]),
        mean_y_hs   = mean(highschool_child[headstart == 1]),
        mts_y0       = list( lower = mean(highschool_child[headstart == 0])*non_p + 0,
                             upper = mean(highschool_child[headstart == 0])*non_p + mean(highschool_child[headstart == 0])*p
                             )  
        ) 

c( df_ystat$mts_y0[1], df_ystat$mts_y0[2])

# y1
df_hs %>%
    summarise( 
        p = sum(headstart)/nrow(.)  ,
        non_p = 1-p,
        mts_y1       = list( lower = mean(highschool_child[headstart == 1])*p + mean(highschool_child[headstart == 1])*non_p,
                             upper = mean(highschool_child[headstart == 1])*p + 1*non_p
        )  
    ) %>% unnest( mts_y1)



df_hs_parent <- df_hs %>%
    #mutate(p = sum(headstart)/nrow(.),
    #       non_p = 1-p) %>% 
    group_by(  edu_parent) %>% 
    summarise( 
        p = sum(headstart)/n()  ,
        non_p = 1-p,
        mean_y_nohs = mean( highschool_child[headstart == 0]),
        mean_y_hs   = mean( highschool_child[headstart == 1]),
        mts_y      =  list( lower.y0 = mean(highschool_child[headstart == 0])*non_p + 0,
                            upper.y0 = mean(highschool_child[headstart == 0])*non_p + mean(highschool_child[headstart == 0])*p,
                            lower.y1 = mean(highschool_child[headstart == 1])*non_p + mean(highschool_child[headstart == 1])*p,
                            upper.y1 =  mean(highschool_child[headstart == 1])*p + 1*non_p 
                            ),
        .groups = "drop"
    ) 


    

df <- df_hs_parent %>%
    select(
        edu_parent,
        mts_y) %>% 
    group_by( edu_parent ) %>% 
    mutate( interval =  c("lower.y0", "upper.y0", "lower.y1", "upper.y1")) %>% 
    pivot_wider( names_from = interval, values_from = mts_y) %>% 
    unnest( cols = c(lower.y0, upper.y0, lower.y1, upper.y1) 
            ) %>% 
    distinct()

df


df %>% pivot_longer( -edu_parent) %>% 
    mutate( y =  ifelse( str_detect(name, "y1"), "y1", "y0"), edu_parent = as.integer(edu_parent) ) %>% 
    arrange( y, name) %>% 
    ggplot( aes( x = as.integer(edu_parent), y = value, fill = as.factor(name), color = as.factor(name)) ) +
        geom_line() +
        geom_point( color = "black", size = 3, aes(shape = name)) +
        facet_wrap(~y , nrow = 2) 




 







