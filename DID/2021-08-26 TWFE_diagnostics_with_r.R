
# Packages and data
library(tidyverse)
theme_set(theme_light())


# Regression coefficients - 

set.seed(1234)

# Simulated data
data_sim <- tibble( treat = round( rbeta(n = 500, shape1 = 3, shape2 = 7)*100 ) ) %>% 
    mutate( y = rnorm(n = 500, mean = 800, sd = 200) + 10*treat + rnorm(n = 500, 200, 100) 
    ) %>% 
    select(y, treat)

# 
data_sim %>% pivot_longer(everything()) %>% ggplot( aes(x = value) ) + geom_histogram( ) + facet_wrap(~name, scales = "free")    

# Histogram y
data_sim %>% ggplot( aes(x = y)) + geom_histogram( binwidth = 100, boundary = 0, color = "white", fill = "#FFA615") + scale_x_continuous(labels = scales::dollar_format())


data_sim %>% ggplot( aes(x = treat, y = y)) + geom_point( size = 1) + geom_smooth( method = "lm", se = F)

# ATE
data_sim %>% lm( y ~  treat, data = .) %>% summary()

# Frisch-Waugh:
data_sim %>% lm( treat ~1 , data = .)

mean(data_sim$treat)

# Coeff. from residuals from Treat-regression (bivariat is only the res = Treat - mean(Treat)  )
data_beta_from_x_weight_on_y <- data_sim %>% mutate( t_residual = treat - mean(data_sim$treat),
                                                     t_r_2      = t_residual^2,
                                                     treatment_weight = (t_residual)/sum(t_r_2),
                                                     diff      = treat - mean(treat)
) 

data_beta_from_x_weight_on_y %>%  summarise( beta = sum(y*treatment_weight) )

# Treatment weigh: How each variables weight in the B
# Residual and diff are identical.
data_beta_from_x_weight_on_y

# The weight should sum up to 0:
sum(data_beta_from_x_weight_on_y$treatment_weight)


data_beta_from_x_weight_on_y %>% ggplot( aes(x = treatment_weight)) +
    geom_histogram( color = "white",
                    fill = "#F6C848") + 
    geom_vline( xintercept = 0, color = "red", size = 1.1)

# Treat below mean get a negative weight. "May be thout of as part of the comparision group".



# # Residual weights with TWFE models -------------------------------------

# Problem with differently-timed treatments -> is that treated obs (T above mean) get negative weights (and vice versa)
# not always a problem, but some time it can be.
# Jakiela -> give diagnostics to help determine if there are serious issues with these residualized treatment weights


# New data to illustrate the problem:
data_fpe_raw <- haven::read_dta("https://github.com/pjakiela/TWFE/raw/main/WDI-FPE-data.dta") %>% 
    mutate( year = as.factor(year))

# Remove primary school enrollment which is na. remove rows where secondary school is missing

data_prim <- data_fpe_raw %>% filter(!is.na(primary))

data_sec  <- data_fpe_raw %>% filter(!is.na(secondary))

data_prim %>% skimr::skim()

## The TWFE-model

# With cluster robust standard error
model1_twfe <- data_prim %>% lm(primary  ~ treatment + country + year, data = . ) 


model1_twfe%>% broom::tidy() %>% filter( !str_detect(term, "year|country") )

# cluster by country:
df_country_prim <- unique(data_prim$country)

# Cluster st.error
lmtest::coeftest(model1_twfe, 
                 vcov = sandwich::vcovCL,
                 cluster = ~country,
                 df = data_prim %>% distinct(country) %>% nrow()
) %>% 
    broom::tidy() %>% 
    filter(!str_detect(term, "country|year"))

#
model_lm_robust <- estimatr::lm_robust( primary ~treatment, 
                                        fixed_effects =  ~country + year,
                                        data = data_prim,
                                        cluster = country , se_type = "stata"
                                        )


# TWFE estimate with residualized treatment weights
# There is a problem with the different timing 
    # countries implement laws of fees in differnet years
data_prim %>% 
    filter( treatment == 1) %>% 
    group_by(country) %>% 
    summarise( ar = min( as.numeric(as.character( year)), na.rm = T))


# A look at the problem:
# ATE;

tr_res_prim <- data_prim %>% lm( treatment ~ country + year, data = .)

data_prim_w <- data_prim %>% 
    mutate( treat_res  = residuals(tr_res_prim),
            res_weight =  treat_res/sum(treat_res^2)
            ) 

data_prim_w %>% summarise( beta_twfe = sum(primary * res_weight ) )    

data_prim_w %>% summarise( sum_w = sum( res_weight ) )    

# Remember from the simple example with fake data that observations in the treatment group typically have positive treatment weights, 
# while those in the comparison /
# control group have negative weights. With TWFE, some observations’ weights switch directions. 
# There are systematic reasons for this. According to Jakiela (2021, 5), 
# negative weights in treated observations are more likely in (1) early adopter countries, since the country -
# level treatment mean is high, and (2) later years, since the year - level treatment mean is higher.

 



















