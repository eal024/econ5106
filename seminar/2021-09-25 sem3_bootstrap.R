
library(tidyverse)
library(data.table)

# Bootstrap
# Study the behavior of US Black women in Midwest region. Data from Current Population Survey

cps <- haven::read_dta("data/cps09mar.dta")

ipumsr::ipums_val_labels(cps$region)

# See label of multivar.
list( (cps$region) , (cps$marital), (cps$race)) %>% map(~ipumsr::ipums_val_labels(.x) )

skimr::skim(cps)
(cps$education) 

# 1)
# Filter region == Midwest, martial equal. 
# n = 433
cps_1 <- cps %>%
    filter( region == 2 & race %in% c(2) & female == 1 ) %>% 
    mutate( mar = ifelse( marital %in% c(1:3), 1, 0) )


# 2 ) The ML estimation:
probit_model <- glm( data = cps_1, formula = mar ~ age + I(age^2) + education, family = binomial("probit") )

probit_model$coefficients["age"]

# Explained the marriage behavior
stargazer::stargazer(probit_model, type = "text" )

# 3) How calculate the bootstrap st.error

# i: r-th random sample with replacement
# ii: calculate B*r
# iii: repeat i. and ii. R-times
# iv: se( B*r) = (1/(R-1)) sum[ (B*r - mean(B))^2 ]

df_model <- tibble( data = map2( 1:1000, nrow(cps_1), function(x,y) cps_1[sample(nrow(cps_1), size = y, replace = T),])) %>% 
    mutate( model     = map(data, ~glm( data = .x, formula = mar ~ age + I(age^2) + education, family = binomial("probit") )),
            education = map(model, ~.x$coefficients["education"]) 
            ) %>% unnest( education)

se <- df_model %>%
    select(education) %>% 
    mutate( m_educ = mean(education),
            se = (education - m_educ)^2
            ) %>% 
    summarise( se_b = ((1/( nrow(.)-1))*sum(se))^0.5 )

df_model %>%
    ggplot(aes( x = education)) +
    geom_histogram( binwidth = .005, boundary = 0, color = "white", fill = "#FFA615",
                    aes( y = ..density..)
    ) +
    geom_vline( xintercept = c(probit_model$coefficients["education"] - se$se_b,
                               probit_model$coefficients["education"] + se$se_b,
                               probit_model$coefficients["education"]),
                color ="red", linetype = 2, size = 1)

# SE 
sd(df_model$education)

# 4) Different methods to test H, education does not have any effect

# LM method
(lmtest::coeftest( probit_model)[,1][4]-0)/lmtest::coeftest( probit_model)[,2][4]

# Bootstrap method 
sd(df_model$education)


## 4) Confidence interval method

# i) Percentil method
quantile(df_model$education, probs =  c(0.05/2, (1-0.05/2) ) )

median(df_model$education)

# 5%/2-percentil, and 97.5-percentil 
(length( df_model$education)/100)*5/2
(length( df_model$education)/100)*97.5

sort(df_model$education)[25]
sort(df_model$education)[975]

# ii) The percentil t-method

probit_model <- glm( data = cps_1, formula = mar ~ age + I(age^2) + education, family = binomial("probit") )

b_educ_true <- summary(probit_model)$coefficients[4,2]

# Construction t_r for each simulation
df_model_2 <- df_model %>% 
    mutate( se_b_r =  map(model, ~summary(.x)$coefficients[4,2]),
            t_r    =  map2(education, se_b_r, function(x,y) { (x-b_educ_true)/y })
            ) %>% 
    unnest( t_r ) 



# Use the formula [B_true - t_r_1-a/2*sigma_hat , B_true - t_r_1-a/2*sigma_hat]
# sigma_hat = 

sigma_hat <- summary(probit_model)$coefficients[4,2]

t_r_quantile <- quantile(df_model_2$t_r, probs = c(0.05/2, (1-(0.05/2) )   ) )

# Unclear
probit_model$coefficients["education"] - t_r_quantile[2]*sigma_hat
probit_model$coefficients["education"] + t_r_quantile[1]*sigma_hat



# 5 The bias corrected estimate of Beta age:
2*probit_model$coefficients["education"]-mean(df_model_2$education)


# 6) At which age is the woman is mos likely to  marry

# d) propose a estimate:

skimr::skim(cps_1)

probit_model_6 <- glm( data = cps_1, formula = mar ~ age + I(age^2) +  education  + union , family = binomial("probit") )

probit_model_6$coefficients

summary(probit_model_6)



# e) How obtain standard error:

# Delta method

# i) Boostrap
df_model_6 <- tibble( data = map2( 1:100, nrow(cps_1), function(x,y) cps_1[sample(nrow(cps_1), size = y, replace = T),])) %>% 
    mutate( model     = map(data, ~glm( data = .x, formula = mar ~ age + I(age^2) + education, family = binomial("probit") )),
            age  = map(model, ~.x$coefficients["age"]),
            age2 = map(model, ~.x$coefficients["I(age^2)"])
    ) %>% 
    unnest( cols = c(age, age2)) %>% 
    mutate( age_max_estimat = -age/(2*age2) )

df_model_6 %>% summarise( mean = mean(age_max_estimat), sd = sd(age_max_estimat), test = (mean-40)/sd )

quantile( df_model_6$age_max_estimat, probs = c(0.025, 0.975))















