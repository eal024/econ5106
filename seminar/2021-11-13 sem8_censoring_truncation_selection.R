

library(tidyverse)

# Seminar Censoring, Truncation and Selection Models.

# Health care (Deb. & Trivedi (2002)
mus18 <- haven::read_dta("http://fmwww.bc.edu/ec-p/data/mus/mus18data.dta")

df_label <- tibble( n = names(mus18), label = mus18 %>%
            map_chr( function(x) { 
                if( is.null(attr(x, "label")) ){ return("null")}
                else{ attr(x, "label") } }),
            index = 1:ncol(mus18)
            )


# Study year 2 and age > 18
mus18_1 <- mus18 %>% filter( year == 2, age >= 18)
    
# a) medical expenditures - age, sex, and fam size
model1 <- mus18_1 %>% 
    lm( med ~ age + female + num + linc + educdec + (factor(site)) + coins, data =.)


stargazer::stargazer( model1, type = "text")

# coins: 100 = must pay all cost your self.
# Coins: one percent incr. cost born by the patient -> reduce medical expence by -0.74. Not significant. cant conclude

# b) 
# med censored at 0 -> dont observe med lower than 0.
# 20% = 0 medical expenditures -> Estimate censored regression max likelihood

# OLS observed
model2 <- df_obs %>% 
    filter( observed == 1) %>%
    lm( med ~ age + female + num + linc + educdec + site + coins, data = .)

stargazer::stargazer( list(model1,model2), type = "text")

# Tobit model: Correct
library(VGAM)
summary(
    tobit <-
        VGAM::vglm(
            med ~ age + female + num + linc + educdec + site + coins,
            tobit( 00),
            data = mus18_1
        )
)



# What is the propability for positiv med expend. if co-ins = 100 and 0:
# P( y > 0) = Ø(xb/sigma)

sigma <- (sum((mus18_1$med-  p[,1])^2)/(nrow(mus18_1)-8))^0.5


p <- predict( tobit, newdata = mus18_1 %>%
                  summarise_all( mean) %>% 
                  mutate(        coins  = 100) 
)

p2 <- predict( tobit, newdata = mus18_1 %>%
                  summarise_all( mean) %>% 
                  mutate(        coins  = 0) 
)


# What is the propability for positiv med expend. if co-ins = 100 and 0:
# P( y > 0) = Ø(xb/sigma)

pnorm( mean(p[,1])/sigma ) 
pnorm( mean(p2[,1])/sigma ) 

# c) Average partial effect increase coins from 0 to 100 on observed expenditure
VGAM::margeff(tobit, subset =  mus18_1 %>%
                  summarise_all( mean) %>% 
                  mutate(        coins  = 0) 
              )











# # 2-step approach -------------------------------------------------------


# mus18_2 <- mus18_1 %>% 
#     select(med, age,female, num, linc,site, educdec, coins ) %>% 
#     mutate( obs_index = ifelse( med > 0, 1, 0) )
# 
# probit <- mus18_2 %>% glm( obs_index ~ age + female + num + linc + site +  educdec + coins, family = binomial("probit") ,data = .)
# 
# mil <- dnorm( predict(probit) )/pnorm( predict(probit) )
# 
# mil[mus18_2$obs_index == 1]
# 
# 
mus18_2 %>%
    filter( med > 0) %>%
    mutate( imr = mil[mus18_2$obs_index == 1] ) %>%
    lm( med ~ age + female + num + linc + site +  educdec + coins  + imr , data = .) %>%
    summary()




# 1) observ. index
df_obs <- mus18_1 %>%
    mutate( index = 1:nrow(.), observed = ifelse(med == 0, 0, 1)) %>%
    select( med,observed, index, age, female, num, linc, educdec, site, coins)





