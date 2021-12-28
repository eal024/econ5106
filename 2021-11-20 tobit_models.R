

## Tobit regression

library(tidyverse)
library(survival)

# Example One: Use of survreg (from survival)
acad_apt <- read_csv("https://stats.idre.ucla.edu/stat/data/tobit.csv") %>%
    mutate(prog = factor(prog, labels = c('acad', 'general', 'vocational')))


fit_aer <- AER::tobit(
    apt ~ read + math + prog, 
    data = acad_apt,
    left = -Inf,
    right = 800
)

summary(fit_aer)


# Example Two
mus18 <- haven::read_dta("http://fmwww.bc.edu/ec-p/data/mus/mus18data.dta")

mus18_1 <- mus18 %>% filter( year == 2, age >= 18)

coins_tobit <- AER::tobit(
    med ~ age + female + num + linc + educdec + site + coins, 
    data = mus18_1,
    left = 0,
    right = Inf
)

summary(coins_tobit)

# Prop. P(y > 0) = Ø(xb/simga)
sigma <- (sum(residuals(coins_tobit)^2)/(nrow(mus18_1)-7) )^0.5

pred_coins <- predict(coins_tobit, newdata = mus18_1)
pred_coins0 <- predict(coins_tobit, newdata = mus18_1 %>% mutate( coins = 0))
pred_coins100 <- predict(coins_tobit, newdata = mus18_1 %>% mutate( coins = 100))

# Prediction P(y > 0 | coins = c*)
pnorm(mean(pred_coins)/sigma)
pnorm(mean(pred_coins0)/sigma)
pnorm(mean(pred_coins100)/sigma)




