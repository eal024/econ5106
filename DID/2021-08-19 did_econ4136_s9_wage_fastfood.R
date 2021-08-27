

# This exercise uses data from Feb–Mar and Nov–Dec 1992 on employment at fast food
# restaurants in the US States of New Jersey and Pennsylvania taken from Card and Krueger
# in The American Economic Review, Vol. 84(4).
# From Seminar 9, econ 4136 2012

# How does minwage effect employemnt in fastfoods stores?

# Packages
library(tidyverse)
library(data.table)

# data
data_wage <- haven::read_dta("data/econ4137_seminar11_did.dta")

# a) Describe the data
desc_dt <- as.data.table(data_wage)[ , map(.SD, function(x) {
    list(
        n_distinct = n_distinct(x),
        mean = mean(x, na.rm = T),
        min  = min(x),
        max  = max(x),
        sd   = sd(x, na.rm = T)
        ) 
    }) , ][ , var := c("n", "mean", "min", "max", "sd") ,]

setcolorder(desc_dt, c("var", names(desc_dt)[1:13]) )

desc_dt

# Alternative
data_wage %>% skimr::skim()

# b) Regression of employment vs. min-wage in Feb.-Mars 1992 (post = 0)
model1 <- data_wage %>% filter(post == 0) %>%  lm( empft ~ minwage + nregs + hrsopen + d2 + d3 + d4 , data = . )

# Increased minwage gives lower employment.  
stargazer::stargazer( list(model1), type = "text")


# c) Interpret B-min-wage and give CI-90%

# Coeff. in the model is simply the difference in emplyoment across states, condition on X./divided by difference in min-wage.
B_minw <- model1$coefficients[model1$coefficients %>% attr("names") == "minwage" ]

# Confidence intervals
ci_interval <- c(
    B_minw - qt(0.95, df = 398-6)*(summary(model1) %>% coefficients())[, "Std. Error"]["minwage"],
    B_minw + qt(0.95, df = 398-6)*(summary(model1) %>% coefficients())[, "Std. Error"]["minwage"]
    ) 

mss <- sum( (fitted(model1)-mean( data_wage$empft[data_wage$post == 0], na.rm = T))^2)
tss <- data_wage %>%
    na.omit() %>% 
    filter( post == 0) %>% 
    mutate( v = (empft- mean(empft))^2 ) %>% 
    summarise( tss = sum(v )) %>%
    pull()

# Standradrd error of the regression. Root MSE
root_mse <- ( 
    # MSE
    sum(residuals(model1)^2)/( nrow(data_wage %>% filter( !is.na(empft), post == 0) ) - 8 - 1) 
    )^0.5


# d) Interpretation of n2 _ dummy:
tibble(map_chr(data_wage, ~attr(.x,"label")), names = names(data_wage)
       ) %>% filter( str_detect(names, "d")
                     )

# D gives different employment across Burger King, KFC, Roys and Wendys, controling for Hours open and number of registers. 
# Reflect different technology or different rate between L and K in different production -> Burger or Chicken.
summary(model1)

# d2 = 1.12 -> 1.12 personer more than Burger King. osv.

# d ii) Test if d2 == d3

data_wage %>% filter( post == 0) %>%  lm( empft ~ minwage + hrsopen + nregs + d2 + I(d2-d3)  + d4,
                                       data = .) %>% 
    summary()


## The vcov matrices for the regression model
vcov(model1)

vcov <- (vcov(model1) %>% as.data.frame() %>%  rownames_to_column())

vard2    <-  vcov$d2[vcov$rowname == "d2"]
vard3    <-  vcov$d3[vcov$rowname == "d3"]
covd2d3  <-  vcov$d2[vcov$rowname == "d3"]
var      <- vard2 + vard3 - 2*covd2d3

# Manuelly calculate t-test.
(coef(model1)[5]-coef(model1)[6])/var^0.5



# Controling for potential selection issues (OMVB)-------------------------------

# i) What may causes selection (why min-wage states may be bias in estimating employment)? 
 # State spesific factors may impact both employment and the minimum wage. Example, high min-wage could mean
 # higher wage level. This may in turn causes lower employment at a given level.
    # This gives states with high min-wage with lower employment.


tbl <- data_wage %>% group_by( state, post) %>% summarise( empl_mean = mean(empft, na.rm = T),
                                                    empl_sd   = sd(  empft, na.rm = T),
                                                    n = n()
                                                    ) %>% 
    pivot_longer(-c(state, post)) %>%  
    pivot_wider( names_from = post, values_from = value ) %>% 
    janitor::clean_names() %>% 
    mutate( diff = x1-x0)

tbl$diff[tbl$name == "empl_mean"][2] - tbl$diff[tbl$name == "empl_mean"][1]

# Diff-Diff-setup
model_did <- data_wage %>% lm( empft ~ post + state + I(post*state), data = . )



# How much does this suggest that min wage affects employment in fast food restaurants? ------

data_wage %>% lm( minwage ~ nregs + d2 + d3 + d4 + post, data = .)

data_wage %>% group_by( post,state ) %>% summarise( m_wage = mean(minwage))

min_wage_state1_post1 <- 5.05
min_wage_state1_post0 <- 4.25

model_did$coefficients[4]/(min_wage_state1_post1 - min_wage_state1_post0)

## The t-test in regression did- may be understating the uncertainty in the effect of min wage on full time employment.
# Calculate the correct st.errors:

# To get correct st.error we need to account the estimation error on the first stage. Easy way is to estimate the IV-model:
estimatr::iv_robust(
    data = data_wage,
    formula = empft ~ post + state + minwage |
        I(post * state) + post + state ,
    se_type = "stata"
)


## The DiD-model included the 
data_wage %>% 
    lm( empft ~ post + state + I(post*state) + hrsopen + nregs, data = .) %>% 
    summary()


## Testing the identifying assumptions:




























