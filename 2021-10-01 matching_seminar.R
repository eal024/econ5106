

library(tidyverse)


# 1) Exact matching--------------------------------------------------------

data <- tribble(
    ~x, ~d, ~y,
    1,   0, 10, 
    1,   1, 15, 
    1,   1, 20, 
    2,   0, 25, 
    2,   1, 30, 
    2,   0, 30, 
    3,   1, 25, 
    3,   1, 35, 
    4,   0, 50, 
    5,   0, 55, 
)

data$id <- 1:nrow(data)


# a) Estimate causal effect by OLS

data %>% group_by( d) %>% summarise( mean_y = mean(y))

lm(data = data, y ~ x + d) %>% summary()

# b) Exact matching
data_exact_match <- tibble( x = 1:5) %>% 
    expand_grid( d = c(0,1)) %>% 
    left_join( data %>% group_by(x,d) %>% count() , by = c("x","d")) %>% 
    replace_na( list( n = 0 ) ) %>% 
    group_by( x ) %>% 
    mutate( no_cs = ifelse( n == 0, 1, 0),
            no_cs = sum(no_cs)
            ) %>% 
    filter( no_cs == 0)

# Calculate the treatment effect exact match
data %>%
    filter(x %in% c(data_exact_match$x %>% unique() ) ) %>% 
    lm( y ~ x + d, data = .) %>% 
    summary()


# c) Compare with OLS result. How reduce the differences
data %>% 
    lm( y ~ I(as.factor(x)) + d, data = .) %>% 
    summary()




# 2) Effect if union status on log hourly wage ----------------------------

nlsw88 <- haven::read_dta("data/nlsw88.dta") %>% janitor::clean_names()

tibble( names = names(nlsw88), text = map_chr( nlsw88, function(x) attr(x ,"label") ))

# a)
nlsw88 %>% skimr::skim()

model_21 <- nlsw88 %>% lm( log(wage) ~union, data = .)

model_21_c <- nlsw88 %>% lm( log(wage) ~union + age + I(age^2) + collgrad + race  , data = .) 

stargazer::stargazer(model_21, model_21_c, type = "text")

# b) Compare the distribution of characteristics for Union = 1 and Union = 0.

nlsw88 %>% names()

var <- c("age", "race", "married", "never_married", "grade", "collgrad", "south", "wage", "hours", "smsa", "tenure")

nlsw88 %>% select(var)

nlsw88 %>% 
    select(union, var) %>% 
    na.omit() %>% 
    group_by(union) %>% 
    summarise_all( mean) %>% 
    pivot_longer( -union) %>% 
    pivot_wider( names_from = union, values_from = value) %>% janitor::clean_names() %>% 
    mutate( diff = x1-x0) 
    

# with t-value
tibble( vari = var, data = list(nlsw88 %>% na.omit()) ) %>% 
    mutate( test = map2(vari, data, function(x, y) {lm( str_c(x ,"~ union"), data = y) %>% broom::tidy()} ) 
            ) %>% 
    unnest(test) %>% 
    filter( ! str_detect(term, "Inter"))


lapply(var, function(x) {t.test(as.data.frame(nlsw88)[,x] ~ as.data.frame(nlsw88)[ , "union"]) } )

t.test(as.data.frame(nlsw88)[, "age"] ~ as.data.frame(nlsw88)[, "union"] )

# c) How to use the weighting technique to construct the counterfactual to estimate ATT

# Propensity score:
# How to estimate the Matching estimate (Matching in practice)
# 1) Define reseach question (CIA)
# 2) Estimate p(x)

nlsw88 %>% glimpse()

ps_model <- glm( formula = union ~ ttl_exp  + tenure + south +  collgrad +  race,
                 data   = na.omit(nlsw88),
                 family = binomial("logit")
                 ) 

# PS_score summary
summary(ps_model)

# Examining the region of common support
nlsw88_ps <- na.omit(nlsw88) %>% 
    select(union, everything()) %>% 
    mutate( pr_score = predict(ps_model, type = "response")  ) %>% 
    relocate( pr_score , .after  = union) 

nlsw88_ps %>%     select(union, pr_score) %>% 
    ggplot( aes( x = pr_score) ) + geom_histogram( color = "white" ) + facet_wrap(~union )

# How Match 
nlsw88_ps %>% 
    select(union, pr_score, wage) %>% 
    arrange( pr_score) %>% head(20)


# f) How use the weighting method:

# Counter factual for treated (to estimate ATT)
## Make treated to untreated: Treated individuals 􀀀!untreated indiviuals 1-p(x)/p(x)
nlsw88_ps %>% 
    select(union, pr_score, wage,collgrad) %>% 
    mutate( log_wage = log(wage),
            union  = as.integer(union),
            collgrad = as.integer(collgrad),
            weight = ifelse(union == 0, (pr_score/( 1- pr_score)) , 1  ),
            collgrad_w = collgrad*weight,
            log_wage_weighted = log_wage*weight
    ) %>% 
    arrange(pr_score) %>% 
    group_by(union) %>% 
    summarise( mean_log_w = sum(log_wage_weighted, na.rm = T )/sum(weight),
               n = n(),
               mean_collg = sum(collgrad_w)/sum(weight),
               mean_collgr = weighted.mean(collgrad, w = weight)
               )

sum( c(2,4,6)*w)/sum(w)
weighted.mean(c(2,4,6), w = c(0.5,1,0.5))


# Effect
nlsw88_ps %>% 
    select(union, pr_score, wage) %>% 
    mutate( log_wage = log(wage),
            union  = as.integer(union),
            weight = ifelse(union == 0, (1/( 1- pr_score)) , 1/pr_score  ),
            log_wage_weighted = log_wage*weight
    ) %>% 
    group_by(union) %>% 
    summarise( mean_log_w = mean(log_wage_weighted, na.rm = T ))8.13-2.48


# Regression: -------------------------------------------------------------

nlsw88_ps_w <- nlsw88_ps %>% 
    select(union, pr_score, wage,collgrad) %>% 
    mutate( log_wage = log(wage),
            union  = as.integer(union),
            collgrad = as.integer(collgrad),
            weight = ifelse(union == 0, (pr_score/( 1- pr_score)) , 1  ),
            collgrad_w = collgrad*weight,
            log_wage_weighted = log_wage*weight )

# Weighted with weight            
nlsw88_ps_w %>% lm( log_wage ~ union, weights = weight, data = .) %>% summary()




# 3 LaLonde (1986)-------------------------------------------------------------

# dir("data") -> LaLonde
LaLonde <- haven::read_dta("https://www.dropbox.com/s/aw4yi13mz9z03yf/lalonde2.dta?dl=1")

# a) Is the NSW sample consistent with randomized sample?
LaLonde %>% 
    distinct(sample)

LaLonde %>% 
    filter( sample == 1) %>% 
    select(-c(idnum, sample) ) %>% 
    group_by( treated) %>% 
    summarise_all( .funs =  list( N = ~n(), mean = ~mean(.), min = ~min(.), max =  ~max(.), se = ~sd(.) )) %>% 
    pivot_longer( -treated) %>% 
    separate( col = name, into = c("var", "stat"), sep = "_") %>% 
    pivot_wider( names_from = stat, values_from = value) %>% 
    unnest( cols = c(N,mean,min,max, se)) %>% 
    select(-ra) %>% 
    arrange( var) %>% head(20)
    
# Box plot
LaLonde %>% 
    filter( sample == 1) %>% 
    select(-c(idnum, sample) ) %>% 
    pivot_longer( -treated) %>%
    group_by(name) %>% 
    mutate( binom = case_when( max(value) == 1  & min(value) == 0  ~  1,
                               min(value) == 0  &  max(value) > 10  ~ 2,
                               ! between(value, 0.01, 0.99)  ~ 3, 
                               T ~ 0 ) 
            ) %>% 
    ungroup() %>% 
    ggplot( aes( x = name, y = value, fill = factor(treated), color = factor(treated) ) ) +
    geom_boxplot( ) +
    facet_wrap(  ~binom, scales = "free") +
    coord_flip( )

# b) With regression

var <- c("age", "educ", "black", "married", "kids18")

# The data seems randomized
tibble( vari = var, data = list( LaLonde %>% filter( sample == 1) %>%  na.omit() )  ) %>% 
    mutate( test = map2(vari, data, function(x, y) {lm( str_c(x ,"~ treated"), data = y) %>% broom::tidy()} ) 
    ) %>% 
    unnest(test) %>% 
    filter( ! str_detect(term, "Inter"))


# c) Use sample NSW (Treated) and CPS (Comparisons) 
LaLonde_match <- LaLonde %>% 
    mutate( keep = ifelse( (sample == 2 | sample == 1 & treated == 1), T, F ),
            treated = ifelse( sample == 2, 0, treated) 
            ) %>% 
    filter( keep ) #

# Consist of untreated from CPS and treated from NSW
LaLonde_match %>% distinct( sample, treated) 


# d) OLS of treatment
LaL_model1_ols <- LaLonde_match %>% lm( re78 ~ treated , data = .)
LaL_model2_ols <- LaLonde_match %>% lm( re78 ~ treated + age + educ + black + married + nodegree + hisp + kids18, data = .)    

# Treatment seems to give have negative impact on earnings. 
# The effect decrease substantially when including controls
stargazer::stargazer(LaL_model1_ols, LaL_model2_ols, type = "text")


# e) heavily unbalanced. OLS will give (most likely) a wrong/biased estimate for treatment on y
tibble( vari = var, data = list( LaLonde_match %>% select(-c(dwincl,early_ra) ) %>%  na.omit() )  ) %>% 
    mutate( test = map2(vari, data, function(x, y) {lm( str_c(x ,"~ treated"), data = y) %>% broom::tidy()} ) 
    ) %>% 
    unnest(test) %>% 
    filter( ! str_detect(term, "Inter"))


# f) Nearest neighborhood 

# 1) PS
ps_model <- LaLonde_match %>% glm( treated ~ age + I(age^2) + I(age^3) + educ  + I(educ^2) + black + married + nodegree + hisp + re74 + kidmiss + kids18 , data = ., family = binomial("logit"))

df <- tibble( treated = LaLonde_match$treated, ps_score = predict(ps_model, type = "response"), re78 = LaLonde_match$re78) %>%
        mutate( weight = ifelse(treated == 0, (ps_score/( 1- ps_score)) , 1  ),
                re78_w   = re78*weight 
                )
        

lm( re78 ~treated, weights = weight, data = df)

# df %>% ggplot( aes( x = ps_score, y = ..density..)) +
#     geom_histogram( fill = "gray", color = "black") + facet_wrap(~ treated, scales = "free") +
#     geom_density( fill = "blue", alpha = 0.2)
# 
# 
# 
# df %>% 
#     # This creates a sub sample - which may not be a good representation
#     filter( between(ps_score, 0.1, 0.8) ) %>% 
#     ggplot( aes(x = ps_score , fill= factor(treated) ) ) +
#     geom_histogram(color = "white") + 
#     facet_wrap(~treated, scales = "free")
# 
# 
# df %>% filter( between(ps_score, 0.1, 0.8) ) %>% arrange(ps_score) %>% 
#     mutate(  ps = plyr::round_any( ps_score, 0.01, f = round)) %>% 
#     group_by( treated, ps) %>% 
#     summarise( re78 = mean(re78)) %>% 
#     arrange(ps) %>% 
#     pivot_wider( names_from = treated, values_from = re78) %>% 
#     drop_na() %>% janitor::clean_names() %>% 
#     mutate( diff = x1-x0) %>% 
#     summarise( mean = mean(diff))
    
    
# mathit    
library(MatchIt)    

LaLonde_matchIt <- LaLonde_match %>%
    select(-dwincl, -early_ra) %>%  # MatchIt does not allow missing values
    na.omit()




mod_match <- matchit(treated ~ age + I(age^2) + I(age^3) + educ  + I(educ^2) + black + married + nodegree + hisp + re74 + kidmiss + kids18 ,
                     method = "nearest", data = LaLonde_matchIt)

summary(mod_match)


mod_match %>% match.data() %>% lm( re78 ~ treated, data = .)



