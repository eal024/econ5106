

# Seminar ML for Microeconometrics
# Monte Carl- OLS vs. Naive Lasso vs. Double Secetion Lasso

# https://skranz.github.io/r/2020/09/14/LassoCausality.html
# Belloni, Chernozhukov and Hansen (2014) introduces a post double selection method where one first runs two lasso regressions to select suitable control variables for a final OLS regression.
# This blog post first introduces the concepts on an intuitive level and illustrates cases where post double selection works very well.

# First part: first introduces the concepts on an intuitive level and illustrates cases where post double selection works very well. 
# Second part:  I intuitively discuss and simulate cases where post-double selection fails while variable selection with a simple lasso regression works pretty well.

# libr
library(tidyverse)
#source("~/R-prosjekter/econ5106/2022-02-01 run_lasso_simulation.R")
source("~/R-prosjekter/econ5106/2022-02-04 helper_sim.R")

# Motivation --------------------------------------------------------------

# DGP
n     <- 200               # number obs.
xc    <- rnorm(n, 0, 1)    # confounders
d     <- xc + rnorm(n,0,1) # Endogen variable
alpha <- 1  # Coef
beta  <- 1  

# depentent var Y
y <- alpha*d + beta*xc + rnorm(n, 0, 1) 

# OLS - without control confounder (x that both explain y and d)
# Bias
coef( lm( y ~ d)) [2]

# Correct:
coef( lm( y ~ d + xc)) [2]



# Selection of X`s with Lasso ---------------------------------------------

# In general: In empirical studies, the main problem is typically that we just don’t have data for all relevant confounders
# Few n, but many of X (predictors). Example growth models
# But if we don’t have enough observations, we need some method to select the relevant control variables

# DGP (y and d)
#' x_k confounders: effect y and d. Not include -> bias estimate of d
#' x_y only effect y,               not include -> no bias if exlude          
#' x_e effect only d, sorce of exogen var. can estimate D from x_e, reduce the bias in main equation.
#' x_u does not effect y or d. noise, idealy leave out


# Helper: source("~/R-prosjekter/econ5106/2022-02-04 helper_sim.R")
# Simulate data.  1000 obs, 50 x, of each variable
# mat <- sim_data( n = 1000, k = 50*4, noise_e =  0)

# Create the data
# N = 1000, Kc = 50 (confounders), Ky = 50 (affect only y), x_e = 1 source of exogen. var, k_u = 1000
set.seed(1)
mat <- lasso_sim(alpha=1, n=1000,Kc=50,Ke=1,Ky=50,Ku=1000,return.what = "data")

#
dim(mat)
mat[1:3, 1:5]


# 1. OLS Three regression w. OLS ------------------------------------------

#
y <- mat[, 1]
X <- cbind(1, mat[,-1]); colnames(X)[1] = "const"

# Look at the data
as_tibble(cbind(y,X)) %>% head()

# Regression 1: gives non-sensible result
# Because we have too many variables
coef(lm.fit(y = y, x = X))[2]
summary(lm(y ~ 1+ X))$coefficients[2]

# Regression 2: Short regression gives positiv bias, due to OVB (confoundings, x_k)
# OVB
summary(lm(y ~ 1+ X[,1:2]))$coefficients[2]

# Reg. 3: Controloling for all confounders -> No bias. Alpha = 1
head(as_tibble(mat))
coef(lm.fit(y = y, x = X[,1:52]))[2]
summary(lm(y ~ 1+ X[,1:52]))$coefficients[2]
summary(lm(y ~ 1 + ., data = as_tibble(mat) %>% select(1:52) )) $coefficients[2]

# If we know all x_c (confounders), we can easly consistely estimate the causal effect a = 1


# 2. Lasso, double selection ----------------------------------------------------------------

# But let’s assume we don’t know which of the 1101 potential control variables are the confounders.

# Lasso: coefficients of the selected variables will be typically attenuated towards 0 because of the penalty term. 

# Post-lasso: Post-lasso estimator avoids this attenuation by simply performing an OLS estimation using all the selected variables from the lasso estimation.

# First lasso (y~x), than use the seleceted var and do a lm( y~d selected)

# gamlr::
# The gamlr function uses by default the corrected Akaike Information Criterion (AIC) to select the penalty parameter λ.
# This avoids the random fluctuation from cross validation (used in the popular cv.glmnet function) and makes the gamlr function blazingly fast.
library(gamlr)

lasso <- gamlr(x = X, y = y)

# lasso_coef
is(lasso, "gamlr")
head(as.matrix(coef(lasso)))

co   <- as.matrix(coef(lasso))
rows <- co[,1] != 0
rows[1] = F  #keep.intercept
head(rows,5)
co <- co[rows,]

# Coeffisient from Lasso (pacakges gamlr)
lasso_coef(lasso, keep.intercept = F )

# The coef to d is still bias
lasso_coef(lasso, keep.intercept = F )[1]


# 2) Post-lasso -> result: still bias

# Choice of variables lasso:
coefs   <- lasso_coef(lasso, keep.intercept = F)
vars    <- names(coefs)

# Problem: To many xe are selected. To few of xc
vars

f <- str_c("y ", "~", str_c(vars, collapse = "+")) 
post_lasso <- lm( formula = f  , data = cbind(y = y ,X[,c("const", vars)]) %>% as_tibble() )

# Post Lasso: Give still a bias result
coef(post_lasso)[2]

# Bias result about the same as OLS (Short)

# Understand the - look at the selected variables:
vars_counts(vars)

tibble(vars = vars) %>% 
    mutate( c = case_when( str_detect(vars,"c") ~"xc",
                           str_detect(vars,"e") ~"xe",
                           str_detect(vars,"u") ~"xu",
                           vars == "y" ~"y",
                           T ~"xy"
                           )) %>% 
    group_by(c) %>% count() %>% pivot_wider(names_from = c, values_from = n)


# Lasso only select 2 of the cofoundings variables. Because d capture some effect of the confounders on y
# thus xc is less importan for prediction of y, and not selected


## Alternative approach: 
    # i)  lasso: d ~ x (all controls)
    # ii) lasso: y ~ x (exluded d) 
    # iii) union of selected x from (i) and (ii) 
    # Intuion: xc affect d and y. To make sure few confounders are omitted. Use all x that are relevant affect d or y

# i) d ~ x (all controls)
d <- mat[,2]
lasso_d   <- gamlr(y = d, x = X[,-2] )
# ii) y ~ x (exluded d) 
lasso_yx <-  gamlr(y = y, x = X[,-2] )
# select var from i and ii
    # co   <- as.matrix(coef(lasso_d))
    # rows <- co[,1] != 0
    # rows[1] = F  #keep.intercept
    # head(rows,5)
    # co <- co[rows,]
    # names(co)
var1 <- names(lasso_coef(lasso_d) )
var2 <- names(lasso_coef(lasso_yx))

# union var (#setdiff(var1,var2))
vars <- union(var1, var2)

f_post_lasso2 <- str_c("y ~ d + ", str_c(vars, collapse = "+") )

post_lasso <- lm( formula =  f_post_lasso2, data = as_tibble(mat) )

# Close to 1 (near unbiased)
coef(post_lasso)[2]

vars_counts(var1) # From d ~ (eculded y, all x)
vars_counts(var2) # from y ~ (exluded d)
vars_counts(vars) # union y and d

# post-lasso: Select all xc, 
# When not including d, more likely to include all cofounders 


# Selection of landa and packages hdm: Double selection -------------------------------------------------


library(hdm)
mat <- lasso_sim(alpha=1, n=1000,Kc=50,Ke=1,Ky=50,Ku=1000,return.what = "data")
y <- mat[,1]
X <- cbind(1,mat[,-1]); colnames(X)[1] = "const"
d <- mat[,2]
# post double selection

# 1. rund two lasso:
lasso_dx   <- rlasso(y = d, x = X[,-2])
lasso_yx2  <- rlasso(y = y, x = X[,-2])

# 2. comupte union
vars1 <- names(lasso_coef(lasso_dx))
vars2 <- names(lasso_coef(lasso_yx2))
vars_u <- union(vars1, vars2) 

vars_u

# 3. OLS with union X and d included
f_post2 <- str_c( "y ~ d + ", str_c(vars_u, collapse = "+"))
post_lasso <- lm( f_post2, data = as_tibble(mat))

coef(post_lasso)[2]
vars_counts(vars_u) # All xc are selected
# Only 12 unrelated variables are selected 


## Function for directly double selection method:
rlassoEffect( x = X[,-2], d = d, y = y, method = "double selection")

# Coefficients of interest: d #  1.013

# Alternative to double selection: Anatomy and the FWL theorem

rlassoEffect(x=X[,-2],d=d,y=y,method="partialling out")

# While the estimated α^ differs slightly, 
# in all simulations where I checked either both approaches seemed to worked well or both failed.




# 3. Cases were DoubleS-lasso fails, single lasso works well --------------

# Cases were lasso on y works better than DS lasso.

set.seed(1)

mat <- lasso_sim( alpha = 1, n = 700, Kc = 50, Ke = 50, Ky = 50, Ku = 700, return.what = "data")

head(mat %>% as_tibble())
y <- mat[,1]
d <- mat[,2]
X <- cbind(1,mat[,-1]);colnames(X)[1] = "const"

# short OLS: d is bias
# head(mat[,1:2])
coef( lm( y ~ 1 +  d , data = as_tibble(mat[,1:2] ) ))[2]

## Compare two linaer regression



# 1. control for 49 of 50 cofounders
mat <- as_tibble(mat)
names(mat)[3:52]
names(mat)[53:102]
# 1.
ex_4 <- str_c("y ~ d + ", str_c( names(mat)[3:51], collapse = "+" ))
coef(lm( formula = ex_4, data = as_tibble(mat)))[2] # Near 1


# 2. controling for 49/50 xc and xe (all exogen variation)
# xek is one of Ke variables that only affect d but not through any other channel the dependent variable  y
ex_5 <- str_c("y ~ d + ", str_c( names(mat)[ c(3:51,53:102)] , collapse = "+" ))
coef(lm( formula = ex_5, data = as_tibble(mat)))[2] # Bias

# If we don’t add any observable source of exogenous variation, 
# we find an estimate pretty close to the true causal effect α=1,
# but if we control for the exogenous variation our estimate looks substantially biased.



# Simulated many times ----------------------------------------------------

lasso_sim( alpha = 1, n = 700, Kc = 50, Ke = 50, Ky = 50, Ku = 700, return.what = "data")


df <- tibble( id = 1:1000) %>% 
    mutate( data = map(id, ~.x %>% lasso_sim( alpha = 1, n = 700, Kc = 50, Ke = 50, Ky = 50, Ku = 700, return.what = "data") ) 
            )


# Sim models
mat <- as_tibble(mat)
model1_only_xc    <- ex_4 <- str_c("y ~ d + ", str_c( names(mat)[3:51], collapse = "+" ))
model2_only_xc_xe <- ex_5 <- str_c("y ~ d + ", str_c( names(mat)[ c(3:51,53:102)] , collapse = "+" ))
    
df_sim_mpodel <- df %>% 
    mutate( model1_xc = map(data, function(x) coef( lm( formula = model1_only_xc, data = as_tibble(x) ) )[2]  ),
            model2_xc_xe = map(data, function(x) coef( lm( formula = model2_only_xc_xe, data = as_tibble(x) ))[2])
            )
    

df_sim_mpodel %>%
    select(id, matches("model")) %>% 
    unnest(cols = c(model1_xc, model2_xc_xe ) ) %>% 
    pivot_longer( -id) %>% 
    ggplot( aes( x = value, fill= name)) +
    geom_vline( xintercept =  1)  +
    geom_histogram( alpha = 0.5, aes( y = ..density.. ) ) + facet_wrap( ~name)



# Conclusion, last remark -------------------------------------------------

# coef(d) bias if control for xc and xe
# also with high standard errors

# If not control xe, but xc - > low bias, low se

# Post double lasso; need to exclude relevant source of exogen variation (d~xe)
# Do lasso (d~x, and y~x), and then union(vars1, vars2) -> less likely to select source of exogenous variation 


# The lasso regression of d on all potential control variables is likely to select the sources of exogenous variation, which we don’t want to use as control variables.

#
set.seed(1)
double_sel <- rlassoEffect(x=X[,-2],y=y,d=d, method="double selection")
coef(double_sel)
confint(double_sel,level = 0.999)

simple_lasso = gamlr(x=X,y=y)
post_lasso_coef(simple_lasso,x=X,y=y,keep.intercept = FALSE)[1]

# Simple gamlr_simple vs. rlasso_double_sel
set.seed(1)
# Only 7 of xc are included.Thats why rlasso_double_sel dont work
lasso_sim(alpha=1,n=700,Kc=50,Ke=50,Ky=50,Ku=700, 
          models=c("gamlr_simple","rlasso_double_sel")
          )


# 4. Are confounders that strongly affect y worse than those that affect d? ------
















