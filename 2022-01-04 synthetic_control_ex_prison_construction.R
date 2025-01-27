

# Causal inference method: Synthetic control
# Example from: https: //mixtape.scunning.com/synthetic-control.html 
# R-code from also from: //mixtape.scunning.com/synthetic-control.html 

library(tidyverse)

# 1980, Texas Dep. of Corrections lost a civil action lawsuit.


df <- haven::read_dta("https://raw.github.com/scunning1975/mixtape/master/texas.dta")

# names / label
df_label <- tibble( name = names(df),
        label = map_chr( df, function(x){
            ifelse( !is.null(attr(x, "label")), attr(x, "label"), "NULL")})
)

df %>% skimr::skim()

library(SCtools)
library(future)
library(Synth)

texas  <- df %>% as.data.frame()

dataprep_out <- dataprep(
    foo = texas,
    predictors = c("poverty", "income"),
    predictors.op = "mean",
    time.predictors.prior = 1985:1993,
    special.predictors = list(
        list("bmprison", c(1988, 1990:1992), "mean"),
        list("alcohol", 1990, "mean"),
        list("aidscapita", 1990:1991, "mean"),
        list("black", 1990:1992, "mean"),
        list("perc1519", 1990, "mean")),
    dependent = "bmprison",
    unit.variable = "statefip",
    unit.names.variable = "state",
    time.variable = "year",
    treatment.identifier = 48,
    controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),
    time.optimize.ssr = 1985:1993,
    time.plot = 1985:2000
)

synth_out <- synth(data.prep.obj = dataprep_out)

path.plot(synth_out, dataprep_out)