## Preamble Load Packages set options etc.

rm(list=ls())
library("texreg")
library("stargazer")
library(pscl)
library(knitr)
library(kableExtra)
library("MASS")     # for negative binomial count model
library(magrittr)
library(apollo)
library(haven)
library(modelsummary)
library(gt)
library(haven)
library(readxl)
library(DescTools)
library(stringr)
library("dplyr")
library(tidylog)

select<-dplyr::select ## make sure dplyrs select is used and not the one from MASS



draws=1000    ## set draws for mixed logit model