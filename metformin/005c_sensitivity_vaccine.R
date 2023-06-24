#' 005c_sensitivity_vaccine.R
#' 
#' Run TMLE shift intervention analysis, stratified on dosages of vaccines received
#' 
#' Required data:
#' - SNV_MET_ALL_BEFORE_POSITIVE.RData

library(tmle3)
library(data.table)
library(sl3)
library(SuperLearner)
library(origami)
library(dplyr)
source("helper_functions.R")
source("helper_functions_005c.R")

options(sl3.verbose = TRUE)

# NUM_VAX = 0
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "NUM_VAX_0_0.05", 0.05, 0)
gc()
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "NUM_VAX_0_0.1", 0.1, 0)
gc()

# NUM_VAX = 1 or 2
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "NUM_VAX_1_OR_2_0.05", 0.05, c(1, 2))
gc()
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "NUM_VAX_1_OR_2_0.1", 0.1, c(1, 2))
gc()
