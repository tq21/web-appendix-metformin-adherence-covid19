#' 003_tmle_shift_intervention.R
#' 
#' Run TMLE shift intervention analysis
#' 
#' Required data:
#' - SNV_MET_ALL_BEFORE_POSITIVE.RData

library(tmle3)
library(sl3)
library(SuperLearner)
library(origami)
library(data.table)
library(dplyr)
source("helper_functions.R")
source("helper_functions_003.R")

options(sl3.verbose = TRUE)

# OVERALL
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "OVERALL_0.05", 0.05, c(1, 2, 3))
gc()
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "OVERALL_0.1", 0.1, c(1, 2, 3))
gc()

# WAVE 1
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "WAVE_1_0.05", 0.05, 1)
gc()
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "WAVE_1_0.1", 0.1, 1)
gc()

# WAVE 2
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "WAVE_2_0.05", 0.05, 2)
gc()
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "WAVE_2_0.1", 0.1, 2)
gc()

# WAVE 3
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "WAVE_3_0.05", 0.05, 3)
gc()
TMLE_shift_custom("data/SNV_MET_ALL_BEFORE_POSITIVE.RData", "WAVE_3_0.1", 0.1, 3)
gc()
