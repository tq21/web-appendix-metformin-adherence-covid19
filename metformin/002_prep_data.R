#' 002_prep_data.R
#' 
#' Prepare data for TMLE analysis
#' 
#' Required data:
#' - SNV_metformin.RData
#' - SNV_pred_adherence.RData

library(data.table)
source("helper_functions.R")

# Prepare dataset (PDC calculated from all prescription history before positive)
load("data/SNV_metformin.RData")
load("data/SNV_pred_adherence.RData")
prep_data("SNV_MET_ALL_BEFORE_POSITIVE", "all before positive")

# PDC on positive test result
load("data/SNV_pred_positivity.RData")
prep_data_pos("SNV_MET_ALL_BEFORE_POSITIVE_POS")
