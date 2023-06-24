library(sl3)
library(tmle3shift)
library(tmle3)
library(dplyr)
library(SuperLearner)
library(data.table)
library(tidyverse)
library(purrr)
library(lubridate)
library(pROC)
library(caret)
library(origami)

`%+%` <- function(a, b) paste0(a, b)

# Function: Deep cleans prescription history data
# param: DT - must include FECHA_REGISTRO, FECHA_EXPEDICION, NSS_BIN, AM_BIN, TEST_DATE
clean_data <- function(DT) {
  # Assume prescriptions filled after 3 months were never filled
  cat("Assigning NA to prescriptions with dispense date over 3 months...\n")
  DT[, GAP_TMP := as.numeric(FECHA_EXPEDICION - FECHA_REGISTRO)]
  DT[GAP_TMP < -90 | GAP_TMP > 90, FECHA_EXPEDICION := NA]
  DT[, GAP_TMP := NULL]
  
  # Switch register date and fill date if register date is after fill date
  cat("Switcing fecha registro and fecha expedicion...\n")
  DT[FECHA_REGISTRO > FECHA_EXPEDICION, `:=`(FECHA_REGISTRO_TMP = FECHA_EXPEDICION, FECHA_EXPEDICION_TMP = FECHA_REGISTRO)]
  DT[FECHA_REGISTRO > FECHA_EXPEDICION, `:=`(FECHA_REGISTRO = FECHA_REGISTRO_TMP, FECHA_EXPEDICION = FECHA_EXPEDICION_TMP)]
  DT[, `:=`(FECHA_REGISTRO_TMP = NULL, FECHA_EXPEDICION_TMP = NULL)]
  setorder(DT, ID, FECHA_EXPEDICION, FECHA_REGISTRO)
  
  # Collapse into one row if there are multiple prescriptions of the same drug picked up on the same day
  cat("Collapsing rows...\n")
  DT <- DT[, .(NO_DOCUMENTO = NO_DOCUMENTO[1], 
               FECHA_REGISTRO = FECHA_REGISTRO[1],
               ID_PRODUCTO = ID_PRODUCTO[1],
               CANTIDAD = sum(CANTIDAD),
               PRECIO_UNITARIO = PRECIO_UNITARIO[1],
               MES_REGISTRO = MES_REGISTRO[1]), by = .(ID, FECHA_EXPEDICION)]
  
  # Define fill count, max fill count for each patient
  cat("Defining fill count, max fill count...\n")
  DT[, FILL_COUNT := 1:.N, by = .(ID)]
  DT[, MAX_FILL := .N, by = .(ID)]
  
  # Define max, min fill date for each patient
  cat("Defining max, min fecha registro...\n")
  DT[, MIN_FECHA_REGISTRO := min(FECHA_REGISTRO), by = .(ID)]
  DT[, MAX_FECHA_REGISTRO := max(FECHA_REGISTRO), by = .(ID)]
  
  return(DT)
}

# Function: Computes medication adherence measure: Proportion of Days Covered (PDC)
# Param: DT - the output from clean_data()
#        pres_hist_length - length of prescription history to consider
#                           all -> all available prescription history
#                           all before positive -> all prescription history before COVID positive test
#                           a numeric value -> max number of months of prescription history before COVID positive test
PDC <- function(DT, no_gap_above, pres_hist_length = "abc", exclude_gap = -100) {
  # Total number of days to cover
  cat("Defining total days to cover...\n")
  DT[, TOTAL_DAYS := c(0, as.integer(diff(FECHA_EXPEDICION))), by = .(ID)]
  DT[is.na(TOTAL_DAYS), TOTAL_DAYS := 0]
  
  # Gap between expected fill date and observed fill date
  cat("Defining fill gaps...\n")
  DT[TOTAL_DAYS > 0, FILL_GAP := 30 - TOTAL_DAYS]
  
  # Medication count
  cat("Defining medication count...\n")
  DT[!is.na(FILL_GAP), MED_COUNT := purrr::accumulate(FILL_GAP, ~ifelse(.x + .y < 0, 0, .x + .y), .init = max(0, FILL_GAP[1]))[-1], by = .(ID)]
  
  # Number of days without medication coverage
  cat("Defining days missed...\n")
  DT[!is.na(FILL_GAP), TOTAL_DAYS_MISSED := missed_fun(FILL_GAP), by = .(ID)]
  DT[FILL_GAP < -no_gap_above, TOTAL_DAYS := NA]
  DT[FILL_GAP < -no_gap_above, TOTAL_DAYS_MISSED := NA]
  
  # Filter based on specified prescription history length
  if (pres_hist_length == "all before positive") {
    DT <- DT[FECHA_REGISTRO <= TEST_DATE, ]
  } else if (pres_hist_length == "most recent") {
    DT <- DT[FECHA_REGISTRO <= TEST_DATE, ]
    DT[, START_PDC_CALC := tail(which(FILL_GAP < exclude_gap), 1), by = .(ID)]
    DT <- DT[FILL_COUNT > START_PDC_CALC + 1, ]
  } else if (is.numeric(pres_hist_length)) {
    DT <- DT[FECHA_REGISTRO >= TEST_DATE %m-% months(pres_hist_length) | FECHA_EXPEDICION >= TEST_DATE %m-% months(pres_hist_length), ]
  }
  
  # Define PDC
  cat("Defining PDC...\n")
  DT[, PDC := (sum(TOTAL_DAYS, na.rm = T) - sum(TOTAL_DAYS_MISSED, na.rm = T)) / sum(TOTAL_DAYS, na.rm = T), by = .(ID)]
  DT[PDC > 1, PDC := 1]

  return(DT)
}

PDC_2 <- function(DT, pres_hist_length = "abc", exclude_gap = -100) {
  # Total number of days to cover
  cat("Defining total days to cover...\n")
  DT[, TOTAL_DAYS := c(0, as.integer(diff(FECHA_EXPEDICION))), by = .(ID)]
  DT[is.na(TOTAL_DAYS), TOTAL_DAYS := 0]
  
  # Gap between expected fill date and observed fill date
  cat("Defining fill gaps...\n")
  DT[TOTAL_DAYS > 0, FILL_GAP := 30 - TOTAL_DAYS]
  
  # Medication count
  cat("Defining medication count...\n")
  DT[!is.na(FILL_GAP), MED_COUNT := purrr::accumulate(FILL_GAP, ~ifelse(.x + .y < 0, 0, .x + .y), .init = max(0, FILL_GAP[1]))[-1], by = .(ID)]
  
  # Number of days without medication coverage
  cat("Defining days missed...\n")
  DT[!is.na(FILL_GAP), TOTAL_DAYS_MISSED := missed_fun(FILL_GAP), by = .(ID)]
  DT[FILL_GAP < -60, TOTAL_DAYS := NA]
  DT[FILL_GAP < -60, TOTAL_DAYS_MISSED := NA]
  
  # Filter based on specified prescription history length
  if (pres_hist_length == "all before positive") {
    DT <- DT[FECHA_REGISTRO <= TEST_DATE, ]
  } else if (pres_hist_length == "most recent") {
    DT <- DT[FECHA_REGISTRO <= TEST_DATE, ]
    DT[, START_PDC_CALC := tail(which(FILL_GAP < exclude_gap), 1), by = .(ID)]
    DT <- DT[FILL_COUNT > START_PDC_CALC + 1, ]
  } else if (is.numeric(pres_hist_length)) {
    DT <- DT[FECHA_REGISTRO >= TEST_DATE %m-% months(pres_hist_length) | FECHA_EXPEDICION >= TEST_DATE %m-% months(pres_hist_length), ]
  }
  
  # Define PDC
  cat("Defining PDC...\n")
  DT[, PDC := (sum(TOTAL_DAYS, na.rm = T) - sum(TOTAL_DAYS_MISSED, na.rm = T)) / sum(TOTAL_DAYS, na.rm = T), by = .(ID)]
  #DT[PDC > 1, PDC := 1]
  
  return(DT)
}

# Function: 
# Required data: SNV_metformin, SNV_pred_adherence
prep_data <- function(fname, pres_hist_length, no_gap_above = 60) {
  DT <- clean_data(SNV_metformin)
  DT_TEST_DATE <- SNV_pred_adherence[, .(ID, TEST_DATE)]
  
  # Append COVID test date to prescription data
  DT <- merge(DT, DT_TEST_DATE, all.x = TRUE, by = "ID")
  DT <- DT[!is.na(TEST_DATE), ]
  
  # Calculate PDC (all prescriptions before COVID positive test) -----------------
  DT <- PDC(DT, no_gap_above = no_gap_above, pres_hist_length = pres_hist_length)
  DT[, MAX_FILL_BEFORE_POS := .N, by = .(ID)]
  
  ## Patients on Metformin
  DT_all <- data.table()
  if (pres_hist_length == "most recent") {
    DT_all <- DT[FILL_COUNT == START_PDC_CALC + 2 & !is.na(PDC), ]
  } else {
    DT_all <- DT[FILL_COUNT == 2 & !is.na(PDC), ]
  }
  DT_all <- merge(SNV_pred_adherence[, !"TEST_DATE", with = FALSE], DT_all, by = "ID", all = FALSE)
  DT_all <- DT_all[, .(ID, SEX, AGE, INDIGENOUS, MAX_FILL_BEFORE_POS, 
                       IMSS_STATE, AVG_SCHOOLING_DEGREE, PRO_DISABLED_POP, PRO_IMSS_POP, PRO_ILLITERATE_POP , AVG_OCCUPANT_PER_PRIVATE_HOME,
                       COPD, DIABETES, ASTHMA, IMMUNOSUPRESSION, SMOKING, OBESITY,
                       HIV, HYPERTENSION, CARDIOVASCULAR, CHRONIC_LIVER_DISEASE, HEMOLYTIC_ANEMIA, 
                       NEOROLOGICAL, TUBERCULOSIS, CANCER, RENAL, NUM_VAX, WAVE, ON_INSULIN, PDC, DEAD)]
  DT <- DT_all
  
  save(DT, file = "data/" %+% fname %+% ".RData")
}

prep_data_2 <- function(fname, pres_hist_length) {
  DT <- clean_data(SNV_metformin)
  DT_TEST_DATE <- SNV_pred_adherence[, .(ID, TEST_DATE)]
  
  # Append COVID test date to prescription data
  DT <- merge(DT, DT_TEST_DATE, all.x = TRUE, by = "ID")
  DT <- DT[!is.na(TEST_DATE), ]
  
  # Calculate PDC (all prescriptions before COVID positive test) -----------------
  DT <- PDC_2(DT, pres_hist_length = pres_hist_length)
  DT[, MAX_FILL_BEFORE_POS := .N, by = .(ID)]
  
  ## Patients on Metformin
  DT_all <- data.table()
  if (pres_hist_length == "most recent") {
    DT_all <- DT[FILL_COUNT == START_PDC_CALC + 2 & !is.na(PDC), ]
  } else {
    DT_all <- DT[FILL_COUNT == 2 & !is.na(PDC), ]
  }
  DT_all <- merge(SNV_pred_adherence[, !"TEST_DATE", with = FALSE], DT_all, by = "ID", all = FALSE)
  DT_all <- DT_all[, .(ID, SEX, AGE, INDIGENOUS, MAX_FILL_BEFORE_POS, 
                       IMSS_STATE, AVG_SCHOOLING_DEGREE, PRO_DISABLED_POP, PRO_IMSS_POP, PRO_ILLITERATE_POP , AVG_OCCUPANT_PER_PRIVATE_HOME,
                       COPD, DIABETES, ASTHMA, IMMUNOSUPRESSION, SMOKING, OBESITY,
                       HIV, HYPERTENSION, CARDIOVASCULAR, CHRONIC_LIVER_DISEASE, HEMOLYTIC_ANEMIA, 
                       NEOROLOGICAL, TUBERCULOSIS, CANCER, RENAL, NUM_VAX, WAVE, ON_INSULIN, PDC, DEAD)]
  DT <- DT_all
  
  save(DT, file = "data/" %+% fname %+% ".RData")
}

prep_data_pos <- function(fname) {
  DT <- clean_data(SNV_metformin)

  # Calculate PDC (all prescriptions before COVID positive test) -----------------
  DT <- PDC(DT)
  DT[, MAX_FILL_BEFORE_POS := .N, by = .(ID)]
  
  ## Patients on Metformin
  DT_all <- data.table()
  DT_all <- DT[FILL_COUNT == 2 & !is.na(PDC), ]

  DT_all <- merge(SNV_pred_positivity, DT_all, by = "ID", all = FALSE)
  DT_all <- DT_all[, .(ID, SEX, AGE, INDIGENOUS, MAX_FILL_BEFORE_POS, 
                       IMSS_STATE, AVG_SCHOOLING_DEGREE, PRO_DISABLED_POP, PRO_IMSS_POP, PRO_ILLITERATE_POP , AVG_OCCUPANT_PER_PRIVATE_HOME,
                       COPD, DIABETES, ASTHMA, IMMUNOSUPRESSION, SMOKING, OBESITY,
                       HIV, HYPERTENSION, CARDIOVASCULAR, CHRONIC_LIVER_DISEASE, HEMOLYTIC_ANEMIA, 
                       NEOROLOGICAL, TUBERCULOSIS, CANCER, RENAL, NUM_VAX, WAVE, PDC, POS)]
  DT <- DT_all
  
  save(DT, file = "data/" %+% fname %+% ".RData")
}

missed_fun <- function(x) {
  out <- c()
  tmp <- 0
  for (i in seq_len(length(x))) {
    tmp <- tmp + x[i]
    out <- c(out, ifelse(tmp < 0, abs(tmp), 0))
    tmp <- max(0, tmp)
  }
  return(out)
}
