#' 001_sql_get_data.R
#' 
#' Query relevant data from the IMSS database, save as .RData files

source("sql_connect.R") # load credentials for SQL
source("helper_functions.R")

library(data.table)
library(lubridate)

# For each medication, we need to get 2 datasets from the database:
# DATASET 1: prescriptions related to the medication
# DATASET 2: baseline covariates of the patients in DATASET 1

# DATASET 1 --------------------------------------------------------------------
SNV_metformin <- sqlQuery(
  odbconexion,
  "SELECT *
   FROM [IMSS Sandbox].[dbo].[SNV_FARMACIA_METFORMIN]")
setDT(SNV_metformin)

# Enforce data types, minor cleaning, save
SNV_metformin[, NSS_BIN := NULL]
SNV_metformin[, AM_BIN := NULL]
SNV_metformin[, NO_DOCUMENTO := trimws(NO_DOCUMENTO)]
SNV_metformin[, FECHA_REGISTRO := ymd(FECHA_REGISTRO)]
SNV_metformin[, FECHA_EXPEDICION := ymd(FECHA_EXPEDICION)]
setkeyv(SNV_metformin, "ID")
save(SNV_metformin, file = "data/SNV_metformin.RData")

# DATASET 2 --------------------------------------------------------------------
# criteria: history of metformin prescription
#           tested positive for COVID
SNV_pred_adherence <- sqlQuery(
  odbconexion,
  "SELECT SNV_ID.ID, 
          FECHA_MUESTRA_CONF1,
          FECHA_DEFUNCION,
          SEXO,
          EDAD_ANO,
          DESC_DELEGACION,
          PCON_DISC,
          GRAPROES,
          P15YM_AN,
          PDER_IMSS,
          SE_RECONOCE_INDIGENA,
          PRO_OCUP_C,
          POBTOT,
          ANTECED_EPOC,
          ANTECED_DIABETES,
          ANTECED_ASMA,
          ANTECED_INMUNOSUPRESION,
          ANTECED_TABAQUISMO,
          ANTECED_OBESIDAD,
          ANTECED_VIH_EVIH,
          ANTECED_HIPERTENSION,
          ANTECED_CARDIOVASCULAR,
          ANT_ENF_HEPATICA_CRONICA,
          ANT_ANEMIA_HEMOLITICA,
          ANT_ENF_NEUROLOGICA,
          ANTECED_TUBERCULOSIS,
          ANTECED_CANCER,
          ANTECED_RENAL,
          DOSIS_VAC_COVID19,
          Oleada
   FROM [IMSS Sandbox].[dbo].[sinolave_areavars] AS SNV,
        [IMSS Sandbox].[dbo].[SNV_FARMACIA_METFORMIN_ID] AS SNV_ID
   WHERE SNV.NSS_BIN = SNV_ID.NSS_BIN AND
         SNV.AM_BIN = SNV_ID.AM_BIN AND
         SNV.DESC_ESTATUS_CONF1 = 'VALIDADA' AND 
         SNV.DESC_RESULTADO_CONF1 = 'POSITIVO'"
)
setDT(SNV_pred_adherence)

SNV_pred_adherence_dict <- c(
  "ID", "TEST_DATE", "DEATH_DATE", "SEX", "AGE", "IMSS_STATE",
  "DISABLED_POP", "AVG_SCHOOLING_DEGREE", "ILLITERATE_POP", "IMSS_POP", "INDIGENOUS", "AVG_OCCUPANT_PER_PRIVATE_HOME","TOT_POP", 
  "COPD", "DIABETES", "ASTHMA", "IMMUNOSUPRESSION", "SMOKING", "OBESITY",
  "HIV", "HYPERTENSION", "CARDIOVASCULAR", "CHRONIC_LIVER_DISEASE", "HEMOLYTIC_ANEMIA", 
  "NEOROLOGICAL", "TUBERCULOSIS", "CANCER", "RENAL", "NUM_VAX", "WAVE"
)

# Enforce data types, minor cleaning, save
colnames(SNV_pred_adherence) <- SNV_pred_adherence_dict
SNV_pred_adherence[, TEST_DATE := ymd_hms(TEST_DATE)]
SNV_pred_adherence[, DEATH_DATE := ymd_hms(DEATH_DATE)]
SNV_pred_adherence[DEATH_DATE < "2000-01-01", DEATH_DATE := NA]
setorder(SNV_pred_adherence, ID, TEST_DATE)
SNV_pred_adherence[, COUNT := .N:1, by = .(ID)]
SNV_pred_adherence[, MAX_COUNT := .N, by = .(ID)]
SNV_pred_adherence <- SNV_pred_adherence[COUNT == 1, ] # most recent test
SNV_pred_adherence[, DEAD := 0]
SNV_pred_adherence[!is.na(DEATH_DATE), DEAD := 1]

# Define proportions
SNV_pred_adherence[, PRO_DISABLED_POP := DISABLED_POP / TOT_POP]
SNV_pred_adherence[, PRO_IMSS_POP := IMSS_POP / TOT_POP]
SNV_pred_adherence[, PRO_ILLITERATE_POP := ILLITERATE_POP / TOT_POP]

# Remove NA preexisting conditions
SNV_pred_adherence <- SNV_pred_adherence[COPD != 2 &
                                         DIABETES != 2 &
                                         ASTHMA != 2 &
                                         IMMUNOSUPRESSION != 2 &
                                         SMOKING != 2 &
                                         OBESITY != 2 &
                                         HIV != 2 &
                                         HYPERTENSION != 2 &
                                         CARDIOVASCULAR != 2 &
                                         CHRONIC_LIVER_DISEASE != 2 &
                                         HEMOLYTIC_ANEMIA != 2 &
                                         NEOROLOGICAL != 2 &
                                         TUBERCULOSIS != 2 &
                                         CANCER != 2 &
                                         RENAL != 2]

# add insulin indicator
load("../insulin/data/SNV_insulin.RData")

DT <- clean_data(SNV_metformin)
DT_TEST_DATE <- SNV_pred_adherence[, .(ID, TEST_DATE)]

# Append COVID test date to prescription data
DT <- merge(DT, DT_TEST_DATE, all.x = TRUE, by = "ID")
DT <- DT[!is.na(TEST_DATE), ]

# get all insulin prescription documentation numbers
insulin_doc_id <- unique(SNV_insulin$NO_DOCUMENTO)

# only get the most recent prescription document number prior to the positive test date
setorderv(DT, c("ID", "FECHA_REGISTRO"))
DT[, IS_PRIOR_POS := as.numeric(FECHA_REGISTRO <= TEST_DATE)]
DT[IS_PRIOR_POS == 1, MOST_RECENT_DATE := max(FECHA_REGISTRO), by = ID]
DT[FILL_COUNT == 1 & !is.na(MOST_RECENT_DATE), ON_INSULIN := as.numeric(NO_DOCUMENTO %in% insulin_doc_id)]
ID_ON_INSULIN <- DT[ON_INSULIN == 1, ID]

SNV_pred_adherence[, ON_INSULIN := 0]
SNV_pred_adherence[ID %in% ID_ON_INSULIN, ON_INSULIN := 1]

# Factor columns
factors <- c(
  "SEX", "IMSS_STATE", "INDIGENOUS", 
  "COPD", "DIABETES", "ASTHMA", "IMMUNOSUPRESSION", "SMOKING", "OBESITY",
  "HIV", "HYPERTENSION", "CARDIOVASCULAR", "CHRONIC_LIVER_DISEASE", "HEMOLYTIC_ANEMIA", 
  "NEOROLOGICAL", "TUBERCULOSIS", "CANCER", "RENAL", "NUM_VAX", "WAVE", "ON_INSULIN", "DEAD")
SNV_pred_adherence[, (factors) := lapply(.SD, as.factor), .SDcols = factors]

save(SNV_pred_adherence, file = "data/SNV_pred_adherence.RData")

SNV_pred_positivity <- sqlQuery(
  odbconexion,
  "SELECT SNV_ID.ID, 
          DESC_ESTATUS_CONF1,
          DESC_RESULTADO_CONF1,
          SEXO,
          EDAD_ANO,
          DESC_DELEGACION,
          PCON_DISC,
          GRAPROES,
          P15YM_AN,
          PDER_IMSS,
          SE_RECONOCE_INDIGENA,
          PRO_OCUP_C,
          POBTOT,
          ANTECED_EPOC,
          ANTECED_DIABETES,
          ANTECED_ASMA,
          ANTECED_INMUNOSUPRESION,
          ANTECED_TABAQUISMO,
          ANTECED_OBESIDAD,
          ANTECED_VIH_EVIH,
          ANTECED_HIPERTENSION,
          ANTECED_CARDIOVASCULAR,
          ANT_ENF_HEPATICA_CRONICA,
          ANT_ANEMIA_HEMOLITICA,
          ANT_ENF_NEUROLOGICA,
          ANTECED_TUBERCULOSIS,
          ANTECED_CANCER,
          ANTECED_RENAL,
          DOSIS_VAC_COVID19,
          Oleada
   FROM [IMSS Sandbox].[dbo].[sinolave_areavars] AS SNV,
        [IMSS Sandbox].[dbo].[SNV_FARMACIA_METFORMIN_ID] AS SNV_ID
   WHERE SNV.NSS_BIN = SNV_ID.NSS_BIN AND
         SNV.AM_BIN = SNV_ID.AM_BIN"
)
setDT(SNV_pred_positivity)

SNV_pred_positivity_dict <- c(
  "ID", "TEST_STATUS", "TEST_RESULT", "SEX", "AGE", "IMSS_STATE",
  "DISABLED_POP", "AVG_SCHOOLING_DEGREE", "ILLITERATE_POP", "IMSS_POP", "INDIGENOUS", "AVG_OCCUPANT_PER_PRIVATE_HOME","TOT_POP", 
  "COPD", "DIABETES", "ASTHMA", "IMMUNOSUPRESSION", "SMOKING", "OBESITY",
  "HIV", "HYPERTENSION", "CARDIOVASCULAR", "CHRONIC_LIVER_DISEASE", "HEMOLYTIC_ANEMIA", 
  "NEOROLOGICAL", "TUBERCULOSIS", "CANCER", "RENAL", "NUM_VAX", "WAVE"
)

# Enforce data types, minor cleaning, save
colnames(SNV_pred_positivity) <- SNV_pred_positivity_dict
setorder(SNV_pred_positivity, ID)
SNV_pred_positivity[, TEST_STATUS := trimws(TEST_STATUS)]
SNV_pred_positivity[, TEST_RESULT := trimws(TEST_RESULT)]
SNV_pred_positivity[, POS := as.integer(TEST_STATUS == "VALIDADA" & TEST_RESULT == "POSITIVO")]
SNV_pred_positivity[, COUNT := .N:1, by = .(ID)]
SNV_pred_positivity[, MAX_COUNT := .N, by = .(ID)]
SNV_pred_positivity <- SNV_pred_positivity[COUNT == 1, ] # most recent test

# Define proportions
SNV_pred_positivity[, PRO_DISABLED_POP := DISABLED_POP / TOT_POP]
SNV_pred_positivity[, PRO_IMSS_POP := IMSS_POP / TOT_POP]
SNV_pred_positivity[, PRO_ILLITERATE_POP := ILLITERATE_POP / TOT_POP]

# Remove NA preexisting conditions
SNV_pred_positivity <- SNV_pred_positivity[COPD != 2 &
                                           DIABETES != 2 &
                                           ASTHMA != 2 &
                                           IMMUNOSUPRESSION != 2 &
                                           SMOKING != 2 &
                                           OBESITY != 2 &
                                           HIV != 2 &
                                           HYPERTENSION != 2 &
                                           CARDIOVASCULAR != 2 &
                                           CHRONIC_LIVER_DISEASE != 2 &
                                           HEMOLYTIC_ANEMIA != 2 &
                                           NEOROLOGICAL != 2 &
                                           TUBERCULOSIS != 2 &
                                           CANCER != 2 &
                                           RENAL != 2]

# Factor columns
factors <- c(
  "SEX", "IMSS_STATE", "INDIGENOUS", 
  "COPD", "DIABETES", "ASTHMA", "IMMUNOSUPRESSION", "SMOKING", "OBESITY",
  "HIV", "HYPERTENSION", "CARDIOVASCULAR", "CHRONIC_LIVER_DISEASE", "HEMOLYTIC_ANEMIA", 
  "NEOROLOGICAL", "TUBERCULOSIS", "CANCER", "RENAL", "NUM_VAX", "WAVE", "POS")
SNV_pred_positivity[, (factors) := lapply(.SD, as.factor), .SDcols = factors]

save(SNV_pred_positivity, file = "data/SNV_pred_positivity.RData")
