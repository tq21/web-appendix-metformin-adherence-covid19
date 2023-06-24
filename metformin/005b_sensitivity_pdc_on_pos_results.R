#' 005b_sensitivity_pdc_on_pos_results.R
#' 
#' Get TMLE estimates and 95% CIs, effect of PDC on test positive
#' 
#' Input TMLE result files: out/TMLE_shift_POS_OVERALL_0.05_result.RData
#'                          out/TMLE_shift_POS_OVERALL_0.1_result.RData
#'                          out/TMLE_shift_POS_WAVE_1_0.05_result.RData
#'                          out/TMLE_shift_POS_WAVE_1_0.1_result.RData
#'                          out/TMLE_shift_POS_WAVE_2_0.05_result.RData
#'                          out/TMLE_shift_POS_WAVE_2_0.1_result.RData
#'                          out/TMLE_shift_POS_WAVE_3_0.05_result.RData
#'                          out/TMLE_shift_POS_WAVE_3_0.1_result.RData
#'
#' Required data: data/SNV_MET_ALL_BEFORE_POSITIVE_POS.RData

library(data.table)

load("data/SNV_MET_ALL_BEFORE_POSITIVE_POS.RData")
if (is.factor(DT$POS)) {
  DT[, POS := as.numeric(POS) - 1]
}

get_result <- function(tmle_result, waves) {
  DT_TMP <- DT[WAVE %in% waves]
  
  se_hat <- sqrt((mean(DT_TMP$POS)*(1-mean(DT_TMP$POS)))/nrow(DT_TMP))
  se_psi_hat <- sqrt(se_hat^2+tmle_result$summary$se^2)
  tmle_res <- mean(DT_TMP$POS)-tmle_result$summary$tmle_est
  tmle_ci <- c(tmle_res - 1.96*se_psi_hat, tmle_res + 1.96*se_psi_hat)
  return(list(est = tmle_res, ci = tmle_ci))
}

# WAVE 1
load("out/TMLE_shift_POS_WAVE_1_0.1_result.RData")
res_1 <- get_result(tmle_fit, 1)
rm(tmle_fit)
gc()

# WAVE 2
load("out/TMLE_shift_POS_WAVE_2_0.1_result.RData")
res_2 <- get_result(tmle_fit, 2)
rm(tmle_fit)
gc()

# WAVE 3
load("out/TMLE_shift_POS_WAVE_3_0.1_result.RData")
res_3 <- get_result(tmle_fit, 3)
rm(tmle_fit)
gc()

res <- list(res_1, res_2, res_3)
df_res <- do.call(rbind, lapply(res, function(x) data.frame(est = x$est, 
                                                            ci_low = x$ci[1], 
                                                            ci_high = x$ci[2])))
save("df_res", file = "out/TMLE_pos_results.RData")
