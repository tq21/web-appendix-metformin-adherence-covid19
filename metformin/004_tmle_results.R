#' 004_tmle_results.R
#' 
#' Get TMLE estimates and 95% CIs
#' 
#' Input TMLE result files: out/TMLE_shift_OVERALL_0.05_result.RData
#'                          out/TMLE_shift_OVERALL_0.1_result.RData
#'                          out/TMLE_shift_WAVE_1_0.05_result.RData
#'                          out/TMLE_shift_WAVE_1_0.1_result.RData
#'                          out/TMLE_shift_WAVE_2_0.05_result.RData
#'                          out/TMLE_shift_WAVE_2_0.1_result.RData
#'                          out/TMLE_shift_WAVE_3_0.05_result.RData
#'                          out/TMLE_shift_WAVE_3_0.1_result.RData
#'
#' Required data: data/SNV_MET_ALL_BEFORE_POSITIVE.RData

library(data.table)

load("data/SNV_MET_ALL_BEFORE_POSITIVE.RData")
if (is.factor(DT$DEAD)) {
  DT[, DEAD := as.numeric(DEAD) - 1]
}

get_result <- function(tmle_result, waves) {
  DT_TMP <- DT[WAVE %in% waves]
  
  se_hat <- sqrt((mean(DT_TMP$DEAD)*(1-mean(DT_TMP$DEAD)))/nrow(DT_TMP))
  se_psi_hat <- sqrt(se_hat^2+tmle_result$summary$se^2)
  tmle_res <- mean(DT_TMP$DEAD)-tmle_result$summary$tmle_est
  tmle_ci <- c(tmle_res - 1.96*se_psi_hat, tmle_res + 1.96*se_psi_hat)
  return(list(est = tmle_res, ci = tmle_ci))
}

# OVERALL
load("out/glm_TMLE_shift_OVERALL_0.05_result.RData")
res_all_5 <- get_result(tmle_fit, c(1, 2, 3))
rm(tmle_fit)
gc()
load("out/with_insulin_TMLE_shift_OVERALL_0.1_result.RData")
res_all_10 <- get_result(tmle_fit, c(1, 2, 3))
rm(tmle_fit)
gc()

# WAVE 1
load("out/glm_TMLE_shift_WAVE_1_0.05_result.RData")
res_1_5 <- get_result(tmle_fit, 1)
rm(tmle_fit)
gc()
load("out/with_insulin_TMLE_shift_WAVE_1_0.1_result.RData")
res_1_10 <- get_result(tmle_fit, 1)
rm(tmle_fit)
gc()

# WAVE 2
load("out/glm_TMLE_shift_WAVE_2_0.05_result.RData")
res_2_5 <- get_result(tmle_fit, 2)
rm(tmle_fit)
gc()
load("out/with_insulin_TMLE_shift_WAVE_2_0.1_result.RData")
res_2_10 <- get_result(tmle_fit, 2)
rm(tmle_fit)
gc()

# WAVE 3
load("out/glm_TMLE_shift_WAVE_3_0.05_result.RData")
res_3_5 <- get_result(tmle_fit, 3)
rm(tmle_fit)
gc()
load("out/with_insulin_TMLE_shift_WAVE_3_0.1_result.RData")
res_3_10 <- get_result(tmle_fit, 3)
rm(tmle_fit)
gc()

res_5 <- list(res_all_5, res_1_5, res_2_5, res_3_5)
res_10 <- list(res_all_10, res_1_10, res_2_10, res_3_10)
df_res_5 <- do.call(rbind, lapply(res_5, function(x) data.frame(est = x$est, 
                                                                ci_low = x$ci[1], 
                                                                ci_high = x$ci[2])))
df_res_10 <- do.call(rbind, lapply(res_10, function(x) data.frame(est = x$est, 
                                                                  ci_low = x$ci[1], 
                                                                  ci_high = x$ci[2])))
save(list = c("df_res_5", "df_res_10"), file = "out/TMLE_results.RData")
