#' 005d_sensitivity_vaccine_results.R
#' 
#' Get TMLE estimates and 95% CIs, stratified on dosages of vaccines received
#' 
#' Input TMLE result files: out/TMLE_shift_NUM_VAX_0_0.05_result.RData
#'                          out/TMLE_shift_NUM_VAX_0_0.1_result.RData
#'                          out/TMLE_shift_NUM_VAX_1_OR_2_0.05_result.RData
#'                          out/TMLE_shift_NUM_VAX_1_OR_2_0.1_result.RData
#'
#' Required data: data/SNV_MET_ALL_BEFORE_POSITIVE.RData

library(data.table)

load("data/SNV_MET_ALL_BEFORE_POSITIVE.RData")
if (is.factor(DT$DEAD)) {
  DT[, DEAD := as.numeric(DEAD) - 1]
}

get_result <- function(tmle_result, num_vax) {
  DT_TMP <- DT[NUM_VAX %in% num_vax]
  
  se_hat <- sqrt((mean(DT_TMP$DEAD)*(1-mean(DT_TMP$DEAD)))/nrow(DT_TMP))
  se_psi_hat <- sqrt(se_hat^2+tmle_result$summary$se^2)
  tmle_res <- mean(DT_TMP$DEAD)-tmle_result$summary$tmle_est
  tmle_ci <- c(tmle_res - 1.96*se_psi_hat, tmle_res + 1.96*se_psi_hat)
  return(list(est = tmle_res, ci = tmle_ci))
}

# NUM_VAX = 0
load("out/TMLE_shift_NUM_VAX_0_0.05_result.RData")
res_0_5 <- get_result(tmle_fit, 0)
rm(tmle_fit)
gc()
load("out/TMLE_shift_NUM_VAX_0_0.1_result.RData")
res_0_10 <- get_result(tmle_fit, 0)
rm(tmle_fit)
gc()

# NUM_VAX = 1 OR 2
load("out/TMLE_shift_NUM_VAX_1_OR_2_0.05_result.RData")
res_1_OR_2_5 <- get_result(tmle_fit, c(1, 2))
rm(tmle_fit)
gc()
load("out/TMLE_shift_NUM_VAX_1_OR_2_0.1_result.RData")
res_1_OR_2_10 <- get_result(tmle_fit, c(1, 2))
rm(tmle_fit)
gc()

res_5 <- list(res_0_5, res_1_OR_2_5)
res_10 <- list(res_0_10, res_1_OR_2_10)
df_res_5 <- do.call(rbind, lapply(res_5, function(x) data.frame(est = x$est, 
                                                                ci_low = x$ci[1], 
                                                                ci_high = x$ci[2])))
df_res_10 <- do.call(rbind, lapply(res_10, function(x) data.frame(est = x$est, 
                                                                  ci_low = x$ci[1], 
                                                                  ci_high = x$ci[2])))
save(list = c("df_res_5", "df_res_10"), file = "out/TMLE_NUM_VAX_results.RData")
