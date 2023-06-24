library(txshift)
library(sl3)
library(SuperLearner)
library(data.table)
library(tmle3)
library(tmle3shift)

additive_bound_val <- function(tmle_task, delta = 0.2, ...) {
  pdc <- tmle_task$get_tmle_node("A")
  shift_amt <- ifelse(pdc > 0.8, 0, delta)
  return(pdc + shift_amt)
}

additive_bound_val_inv <- function(tmle_task, delta = 0.2, ...) {
  pdc <- tmle_task$get_tmle_node("A")
  shift_amt <- ifelse(pdc > 0.8, 0, delta)
  return(pdc - shift_amt)
}

TMLE_shift_custom <- function(data_path, fname, shift_value, num_vax) {
  # load data
  load(data_path)
  
  # filter number of vaccines
  DT <- DT[NUM_VAX %in% num_vax]
  DT[, PDC := round(PDC, 2)]
  if (is.factor(DT$DEAD)) {
    DT[, DEAD := as.numeric(DEAD) - 1]
  }
  
  # Learners for outcome regression
  Q_lib <- list(
    mean = make_learner(Lrnr_mean),
    glm = make_learner(Lrnr_glm_fast),
    xgb = make_learner(Lrnr_xgboost, nrounds = 20, maxdepth = 6),
    ranger_small = make_learner(Lrnr_ranger, num.trees = 500),
    lasso_fast = make_learner(Lrnr_glmnet, nfold = 3),
    ridge_fast = make_learner(Lrnr_glmnet, nfold = 3, alpha = 0),
    enet_fast = make_learner(Lrnr_glmnet, nfold = 3, alpha = 0.5),
    earth = make_learner(Lrnr_earth),
    bayesglm = make_learner(Lrnr_bayesglm),
    gam = Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.gam"),
    bart = Lrnr_dbarts$new(ndpost = 1000, verbose = FALSE),
    xgb_SL = make_learner(Lrnr_xgboost, nrounds = 1000, max_depth = 4, eta = 0.1)
  )
  Q_lrnr <- Lrnr_sl$new(
    learners = Q_lib#,
    #metalearner = Lrnr_cv_selector$new()
  )
  
  # Learners for generalized propensity score
  g_lib <- list(
    hese = make_learner(Lrnr_density_semiparametric,
                        mean_learner = make_learner(Lrnr_glm_fast),
                        var_learner = make_learner(Lrnr_glm_fast)), 
    hose = make_learner(Lrnr_density_semiparametric,
                        mean_learner = make_learner(Lrnr_glm_fast))
  )
  
  g_lrnr <- Lrnr_sl$new(
    learners = g_lib#,
    #metalearner = Lrnr_cv_selector$new()
  )
  
  learner_list <- list(Y = Q_lrnr, A = g_lrnr)
  
  W <- names(DT)[!names(DT) %in% c("ID", "PDC", "DEAD", "WAVE")]
  A <- "PDC"
  Y <- "DEAD"
  task <- make_sl3_Task(DT, c(W, A), Y)
  DT <- task$data
  
  node_list <- list(
    W = names(DT)[!names(DT) %in% c("ID", "PDC", "DEAD")],
    A = A,
    Y = Y
  )
  
  tmle_spec <- tmle_shift(
    shift_val = shift_value,
    shift_fxn = shift_additive,
    shift_fxn_inv = shift_additive_inv
  )
  
  tmle_fit <- tmle3(tmle_spec, DT, node_list, learner_list)
  
  save(tmle_fit, file = "out/TMLE_shift_" %+% fname %+% "_result.RData")
}
