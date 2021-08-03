# SETTINGS AND DATA -----------------------------------------------------------
library(RColorBrewer)
library(tidyquant)
library(tidyverse)
library(pls)
library(sandwich)
library(xtable)
set.seed(137)

logret <- read_rds("~/GitHub/tpcr-paper/data ex/logret.Rds")

# FUNCTION TO GET RESULTS -----------------------------------------------------
get_results <- function(resp_symbol = "HD", train_idx = 1:70)
{
  Y <- as.matrix(logret[, resp_symbol])
  X <- as.matrix(logret[, !names(logret) %in% c("date", resp_symbol)])

  # Create training and test data
  Y_train <- Y[train_idx, , drop = F]
  Y_test <- Y[-train_idx, , drop = F]

  # Training and test predictors
  X_train <- X[train_idx, , drop = F]
  X_test <-  X[-train_idx, , drop = F]

  D_train <- data.frame(Y_train, X_train)
  D_test <- data.frame(Y_test, X_test)

  # Fit our model
  fit_tpcr <- tpcr::tpcr(Y = Y_train, X = X_train, k = 1:10, scale_X = TRUE,
                         Xnew = X_test, m = c(2, log(nrow(X_train))))
  k_aic <- fit_tpcr[[11]][1]
  k_bic <- fit_tpcr[[11]][2]
  pred_aic <- fit_tpcr[[k_aic]]$Yhat
  pred_bic <- fit_tpcr[[k_bic]]$Yhat
  pred_one <- fit_tpcr[[1]]$Yhat

  # Fit classical principal components regression and pick k by CV
  fit_pcr <- pls::pcr(formula(paste0(resp_symbol, "~.")), data = D_train,
                      validation = "LOO", scale = TRUE)
  k_pcr <-  unname(which.min(RMSEP(fit_pcr)$val[1, 1, ]) - 1)
  pred_pcr <- predict(fit_pcr, newdata = D_test, ncomp = k_pcr)[, , 1]
  pred_pcr_one <- predict(fit_pcr, newdata = D_test, ncomp = 1)[, , 1]

  # Fit partial least squares and pick k by CV
  fit_pls <- pls::plsr(formula(paste0(resp_symbol, "~.")), data = D_train,
                       method = "simpls", validation = "LOO")
  k_pls <- unname(which.min(RMSEP(fit_pls)$val[1, 1, ]) - 1)
  pred_pls <- predict(fit_pls, newdata = D_test, ncomp = k_pls)[, , 1]
  pred_pls_one <- predict(fit_pls, newdata = D_test, ncomp = 1)[, , 1]


  # Fit Envelope models for all provided ways to select k
  k_env <- Renvlp::u.xenv(Y = Y_train, X = X_train)
  fit_env_aic <- Renvlp::xenv(Y = Y_train, X = X_train, u = k_env$u.aic)
  fit_env_bic <- Renvlp::xenv(Y = Y_train, X = X_train, u = k_env$u.bic)
  fit_env_lrt <- Renvlp::xenv(Y = Y_train, X = X_train, u = k_env$u.lrt)
  fit_env_one <- Renvlp::xenv(Y = Y_train, X = X_train, u = 1)
  pred_env_aic <- X_test %*% fit_env_aic$beta + rep(fit_env_aic$mu, nrow(X_test))
  pred_env_bic <- X_test %*% fit_env_bic$beta + rep(fit_env_bic$mu, nrow(X_test))
  pred_env_lrt <- X_test %*% fit_env_lrt$beta + rep(fit_env_lrt$mu, nrow(X_test))
  pred_env_one <- X_test %*% fit_env_one$beta + rep(fit_env_one$mu, nrow(X_test))


  # Fit OLS for comparison
  fit_ols <- lm(formula(paste0(resp_symbol, "~.")), data = D_train)
  pred_ols <- predict(fit_ols, newdata = D_test)


  rmse <- c(
    aic = sqrt(mean((Y_test - pred_aic)^2)),
    bic = sqrt(mean((Y_test - pred_bic)^2)),
    one = sqrt(mean((Y_test - pred_one)^2)),
    pcr = sqrt(mean((Y_test - pred_pcr)^2)),
    pcr_one = sqrt(mean((Y_test - pred_pcr_one)^2)),
    pls = sqrt(mean((Y_test - pred_pls)^2)),
    pls_one = sqrt(mean((Y_test - pred_pls_one)^2)),
    env_aic = sqrt(mean((Y_test - pred_env_aic)^2)),
    env_bic = sqrt(mean((Y_test - pred_env_bic)^2)),
    env_lrt = sqrt(mean((Y_test - pred_env_lrt)^2)),
    env_one = sqrt(mean((Y_test - pred_env_one)^2)),
    ols = sqrt(mean((Y_test - pred_ols)^2)),
    no_preds = sqrt(mean((Y_test - mean(Y_train))^2))
  )

  selected_k <- c(aic = k_aic, bic = k_bic,
                  pcr = k_pcr, pls = k_pls, env_aic = k_env$u.aic,
                  env_bic = k_env$u.bic, env_lrt = k_env$u.lrt)
  out <- list(rmse = rmse, selected_k = selected_k)
  return(out)
}

# TABLE 1 ---------------------------------------------------------------------
tab_full <- matrix(0, 58, 5)
ii <- 1
for(symbol in names(logret)[-1]){
  results <- get_results(resp_symbol = symbol)
  tab_full[ii, ] <- c(results$selected_k[c("bic", "pcr", "pls", "env_bic")],
    28)
  tab_full[ii + 1, ] <- results$rmse[c("bic", "pcr", "pls", "env_bic", "ols")] /
    results$rmse["no_preds"]
  ii <- ii + 2
}
colnames(tab_full) <- c("TPCR", "PCR", "PLS", "Env", "OLS")
tab1 <- round(rbind(
  tab_full[6:5, ],
  colSums(outer(apply(tab_full[2 * 1:29, ], 1, which.min), 1:5,  FUN = "==")),
  apply(tab_full[2 * 1:29, ], 2, mean),
  apply(tab_full[2 * 1:29, ], 2, max),
  apply(tab_full[2 * 1:29 - 1, ], 2, mean)),
  3)
rownames(tab1) <- c("RMSE", "k", "# best", "Ave. RMSE", "Max. RMSE", "Ave. k")

# MODEL FIT FULL DATA ---------------------------------------------------------
full_ols <- lm(HD ~ . - date, data = logret)

X <- model.matrix(full_ols)[, -1] # Remove intercept
Y <- logret$HD
full_k <- 5 # k_star = 5
full_tpcr <- tpcr::tpcr(Y = Y,
                        X = X,
                        k = full_k,
                        scale_X = TRUE, scale_Y = TRUE)
s_X <- apply(X, 2, sd)
s_Y <- sd(Y)
eig_X <- eigen(diag(1 / s_X) %*% full_tpcr$Sigma_X %*% diag(1 / s_X), symmetric = TRUE)
b_scale <- (diag(s_X) %*% full_tpcr$b) / s_Y
U_scale <- eig_X$vectors[, 1:full_k]
V_scale <- eig_X$vectors[, -c(1:full_k)]
max(abs(crossprod(V_scale, b_scale))) # Should be ~zero
gamma_scale <- crossprod(U_scale, b_scale) # Should be coef below
fit_reduced <- lm(scale(Y) ~ 0 + scale(X) %*% U_scale)
se_gamma_scale <- sqrt(diag(sandwich::kernHAC(fit_reduced)))
t_vals_gamma_scale <- gamma_scale / se_gamma_scale
p_vals_gamma_scale <- 2 * pnorm(abs(t_vals_gamma_scale), lower = F)
se_b_scale <- sqrt(diag(U_scale %*% sandwich::kernHAC(fit_reduced) %*% t(U_scale)))
t_vals_b_scale <- b_scale / se_b_scale
p_vals_b_scale <- 2 * pnorm(abs(t_vals_b_scale), lower = F)

fit_ols_scale <- lm(scale(Y) ~ 0 + scale(X))
b_ols_scale <- unname(coef(fit_ols_scale))
se_b_ols_scale <- unname(sqrt(diag(sandwich::kernHAC(fit_ols_scale))))
t_vals_ols_scale <- b_ols_scale / se_b_ols_scale
p_vals_ols_scale <- 2 * pnorm(abs(t_vals_ols_scale), lower = F)

se_ratio <- se_b_ols_scale / se_b_scale
names(se_ratio) <- colnames(X)

tab2 <- round(cbind(b_scale, se_b_scale, p_vals_b_scale, b_ols_scale,
                    se_b_ols_scale, p_vals_ols_scale, se_ratio, -U_scale[, 1]), 4) # positive sign wlog
colnames(tab2) <- c("b", "se", "p", "b_ols", "se_ols", "p_ols", "se_ratio", "u_1")
xtable(tab2, digits = 3)

tab3 <- round(cbind(gamma_scale * c(-1, rep(1, length(gamma_scale) - 1)), se_gamma_scale, p_vals_gamma_scale), 3)
colnames(tab3) <- c("b", "se", "p")
rownames(tab3) <- paste0("u", 1:5)
xtable(tab3, digits = 3)
