do_one_sim <- function(set){
  set.seed(set$seed)
  library(doParallel)
  library(doRNG)
  library(pls)

  Sigma0 <- diag(set$ssy, set$r)

  U0 <- rstiefel::rustiefel(set$p, set$k)
  D0 <- diag(seq(0.9 * set$d, set$d * 1.1, length.out = set$k), set$k)
  SigmaX0 <- set$tau * (diag(1, set$p) + U0 %*% D0 %*% t(U0))

  # True coefficients
  alpha0 <- matrix(runif(set$k * set$r, -1, 1), set$k, set$r)

  # Standardize so that each column has magnitude coef_scale
  alpha0 <- set$coef_scale * scale(alpha0, center = F)
  beta0 <- U0 %*% alpha0

  # Simulation
  cl <- makeCluster(set$n_cores)
  registerDoParallel(cl)
  res_mat <- foreach(ii = 1:set$n_sims, .combine = rbind,
                     .errorhandling = "remove",
                     .packages = c("pls", "tpcr") %dorng%{
    X_train <- matrix(rnorm(set$n * set$p), nrow = set$n, ncol = set$p) %*% chol(SigmaX0)
    Y_train <- X_train %*% beta0 + matrix(rnorm(set$n * set$r), set$n, set$r) %*% chol(Sigma0)

    # Select k
    fit_tpcr <- tpcr::tpcr(Y = Y_train,
                              X = X_train,
                              k = seq(max(1, set$k - 5),  min(set$p - 1, set$k + 5)),
                              rho = 0,
                              m = c(2, log(set$n),
                              covmat = F)
                  )
    k_aic <- fit_tpcr$k_star[1]
    k_bic <- fit_tpcr$k_star[2]

    fit_pcr <- pls::pcr(Y_train ~ 0 + X_train, ncomp = set$p - 1, validation = "LOO")
    k_pcr <- which.min(colMeans(pls::RMSEP(fit_pcr)$val[1, 1:2, ])) - 1

    fit_pls <- pls::plsr(Y_train ~ 0 + X_train, ncomp = set$p - 1,
      method = "simpls", validation = "LOO")
    k_pls <- which.min(colMeans(pls::RMSEP(fit_pls)$val[1, 1:2, ])) - 1

    env_k <- Renvlp::u.xenv(X_train, Y_train)
    k_env_aic <- env_k$u.aic
    k_env_bic <- env_k$u.bic
    k_env_lrt <- env_k$u.lrt

    # Get coefficients
    beta_aic <- fit_tpcr[[paste0("fit_k_", k_aic)]]$b
    beta_bic <- fit_tpcr[[paste0("fit_k_", k_bic)]]$b
    beta_pcr <- fit_pcr$coefficients[ , , k_pcr]
    beta_pls <- fit_pls$coefficients[ , , k_pls]
    beta_ols <- coef(lm(Y_train ~ 0 + X_train))
    beta_env_aic <- Renvlp::xenv(X = X_train, Y = Y_train, u = k_env_aic)$beta
    beta_env_bic <- Renvlp::xenv(X = X_train, Y = Y_train, u = k_env_bic)$beta
    beta_env_lrt <- Renvlp::xenv(X = X_train, Y = Y_train, u = k_env_lrt)$beta
    # Collect normed relative estimation error
    out <- c(norm(beta_aic - beta0, "F"),
             norm(beta_bic - beta0, "F"),
             norm(beta_pcr - beta0, "F"),
             norm(beta_pls - beta0, "F"),
             norm(beta_env_aic - beta0, "F"),
             norm(beta_env_bic - beta0, "F"),
             norm(beta_env_lrt - beta0, "F"),
             norm(beta_ols - beta0, "F")) / norm(beta0, "F")

    # Generate test data
    X_test <- matrix(rnorm(set$n * set$p), nrow = set$n, ncol = set$p) %*% chol(SigmaX0)
    Y_test <- X_test %*% beta0 + matrix(rnorm(set$n * set$r), set$n, set$r) %*% chol(Sigma0)

    # Collect prediction error
    out <- c(out, norm(X_test %*% beta_aic - Y_test, "F"),
                  norm(X_test %*% beta_bic - Y_test, "F"),
                  norm(X_test %*% beta_pcr - Y_test, "F"),
                  norm(X_test %*% beta_pls - Y_test, "F"),
                  norm(X_test %*% beta_env_aic - Y_test, "F"),
                  norm(X_test %*% beta_env_bic - Y_test, "F"),
                  norm(X_test %*% beta_env_lrt - Y_test, "F"),
                  norm(X_test %*% beta_ols - Y_test, "F"))

    # Collect estimated number of components
    out <- c(out, k_aic, k_bic, k_pcr, k_pls, k_env_aic, k_env_bic, k_env_lrt, set$p)
  }
  stopCluster(cl)
  # Output data
  return(res_mat)
}
