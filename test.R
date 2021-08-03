set.seed(49)
# Settings
n <- 500
p <- 10
k <- 2
r <- 2
Psi0 <- tcrossprod(matrix(rnorm(p * k), nrow = p, ncol = k))
U0 <- eigen(Psi0)$vectors[, 1:k]
tau0 <- 1
Sigma_X0 <- Psi0 + diag(tau0, p)
Sigma0 <- diag(2, r) + diag(runif(r, -0.5, 0.5), r)
beta0 <- Psi0 %*% matrix(runif(r * p, -1, 1), ncol = r)

#beta0 <- diag(1 / diag(Sigma_X0)) %*% beta0 %*% diag(diag(Sigma0))

n_sims <- 1e2
coef_mat_sim_tpcr <- matrix(NA, ncol = p * r, nrow = n_sims)
coef_mat_sim_tpcr_s <- matrix(NA, ncol = p * r, nrow = n_sims)
coef_mat_sim_ols <- matrix(NA, ncol = p * r, nrow = n_sims)
coef_mat_sim_pcr <- matrix(NA, ncol = p * r, nrow = n_sims)
for(ii in 1:n_sims){
  X <- mvtnorm::rmvnorm(n, sigma = Sigma_X0) 
  Y <- X %*% beta0 + mvtnorm::rmvnorm(n, sigma = Sigma0)
  coef_mat_sim_ols[ii, ] <- c(qr.coef(qr(X), Y))
  coef_mat_sim_tpcr[ii, ] <- c(tpcr::tpcr(Y, X, k = k)$b)
  coef_mat_sim_tpcr_s[ii, ] <- c(tpcr::tpcr(Y, X, k = k, scale_X = TRUE)$b)
  Utilde <- svd(X)$v[, 1:k]
  coef_mat_sim_pcr[ii, ] <- c(Utilde %*% qr.coef(qr(X %*% Utilde), Y))
  cat("Completed iteration ", ii, "\n")
}
# Check accuracy
cat("Avererage norm error TPCR: ", mean(colMeans(abs(sweep(coef_mat_sim_tpcr, 2, c(beta0))))), "\n")
cat("Avererage norm error OLS: ", mean(colMeans(abs(sweep(coef_mat_sim_ols, 2, c(beta0))))), "\n")
cat("Avererage norm error TPCR-S: ", mean(colMeans(abs(sweep(coef_mat_sim_tpcr_s, 2, c(beta0))))), "\n")
cat("Avererage norm error PCR: ", mean(colMeans(abs(sweep(coef_mat_sim_pcr, 2, c(beta0))))), "\n")
# Check efficiency compared to least squares
C_HAT_LS <- cov(coef_mat_sim_ols)
C_HAT_TPCR <- cov(coef_mat_sim_tpcr)
print(eigen(C_HAT_LS - C_HAT_TPCR)$values)

# Check covariance matrix
C_TPCR <- kronecker(Sigma0, tcrossprod(U0) %*% qr.solve(Sigma_X0, tcrossprod(U0)))
C_LS <- kronecker(Sigma0, solve(Sigma_X0))


