# Directory of sim_script.R
script_dir <- "~/GitHub/tpcr-suppl/sims/"

# Directory to save results
save_dir <- "~/GitHub/tpcr-suppl/sims/new/"

# Script with function to do one simulation
source(paste0(script_dir, "sim_script.R"))

# Spiked eval
for(dd in c(1, 3, 5)){
  set <- list(n = 120, p = 40, k = 4, r = 2, ssy = 1, tau = 1, d = dd,
              n_sims = 100, seed = idx, n_cores = 11, coef_scale = 0.5)
  
  res_mat <- do_one_sim(set)
  
  MCest <- colMeans(res_mat)
  
  # Performance with BIC
  print(MCest[c("bic", "pcr", "pls", "env_bic")] / MCest["ols"])
  
  # Performance with true k
  print(MCest[c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / MCest["ols"])
}

# Number of preds
for(pp in c(10, 30, 60)){
  set <- list(n = 120, p = pp, k = 4, r = 2, ssy = 1, tau = 1, d = 3,
              n_sims = 100, seed = idx, n_cores = 11, coef_scale = 0.5)
  
  res_mat <- do_one_sim(set)
  
  MCest <- colMeans(res_mat)
  
  # Performance with BIC
  print(MCest[c("bic", "pcr", "pls", "env_bic")] / MCest["ols"])
  
  # Performance with true k
  print(MCest[c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / MCest["ols"])
}

# Number of obs
for(nn in c(50, 100, 200)){
  set <- list(n = nn, p = 30, k = 4, r = 2, ssy = 1, tau = 1, d = 3,
              n_sims = 100, seed = idx, n_cores = 11, coef_scale = 0.5)
  
  res_mat <- do_one_sim(set)
  
  MCest <- colMeans(res_mat)
  
  # Performance with BIC
  print(MCest[c("bic", "pcr", "pls", "env_bic")] / MCest["ols"])
  
  # Performance with true k
  print(MCest[c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / MCest["ols"])
}

# Coef scale
for(cs in c(0.2, 0.5, 1)){
  set <- list(n = 120, p = 30, k = 4, r = 2, ssy = 1, tau = 1, d = 3,
              n_sims = 100, seed = idx, n_cores = 11, coef_scale = cs)
  
  res_mat <- do_one_sim(set)
  
  MCest <- colMeans(res_mat)
  
  # Performance with BIC
  print(MCest[c("bic", "pcr", "pls", "env_bic")] / MCest["ols"])
  
  # Performance with true k
  print(MCest[c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / MCest["ols"])
}

# Number of components
for(kk in c(1, 4, 10)){
  set <- list(n = 120, p = 30, k = kk, r = 2, ssy = 1, tau = 1, d = 3,
              n_sims = 100, seed = idx, n_cores = 11, coef_scale = 0.5)
  
  res_mat <- do_one_sim(set)
  
  MCest <- colMeans(res_mat)
  
  # Performance with BIC
  print(MCest[c("bic", "pcr", "pls", "env_bic")] / MCest["ols"])
  
  # Performance with true k
  print(MCest[c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / MCest["ols"])
}
