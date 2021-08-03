# Directory of sim_script.R
script_dir <- "~/GitHub/tpcr-paper/sims/"

# Directory to save results
save_dir <- "~/GitHub/tpcr-paper/sims/"


source(paste0(script_dir, "sim_script.R"))
methods <- c("aic", "bic", "pcr", "pls", "env_aic", "env_bic",
             "env_lrt", "ols")

# COEFFICIENT SIZE ------------------------------------------------------------
out_names <- c(methods,
              paste0(methods, "_pred"),
              paste0(methods, "_k"),
              paste0("sd_", methods),
              paste0("sd_", methods, "_pred"),
              paste0("sd_", methods, "_k"),
              "coef_scale")

plot_mat <- matrix(0, 5, 48)
idx <- 1
vals <- seq(1, 3, length.out = 5)
for(ii in vals){
  set <- list(n = 120, p = 40, k = 4, r = 2, ssy = 2, tau = 1, d = 3,
              n_sims = 500, seed = idx, n_cores = 11, coef_scale = ii)

  res_mat <- do_one_sim(set)
  plot_mat[idx, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd))
  idx <- idx + 1
}
plot_mat <- cbind(plot_mat, vals)
colnames(plot_mat) <- out_names
saveRDS(plot_mat, paste0(save_dir, "coef_change.Rds"))

# SPIKE EIGENVALUE SIZE -------------------------------------------------------
plot_mat <- matrix(0, 5, 48)
idx <- 1
vals <- seq(1, 5, length.out = 5)
for(ii in vals){
  set <- list(n = 120, p = 40, k = 4, r = 2, ssy = 2, tau = 1, d = ii,
              n_sims = 500, seed = idx, n_cores = 11, coef_scale = 2)

  res_mat <- do_one_sim(set)
  plot_mat[idx, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd))
  idx <- idx + 1
}
plot_mat <- cbind(plot_mat, "d" = vals)
out_names[length(out_names)] <- "d"
colnames(plot_mat) <- out_names
saveRDS(plot_mat, paste0(save_dir, "eval_change.Rds"))


# NO. OF PREDICTORS -----------------------------------------------------------
plot_mat <- matrix(0, 5, 48)
idx <- 1
vals <- floor(seq(20, 60, length.out = 5))
for(ii in vals){
  set <- list(n = 120, p = ii, k = 4, r = 2, ssy = 2, tau = 1, d = 3,
              n_sims = 500, seed = idx, n_cores = 11, coef_scale = 2)

  res_mat <- do_one_sim(set)
  plot_mat[idx, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd))
  idx <- idx + 1
}

plot_mat <- cbind(plot_mat, "p" = vals)
out_names[length(out_names)] <- "p"
colnames(plot_mat) <- out_names
saveRDS(plot_mat, paste0(save_dir, "p_change.Rds"))

# NO. OF COMPONENTS -----------------------------------------------------------
plot_mat <- matrix(0, 5, 48)
idx <- 1
vals <- floor(seq(1, 20, length.out = 5))
for(ii in vals){
  set <- list(n = 120, p = 40, k = ii, r = 2, ssy = 2, tau = 1, d = 3,
              n_sims = 500, seed = idx, n_cores = 11, coef_scale = 2)

  res_mat <- do_one_sim(set)
  plot_mat[idx, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd))
  idx <- idx + 1
}

plot_mat <- cbind(plot_mat, "k" = vals)
out_names[length(out_names)] <- "k"
colnames(plot_mat) <- out_names
saveRDS(plot_mat, paste0(save_dir, "k_change.Rds"))


# NO. OF OBSERVATIONS ---------------------------------------------------------
plot_mat <- matrix(0, 5, 48)
idx <- 1
vals <- floor(seq(50, 250, length.out = 5))
for(ii in vals){
  set <- list(n = ii, p = 40, k = 4, r = 2, ssy = 2, tau = 1, d = 3,
              n_sims = 500, seed = idx, n_cores = 11, coef_scale = 2)

  res_mat <- do_one_sim(set)
  plot_mat[idx, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd))
  idx <- idx + 1
}

plot_mat <- cbind(plot_mat, "n" = vals)
out_names[length(out_names)] <- "n"
colnames(plot_mat) <- out_names
saveRDS(plot_mat, paste0(save_dir, "n_change.Rds"))
