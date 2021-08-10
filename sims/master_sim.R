# Directory of sim_script.R
script_dir <- "~/GitHub/tpcr-suppl/sims/"

# Directory to save results
save_dir <- "~/GitHub/tpcr-suppl/sims/"

# Script with function to do one simulation
source(paste0(script_dir, "sim_script.R"))

# Baseline settings
base_set <- list(n = 120, p = 30, k = 3, r = 2, ssy = 1,
                 tau = 0.5, d = 5,
                 n_sims = 1000, seed = 1, n_cores = 11, coef_scale = 1)
base_set$tau <- base_set$p / ((base_set$p - base_set$k) + 
                      sum(1 + seq(0.9 * base_set$d, base_set$d * 1.1,
                                  length.out = base_set$k)))
# COEFFICIENT SIZE ------------------------------------------------------------
sizes <- seq(0.1, 2.0, length.out = 5)
plot_mat <- matrix(0, length(sizes), 62 + 11)
for(ii in seq(length(sizes))){
  set <- base_set
  set$coef_scale <- sizes[ii]
  set$seed <- ii
  res_mat <- do_one_sim(set)
  plot_mat[ii, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd), unlist(set))
  cat("Completed ", ii, " of ", length(sizes), " coef. size iterations. \n")
}
colnames(plot_mat) <- c(colnames(res_mat), paste0("sd_", colnames(res_mat)),
                        names(unlist(set)))

saveRDS(plot_mat, paste0(save_dir, "coef_change.Rds"))

# SPIKE EIGENVALUE SIZE -------------------------------------------------------
spikes <- seq(2, 10, length.out = 5)
plot_mat <- matrix(0, length(spikes), 62 + 11)
for(ii in seq(length(spikes))){
  set <- base_set
  set$d <- spikes[ii]
  set$tau <- set$p / ((set$p - set$k) + 
                        sum(1 + seq(0.9 * set$d, set$d * 1.1,
                                    length.out = set$k)))
  set$seed <- ii
  res_mat <- do_one_sim(set)
  plot_mat[ii, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd), unlist(set))
  cat("Completed ", ii, " of ", length(spikes), " spike. eval. iterations. \n")
}
colnames(plot_mat) <- c(colnames(res_mat), paste0("sd_", colnames(res_mat)),
                        names(unlist(set)))
saveRDS(plot_mat, paste0(save_dir, "eval_change.Rds"))


# NO. OF PREDICTORS -----------------------------------------------------------
num_p <- floor(seq(10, 60, length.out = 5))
plot_mat <- matrix(0, length(num_p), 62 + 11)
for(ii in seq(length(num_p))){
  set <- base_set
  set$p <- num_p[ii]
  set$tau <- set$p / ((set$p - set$k) + 
                        sum(1 + seq(0.9 * set$d, set$d * 1.1,
                                    length.out = set$k)))
  set$seed <- ii
  res_mat <- do_one_sim(set)
  plot_mat[ii, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd), unlist(set))
  cat("Completed ", ii, " of ", length(num_p), " no. pred. iterations. \n")
}
colnames(plot_mat) <- c(colnames(res_mat), paste0("sd_", colnames(res_mat)),
                        names(unlist(set)))
saveRDS(plot_mat, paste0(save_dir, "p_change.Rds"))

# NO. OF COMPONENTS -----------------------------------------------------------
num_comp<- floor(seq(1, 15, length.out = 5))
plot_mat <- matrix(0, length(num_comp), 62 + 11)
for(ii in seq(length(num_comp))){
  set <- base_set
  set$k <- num_comp[ii]
  set$tau <- set$p / ((set$p - set$k) + 
                        sum(1 + seq(0.9 * set$d, set$d * 1.1,
                                    length.out = set$k)))
  set$seed <- ii
  res_mat <- do_one_sim(set)
  plot_mat[ii, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd), unlist(set))
  cat("Completed ", ii, " of ", length(num_comp), " no. comp. iterations. \n")
}
colnames(plot_mat) <- c(colnames(res_mat), paste0("sd_", colnames(res_mat)), 
                        names(unlist(set)))
saveRDS(plot_mat, paste0(save_dir, "k_change.Rds"))


# NO. OF OBSERVATIONS ---------------------------------------------------------
num_obs <- c(50, 100, 200, 400, 800)
plot_mat <- matrix(0, length(num_obs), 62 + 11)
for(ii in seq(length(num_obs))){
  set <- base_set
  set$n <- num_obs[ii]
  set$seed <- ii
  res_mat <- do_one_sim(set)
  plot_mat[ii, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd), unlist(set))
  cat("Completed ", ii, " of ", length(num_obs), " no. obs. iterations. \n")
}
colnames(plot_mat) <- c(colnames(res_mat), paste0("sd_", colnames(res_mat)), 
                        names(unlist(set)))
saveRDS(plot_mat, paste0(save_dir, "n_change.Rds"))

# If you want a notification at the end (MacOS only)
system("say Your simulations are all done!")
