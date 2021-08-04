# Directory of sim_script.R
script_dir <- "~/GitHub/tpcr-suppl/sims/"

# Directory to save results
save_dir <- "~/GitHub/tpcr-suppl/sims/new/"

# Script with function to do one simulation
source(paste0(script_dir, "sim_script.R"))

# COEFFICIENT SIZE ------------------------------------------------------------
# vals <- seq(1, 3, length.out = 5)
# plot_mat <- matrix(0, length(vals), 62 + 11)
# idx <- 1
# for(ii in vals){
#   set <- list(n = 120, p = 40, k = 4, r = 2, ssy = 2, tau = 1, d = 3,
#               n_sims = 500, seed = idx, n_cores = 11, coef_scale = ii)
# 
#   res_mat <- do_one_sim(set)
#   plot_mat[idx, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd), unlist(set))
#   idx <- idx + 1
#   cat("Completed ", idx - 1, " of ", length(vals), " coef. size iterations. \n")
# }
# colnames(plot_mat) <- c(colnames(res_mat), paste0("sd_", colnames(res_mat)), 
#                         names(unlist(set)))
# 
# saveRDS(plot_mat, paste0(save_dir, "coef_change.Rds"))

# # SPIKE EIGENVALUE SIZE -------------------------------------------------------
# vals <- seq(1, 5, length.out = 5)
# plot_mat <- matrix(0, length(vals), 62 + 11)
# idx <- 1
# for(ii in vals){
#   set <- list(n = 120, p = 40, k = 4, r = 2, ssy = 2, tau = 1, d = ii,
#               n_sims = 500, seed = idx, n_cores = 11, coef_scale = 2)
#   
#   res_mat <- do_one_sim(set)
#   plot_mat[idx, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd), unlist(set))
#   idx <- idx + 1
#   cat("Completed ", idx - 1, " of ", length(vals), " spike. eval. iterations. \n")
# }
# colnames(plot_mat) <- c(colnames(res_mat), paste0("sd_", colnames(res_mat)), 
#                         names(unlist(set)))
# saveRDS(plot_mat, paste0(save_dir, "eval_change.Rds"))
# 
# 
# # NO. OF PREDICTORS -----------------------------------------------------------
# vals <- floor(seq(20, 60, length.out = 5))
# plot_mat <- matrix(0, length(vals), 62 + 11)
# idx <- 1
# for(ii in vals){
#   set <- list(n = 120, p = ii, k = 4, r = 2, ssy = 2, tau = 1, d = 3,
#               n_sims = 500, seed = idx, n_cores = 11, coef_scale = 2)
#   
#   res_mat <- do_one_sim(set)
#   plot_mat[idx, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd), unlist(set))
#   idx <- idx + 1
#   cat("Completed ", idx - 1, " of ", length(vals), " no. pred. iterations. \n")
# }
# colnames(plot_mat) <- c(colnames(res_mat), paste0("sd_", colnames(res_mat)), 
#                         names(unlist(set)))
# saveRDS(plot_mat, paste0(save_dir, "p_change.Rds"))

# NO. OF COMPONENTS -----------------------------------------------------------
vals <- floor(seq(1, 20, length.out = 5))
plot_mat <- matrix(0, length(vals), 62 + 11)
idx <- 1
for(ii in vals){
  set <- list(n = 120, p = 40, k = ii, r = 2, ssy = 2, tau = 1, d = 3,
              n_sims = 500, seed = idx, n_cores = 11, coef_scale = 2)
  
  res_mat <- do_one_sim(set)
  plot_mat[idx, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd), unlist(set))
  idx <- idx + 1
  cat("Completed ", idx - 1, " of ", length(vals), " no. comp. iterations. \n")
}
colnames(plot_mat) <- c(colnames(res_mat), paste0("sd_", colnames(res_mat)), 
                        names(unlist(set)))
saveRDS(plot_mat, paste0(save_dir, "k_change.Rds"))


# NO. OF OBSERVATIONS ---------------------------------------------------------
vals <- floor(seq(50, 250, length.out = 5))
plot_mat <- matrix(0, length(vals), 62 + 11)
idx <- 1
for(ii in vals){
  set <- list(n = ii, p = 40, k = 4, r = 2, ssy = 2, tau = 1, d = 3,
              n_sims = 500, seed = idx, n_cores = 11, coef_scale = 2)
  
  res_mat <- do_one_sim(set)
  plot_mat[idx, ] <- c(colMeans(res_mat), apply(res_mat, 2, sd), unlist(set))
  idx <- idx + 1
  cat("Completed ", idx - 1, " of ", length(vals), " no. obs. iterations. \n")
}
colnames(plot_mat) <- c(colnames(res_mat), paste0("sd_", colnames(res_mat)), 
                        names(unlist(set)))
saveRDS(plot_mat, paste0(save_dir, "n_change.Rds"))

# If you want a notifaction at the end (MacOS only)
system("say Your simulations are all done!")
