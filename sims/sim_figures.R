# SETTINGS --------------------------------------------------------------------
library(RColorBrewer)
cbbPalette <- brewer.pal(n = 5, name = "Dark2")

# Directory of data to plot
data_dir <- "~/GitHub/tpcr-suppl/sims/"

coef_dat <- readRDS(paste0(data_dir, "coef_change.Rds"))
eval_dat <- readRDS(paste0(data_dir, "eval_change.Rds"))
pred_dat <- readRDS(paste0(data_dir, "p_change.Rds"))
pc_dat <- readRDS(paste0(data_dir, "k_change.Rds"))
n_dat <- readRDS(paste0(data_dir, "n_change.Rds"))

our_col <- cbbPalette[1]
pcr_col <- cbbPalette[2]
pls_col <- cbbPalette[3]
env_col <- cbbPalette[4]
ols_col <- cbbPalette[5]

our_lty <- 1
pcr_lty <- 2
pls_lty <- 3
env_lty <- 4
ols_lty <- 5

all_col <- c(our_col, pcr_col, pls_col, env_col)
all_lty <- c(our_lty, pcr_lty, pls_lty, env_lty)
all_type <- rep("l", 4)

###############################################################################
# FIGURE 1
###############################################################################
# Directory and name to save figure
out_name <- "~/GitHub/tpcr-suppl/figs/sims_fig_est.pdf"
pdf(out_name, width = 12.5, height = 12)
par(cex.axis = 1.3, cex.lab = 1.3)
par(mfrow = c(5, 3))
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.5,1.1))

# COEF SCALE ------------------------------------------------------------------
matplot(y = coef_dat[, c("bic", "pcr", "pls", "env_bic")] / coef_dat[, "ols"],
        x = coef_dat[, "coef_scale"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Coefficient column norm",
        main = "Unknown k")
legend("bottomright", legend = c("TPCR", "PCR", "PLS", "XENV"), 
       lty = all_lty, col = all_col, bty = "n", lwd = 2)

matplot(y = coef_dat[, c("ktrue", "pcr_ktrue", "pls_ktrue",
                         "env_ktrue")] / coef_dat[, "ols"],
        x = coef_dat[, "coef_scale"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Coefficient column norm",
        main = "Known k")

matplot(y = coef_dat[, c("k_bic", "k_pcr", "k_pls", "k_env_bic")] - coef_dat[, "k"],
        x = coef_dat[, "coef_scale"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Coefficient column norm",
        main = "Selection of k")

# EGENVALUE SPIKES ------------------------------------------------------------
matplot(y = eval_dat[, c("bic", "pcr", "pls", "env_bic")] / eval_dat[, "ols"],
        x = eval_dat[, "d"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Average spiked eigenvalue")

matplot(y = eval_dat[, c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")]
        / eval_dat[, "ols"],
        x = eval_dat[, "d"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Average spiked eigenvalue")

matplot(y = eval_dat[, c("k_bic", "k_pcr", "k_pls", "k_env_bic")] - eval_dat[, "k"],
        x = eval_dat[, "d"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Average spiked eigenvalue")

# NO. PREDICTORS --------------------------------------------------------------
matplot(y = pred_dat[, c("bic", "pcr", "pls", "env_bic")] / pred_dat[, "ols"],
        x = pred_dat[, "p"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of predictors")

matplot(y = pred_dat[, c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / pred_dat[, "ols"],
        x = pred_dat[, "p"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of predictors")

matplot(y = pred_dat[, c("k_bic", "k_pcr", "k_pls", "k_env_bic")] - pred_dat[, "k"],
        x = pred_dat[, "p"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Number of predictors")

# NO. COMPONENTS --------------------------------------------------------------
matplot(y = pc_dat[, c("bic", "pcr", "pls", "env_bic")] / pc_dat[, "ols"],
        x = pc_dat[, "k"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of components")

matplot(y = pc_dat[, c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / pc_dat[, "ols"],
        x = pc_dat[, "k"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of components")

matplot(y = pc_dat[, c("k_bic", "k_pcr", "k_pls", "k_env_bic")] - pc_dat[, "k"],
        x = pc_dat[, "k"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Number of components")

# NO. OBSERVATIONS ------------------------------------------------------------
matplot(y = n_dat[, c("bic", "pcr", "pls", "env_bic")] / n_dat[, "ols"],
        x = sqrt(n_dat[, "n"]),
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = expression(sqrt("Number of observations")))

matplot(y = n_dat[, c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / n_dat[, "ols"],
        x = sqrt(n_dat[, "n"]),
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = expression(sqrt("Number of observations")))

matplot(y = n_dat[, c("k_bic", "k_pcr", "k_pls", "k_env_bic")] - n_dat[, "k"],
        x = sqrt(n_dat[, "n"]),
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = expression(sqrt("Number of observations")))
dev.off()

###############################################################################
# FIGURE 2
###############################################################################
# Directory and name to save figure
out_name <- "~/GitHub/tpcr-suppl/figs/sims_fig_pred.pdf"
pdf(out_name, width = 12.5, height = 12)
par(cex.axis = 1.3, cex.lab = 1.3)
par(mfrow = c(5, 2))
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.5,1.1))

# COEF SCALE ------------------------------------------------------------------
matplot(y = coef_dat[, c("bic_pred", "pcr_pred", "pls_pred", "env_bic_pred")]
        / coef_dat[, "ols_pred"],
        x = coef_dat[, "coef_scale"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Coefficient column norm",
        main = "Unknown k")
legend("bottomright", legend = c("TPCR", "PCR", "PLS", "XENV"), 
       lty = all_lty, col = all_col, bty = "n", lwd = 2)

matplot(y = coef_dat[, c("ktrue_pred", "pcr_ktrue_pred", "pls_ktrue_pred",
                         "env_ktrue_pred")] / coef_dat[, "ols_pred"],
        x = coef_dat[, "coef_scale"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Coefficient column norm",
        main = "Known k")

# EGENVALUE SPIKES ------------------------------------------------------------
matplot(y = eval_dat[,  c("bic_pred", "pcr_pred", "pls_pred", "env_bic_pred")]
        / eval_dat[, "ols_pred"],
        x = eval_dat[, "d"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Average spiked eigenvalue")

matplot(y = eval_dat[, c("ktrue_pred", "pcr_ktrue_pred", "pls_ktrue_pred",
                         "env_ktrue_pred")] / eval_dat[, "ols_pred"],
        x = eval_dat[, "d"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Average spiked eigenvalue")

# NO. PREDICTORS --------------------------------------------------------------
matplot(y = pred_dat[,  c("bic_pred", "pcr_pred", "pls_pred", "env_bic_pred")]
        / pred_dat[, "ols_pred"],
        x = pred_dat[, "p"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of predictors")

matplot(y = pred_dat[, c("ktrue_pred", "pcr_ktrue_pred", "pls_ktrue_pred",
                         "env_ktrue_pred")] / pred_dat[, "ols_pred"],
        x = pred_dat[, "p"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of predictors")

# NO. COMPONENTS --------------------------------------------------------------
matplot(y = pc_dat[,  c("bic_pred", "pcr_pred", "pls_pred", "env_bic_pred")]
        / pc_dat[, "ols_pred"],
        x = pc_dat[, "k"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of components")

matplot(y = pc_dat[, c("ktrue_pred", "pcr_ktrue_pred", "pls_ktrue_pred",
                       "env_ktrue_pred")] / pc_dat[, "ols_pred"],
        x = pc_dat[, "k"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of components")

# NO. OBSERVATIONS ------------------------------------------------------------
matplot(y = n_dat[,  c("bic_pred", "pcr_pred", "pls_pred", "env_bic_pred")]
        / n_dat[, "ols_pred"],
        x = sqrt(n_dat[, "n"]),
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = expression(sqrt("Number of observations")))

matplot(y = n_dat[, c("ktrue_pred", "pcr_ktrue_pred", "pls_ktrue_pred",
                      "env_ktrue_pred")] / n_dat[, "ols_pred"],
        x = sqrt(n_dat[, "n"]),
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = expression(sqrt("Number of observations")))
dev.off()

###############################################################################
# FIGURE 3
###############################################################################
# Directory and name to save figure
out_name <- "~/GitHub/tpcr-suppl/figs/sims_fig_est_aic.pdf"
pdf(out_name, width = 12.5, height = 12)
par(cex.axis = 1.3, cex.lab = 1.3)
par(mfrow = c(5, 3))
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.5,1.1))

# COEF SCALE ------------------------------------------------------------------
matplot(y = coef_dat[, c("aic", "pcr", "pls", "env_aic")] / coef_dat[, "ols"],
        x = coef_dat[, "coef_scale"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Coefficient column norm",
        main = "Unknown k")
legend("bottomright", legend = c("TPCR", "PCR", "PLS", "XENV"), 
       lty = all_lty, col = all_col, bty = "n", lwd = 2)

matplot(y = coef_dat[, c("ktrue", "pcr_ktrue", "pls_ktrue",
                         "env_ktrue")] / coef_dat[, "ols"],
        x = coef_dat[, "coef_scale"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Coefficient column norm",
        main = "Known k")

matplot(y = coef_dat[, c("k_aic", "k_pcr", "k_pls", "k_env_aic")] - coef_dat[, "k"],
        x = coef_dat[, "coef_scale"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Coefficient column norm",
        main = "Selection of k")

# EGENVALUE SPIKES ------------------------------------------------------------
matplot(y = eval_dat[, c("aic", "pcr", "pls", "env_aic")] / eval_dat[, "ols"],
        x = eval_dat[, "d"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Average spiked eigenvalue")

matplot(y = eval_dat[, c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")]
        / eval_dat[, "ols"],
        x = eval_dat[, "d"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Average spiked eigenvalue")

matplot(y = eval_dat[, c("k_aic", "k_pcr", "k_pls", "k_env_aic")] - eval_dat[, "k"],
        x = eval_dat[, "d"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Average spiked eigenvalue")

# NO. PREDICTORS --------------------------------------------------------------
matplot(y = pred_dat[, c("aic", "pcr", "pls", "env_aic")] / pred_dat[, "ols"],
        x = pred_dat[, "p"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of predictors")

matplot(y = pred_dat[, c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / pred_dat[, "ols"],
        x = pred_dat[, "p"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of predictors")

matplot(y = pred_dat[, c("k_aic", "k_pcr", "k_pls", "k_env_aic")] - pred_dat[, "k"],
        x = pred_dat[, "p"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Number of predictors")

# NO. COMPONENTS --------------------------------------------------------------
matplot(y = pc_dat[, c("aic", "pcr", "pls", "env_aic")] / pc_dat[, "ols"],
        x = pc_dat[, "k"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of components")

matplot(y = pc_dat[, c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / pc_dat[, "ols"],
        x = pc_dat[, "k"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of components")

matplot(y = pc_dat[, c("k_aic", "k_pcr", "k_pls", "k_env_aic")] - pc_dat[, "k"],
        x = pc_dat[, "k"],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Number of components")

# NO. OBSERVATIONS ------------------------------------------------------------
matplot(y = n_dat[, c("aic", "pcr", "pls", "env_aic")] / n_dat[, "ols"],
        x = sqrt(n_dat[, "n"]),
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = expression(sqrt("Number of observations")))

matplot(y = n_dat[, c("ktrue", "pcr_ktrue", "pls_ktrue", "env_ktrue")] / n_dat[, "ols"],
        x = sqrt(n_dat[, "n"]),
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = expression(sqrt("Number of observations")))

matplot(y = n_dat[, c("k_aic", "k_pcr", "k_pls", "k_env_aic")] - n_dat[, "k"],
        x = sqrt(n_dat[, "n"]),
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = expression(sqrt("Number of observations")))
dev.off()
