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
out_name <- "~/GitHub/tpcr-suppl/figs/sims_fig.pdf"
pdf(out_name, width = 12.5, height = 12)
par(cex.axis = 1.3, cex.lab = 1.3)
par(mfrow = c(5, 3))
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.5,1.1))

# COEF SCALE ------------------------------------------------------------------
matplot(y = coef_dat[, c(2, 4, 6, 9)] / coef_dat[, 12],
        x = coef_dat[, 73],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Estimation RMSE",
        xlab = "Coefficient column norm",
        main = "Estimation")
legend("bottomright", legend = c("TPCR", "PCR", "PLS", "XENV"), 
       lty = all_lty, col = all_col, bty = "n", lwd = 2)

matplot(y = coef_dat[, c(14, 16, 18, 21)] / coef_dat[, 24],
        x = coef_dat[, 73],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Prediction RMSE",
        xlab = "Coefficient column norm",
        main = "Prediction")

matplot(y = coef_dat[, c(26, 27, 28, 30)] - 4,
        x = coef_dat[, 73],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Coefficient column norm",
        main = "Selection of k")

# EGENVALUE SPIKES ------------------------------------------------------------
matplot(y = eval_dat[, c(2, 4, 6, 9)] / eval_dat[, 12],
        x = eval_dat[, 69],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Estimation RMSE",
        xlab = "Average spiked eigenvalue")

matplot(y = eval_dat[, c(14, 16, 18, 21)] / eval_dat[, 24],
        x = eval_dat[, 69],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Prediction RMSE",
        xlab = "Average spiked eigenvalue")

matplot(y = eval_dat[, c(26, 27, 28, 30)] - 4,
        x = eval_dat[, 69],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Average spiked eigenvalue")

# NO. PREDICTORS --------------------------------------------------------------
matplot(y = pred_dat[, c(2, 4, 6, 9)] / pred_dat[, 12],
        x = pred_dat[, 64],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Estimation RMSE",
        xlab = "Number of predictors")

matplot(y = pred_dat[, c(14, 16, 18, 21)] / pred_dat[, 24],
        x = pred_dat[, 64],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Prediction RMSE",
        xlab = "Number of predictors")

matplot(y = pred_dat[, c(26, 27, 28, 30)] - 4,
        x = pred_dat[, 64],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Number of predictors")

# NO. COMPONENTS --------------------------------------------------------------
matplot(y = pc_dat[, c(2, 4, 6, 9)] / pc_dat[, 12],
        x = pc_dat[, 65],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Estimation RMSE",
        xlab = "Number of components")

matplot(y = pc_dat[, c(14, 16, 18, 21)] / pc_dat[, 24],
        x = pc_dat[, 65],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Prediction RMSE",
        xlab = "Number of components")

matplot(y = pc_dat[, c(26, 27, 28, 30)] - pc_dat[, 65],
        x = pc_dat[, 65],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Number of components")

# NO. OBSERVATIONS ------------------------------------------------------------
matplot(y = n_dat[, c(2, 4, 6, 9)] / n_dat[, 12],
        x = n_dat[, 63],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Estimation RMSE",
        xlab = "Number of observations")

matplot(y = n_dat[, c(14, 16, 18, 21)] / n_dat[, 24],
        x = n_dat[, 63],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Prediction RMSE",
        xlab = "Number of observations")

matplot(y = n_dat[, c(26, 27, 28, 30)] - 4,
        x = n_dat[, 63],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selected - true k",
        xlab = "Number of observations")
dev.off()

###############################################################################
# FIGURE 2
###############################################################################
# Directory and name to save figure
out_name <- "~/GitHub/tpcr-suppl/figs/sims_fig_true.pdf"
pdf(out_name, width = 12.5, height = 12)
par(cex.axis = 1.3, cex.lab = 1.3)
par(mfrow = c(5, 2))
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.5,1.1))

# COEF SCALE ------------------------------------------------------------------
matplot(y = coef_dat[, c(3, 5, 7, 11)] / coef_dat[, 12],
        x = coef_dat[, 73],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Estimation RMSE",
        xlab = "Coefficient column norm",
        main = "Estimation")
legend("topleft", legend = c("TPCR", "PCR", "PLS", "XENV"), 
       lty = all_lty, col = all_col, bty = "n", lwd = 2)

matplot(y = coef_dat[, c(15, 17, 19, 23)] / coef_dat[, 24],
        x = coef_dat[, 73],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Prediction RMSE",
        xlab = "Coefficient column norm",
        main = "Prediction")
# EGENVALUE SPIKES ------------------------------------------------------------
matplot(y = eval_dat[, c(3, 5, 7, 11)] / eval_dat[, 12],
        x = eval_dat[, 69],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Estimation RMSE",
        xlab = "Average spiked eigenvalue")

matplot(y = eval_dat[, c(15, 17, 19, 23)] / eval_dat[, 24],
        x = eval_dat[, 69],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Prediction RMSE",
        xlab = "Average spiked eigenvalue")

# NO. PREDICTORS --------------------------------------------------------------
matplot(y = pred_dat[, c(3, 5, 7, 11)] / pred_dat[, 12],
        x = pred_dat[, 64],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Estimation RMSE",
        xlab = "Number of predictors")

matplot(y = pred_dat[, c(15, 17, 19, 23)] / pred_dat[, 24],
        x = pred_dat[, 64],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Prediction RMSE",
        xlab = "Number of predictors")

# NO. COMPONENTS --------------------------------------------------------------
matplot(y = pc_dat[, c(3, 5, 7, 11)] / pc_dat[, 12],
        x = pc_dat[, 65],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Estimation RMSE",
        xlab = "Number of components")

matplot(y = pc_dat[, c(15, 17, 19, 23)] / pc_dat[, 24],
        x = pc_dat[, 65],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Prediction RMSE",
        xlab = "Number of components")

# NO. OBSERVATIONS ------------------------------------------------------------
matplot(y = n_dat[, c(3, 5, 7, 11)] / n_dat[, 12],
        x = n_dat[, 63],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Estimation RMSE",
        xlab = "Number of observations")

matplot(y = n_dat[, c(15, 17, 19, 23)] / n_dat[, 24],
        x = n_dat[, 63],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Prediction RMSE",
        xlab = "Number of observations")
dev.off()
