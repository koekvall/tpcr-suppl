# SETTINGS --------------------------------------------------------------------
library(RColorBrewer)
cbbPalette <- brewer.pal(n = 5, name = "Dark2")

# Directory of data to plot
data_dir <- "~/GitHub/tpcr-suppl/sims/"

# Directory and name to save figure
out_name <- "~/GitHub/tpcr-suppl/figs/sims_fig.pdf"


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

aic_pch <- 1
bic_pch <- 2
cv_pch <- 3
lrt_pch <- 4

all_col <- c(our_col, pcr_col, pls_col, env_col)
all_lty <- c(our_lty, pcr_lty, pls_lty, env_lty)
#all_pch <- c(aic_pch, bic_pch, cv_pch, cv_pch, aic_pch, bic_pch, lrt_pch, 0)
all_type <- rep("l", 4)


pdf(out_name, width = 12.5, height = 12)
par(cex.axis = 1.3, cex.lab = 1.3)
par(mfrow = c(5, 3))
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.5,1.1))

# COEF SCALE ------------------------------------------------------------------
matplot(y = coef_dat[, c(2:4, 6)] / coef_dat[, 8],
        x = coef_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Coefficient column norm",
        main = "Estimation")
legend("bottomright", legend = c("TPCR", "PCR", "PLS", "XENV"), 
       lty = all_lty, col = all_col, bty = "n", lwd = 2)

matplot(y = coef_dat[, c(10:12, 14)] / coef_dat[, 16],
        x = coef_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Coefficient column norm",
        main = "Prediction")

matplot(y = coef_dat[, c(18:20, 22)] - 4,
        x = coef_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selection bias",
        xlab = "Coefficient column norm",
        main = "Selection of k")

# EGENVALUE SPIKES ------------------------------------------------------------
matplot(y = eval_dat[, c(2:4, 6)] / eval_dat[, 8],
        x = eval_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Average spiked eigenvalue")

matplot(y = eval_dat[, c(10:12, 14)] / eval_dat[, 16],
        x = eval_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Average spiked eigenvalue")

matplot(y = eval_dat[, c(18:20, 22)] - 4,
        x = eval_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selection bias",
        xlab = "Average spiked eigenvalue")

# NO. PREDICTORS --------------------------------------------------------------
matplot(y = pred_dat[, c(2:4, 6)] / pred_dat[, 8],
        x = pred_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of predictors")

matplot(y = pred_dat[, c(10:12, 14)] / pred_dat[, 16],
        x = pred_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of predictors")

matplot(y = pred_dat[, c(18:20, 22)] - 4,
        x = pred_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selection bias",
        xlab = "Number of predictors")

# NO. COMPONENTS --------------------------------------------------------------
matplot(y = pc_dat[, c(2:4, 6)] / pc_dat[, 8],
        x = pc_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of components")

matplot(y = pc_dat[, c(10:12, 14)] / pc_dat[, 16],
        x = pc_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of components")

matplot(y = pc_dat[, c(18:20, 22)] - floor(seq(1, 20, length.out = 5)),
        x = pc_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selection bias",
        xlab = "Number of components")

# NO. OBSERVATIONS ------------------------------------------------------------
matplot(y = n_dat[, c(2:4, 6)] / n_dat[, 8],
        x = n_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of observations")

matplot(y = n_dat[, c(10:12, 14)] / n_dat[, 16],
        x = n_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Relative RMSE",
        xlab = "Number of observations")

matplot(y = n_dat[, c(18:20, 22)] - 4,
        x = n_dat[, 49],
        type = all_type,
        lwd = 2,
        lty = all_lty,
        col = all_col,
        ylab = "Selection bias",
        xlab = "Number of observations")
dev.off()

