# ==================== 收敛诊断 ====================
source("00_config.R")
library(coda)
library(ggplot2)

load("results/data/empirical_results.RData")
dir.create("results/plots", recursive = TRUE, showWarnings = FALSE)

# ---- 1. 先看每个 mcmc.list 的维度 ----
for (nm in c("samples_basic", "samples_items", "samples_alpha")) {
  ml <- samples[[nm]]
  cat("\n===", nm, "===\n")
  cat("链数:", length(ml), "\n")
  cat("每链迭代:", niter(ml), " 参数数:", nvar(ml), "\n")
  cat("参数名(前6):", paste(head(varnames(ml), 6), collapse = ", "), "\n")
}

# ---- 2. Gelman-Rubin R-hat（点估计 + 上置信界） ----
# 经验阈值：点估计 < 1.01 收敛良好；< 1.05 可接受；> 1.1 需加长链
gd_items <- gelman.diag(samples$samples_items, multivariate = FALSE)
gd_basic <- gelman.diag(samples$samples_basic, multivariate = FALSE)
gd_alpha <- gelman.diag(samples$samples_alpha, multivariate = FALSE)

cat("\n-- R-hat (samples_items) --\n"); print(round(gd_items$psrf, 4))
cat("\n-- R-hat (samples_basic) --\n"); print(round(gd_basic$psrf, 4))
# alpha 参数很多，只汇总
psrf_a <- gd_alpha$psrf[, 1]
cat("\nalpha 的 R-hat:  max =", round(max(psrf_a), 4),
    "  >1.05 的个数 =", sum(psrf_a > 1.05), "/", length(psrf_a), "\n")

# 存表
rhat_df <- rbind(
  data.frame(param = rownames(gd_basic$psrf), Rhat = gd_basic$psrf[,1],
             upper = gd_basic$psrf[,2], group = "basic"),
  data.frame(param = rownames(gd_items$psrf), Rhat = gd_items$psrf[,1],
             upper = gd_items$psrf[,2], group = "items"),
  data.frame(param = rownames(gd_alpha$psrf), Rhat = gd_alpha$psrf[,1],
             upper = gd_alpha$psrf[,2], group = "alpha")
)
write.csv(rhat_df, "results/Rhat_summary.csv", row.names = FALSE)

# ---- 3. R-hat 图（只画 basic + items，alpha 太多） ----
p_rhat <- ggplot(subset(rhat_df, group != "alpha"),
                 aes(x = reorder(param, Rhat), y = Rhat,
                     ymin = Rhat, ymax = upper, color = group)) +
  geom_pointrange(size = 0.4) +
  geom_hline(yintercept = 1.01, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 1.05, linetype = "dotted", color = "red") +
  coord_flip() +
  labs(title = "Gelman-Rubin R-hat 收敛诊断",
       subtitle = "蓝虚线=1.01(良好), 红点线=1.05(可接受)",
       x = NULL, y = "R-hat") +
  theme_minimal(base_size = 10)
ggsave("results/plots/收敛图_Rhat.png", p_rhat, width = 8, height = 10)

# ---- 4. 有效样本量 ESS ----
ess_items <- effectiveSize(samples$samples_items)
ess_basic <- effectiveSize(samples$samples_basic)
cat("\nESS (basic):\n"); print(round(ess_basic, 1))
cat("\nESS (items): min =", round(min(ess_items),1),
    " median =", round(median(ess_items),1), "\n")

# ---- 5. Trace plot：挑几个参数，2 条链分开画 ----
# 挑 items 里 s[1..4] 和 basic 里 phi[1..2]（按你实际列名改）
trace_pick <- function(ml, params, title) {
  # mcmc.list -> 长表：链/迭代/参数/值
  dfs <- lapply(seq_along(ml), function(ch) {
    m <- as.matrix(ml[[ch]])[, params, drop = FALSE]
    data.frame(iter = seq_len(nrow(m)),
               chain = factor(paste0("链", ch)),
               m,
               check.names = FALSE) |>
      tidyr::pivot_longer(cols = all_of(params),
                          names_to = "param", values_to = "value")
  })
  df <- do.call(rbind, dfs)
  ggplot(df, aes(x = iter, y = value, color = chain)) +
    geom_line(alpha = 0.6, linewidth = 0.3) +
    facet_wrap(~param, ncol = 2, scales = "free_y") +
    labs(title = title, x = "迭代", y = "参数值") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
}

vn_s   <- grep("^s\\[",   varnames(samples$samples_items), value = TRUE)
vn_phi <- grep("^phi\\[", varnames(samples$samples_basic), value = TRUE)

p_tr_s   <- trace_pick(samples$samples_items, vn_s[1:min(4, length(vn_s))],
                       "Trace: 失误参数 s_j (前4个)")
p_tr_phi <- trace_pick(samples$samples_basic, vn_phi[1:min(4, length(vn_phi))],
                       "Trace: 特征权重 φ_k (前4个)")

ggsave("results/plots/收敛图_trace_s.png",   p_tr_s,   width = 10, height = 6)
ggsave("results/plots/收敛图_trace_phi.png", p_tr_phi, width = 10, height = 6)

# ---- 6. 自相关图（1 张代表） ----
png("results/plots/收敛图_自相关.png", width = 1000, height = 600)
par(mfrow = c(2, 2))
autocorr.plot(samples$samples_items[, vn_s[1]], auto.layout = FALSE,
              main = paste("自相关:", vn_s[1]))
autocorr.plot(samples$samples_basic[, vn_phi[1]], auto.layout = FALSE,
              main = paste("自相关:", vn_phi[1]))
dev.off()

# ---- 7. 密度图：2 链叠加，看是否一致 ----
df_dens <- lapply(seq_along(samples$samples_items), function(ch) {
  m <- as.matrix(samples$samples_items[[ch]])[, vn_s[1:2], drop = FALSE]
  data.frame(chain = factor(paste0("链", ch)), m, check.names = FALSE) |>
    tidyr::pivot_longer(-chain, names_to = "param", values_to = "value")
}) |> (\(x) do.call(rbind, x))()

p_dens <- ggplot(df_dens, aes(value, color = chain, fill = chain)) +
  geom_density(alpha = 0.25) +
  facet_wrap(~param, scales = "free") +
  labs(title = "后验密度（2 链叠加）", x = "参数值", y = "密度") +
  theme_minimal()
ggsave("results/plots/收敛图_后验密度.png", p_dens, width = 8, height = 4)

cat("\n✅ 收敛诊断图已生成到 results/plots/\n")
cat("   R-hat 汇总表: results/Rhat_summary.csv\n")