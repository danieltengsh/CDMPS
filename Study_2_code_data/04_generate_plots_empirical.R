# ==================== 实证研究图表 ====================

source("00_config.R")

generate_empirical_plots <- function(samples, empirical_data) {
  
  cat("=== 生成实证研究图表 ===\n")
  
  # 提取数据
  items_mat <- as.matrix(samples$samples_items)
  s_cols <- grep("^s\\[", colnames(items_mat), value = TRUE)
  s_est <- apply(items_mat[, s_cols], 2, median)
  s_numbers <- as.numeric(gsub("s\\[|\\]", "", names(s_est)))
  s_est <- s_est[order(s_numbers)]
  
  # ==================== 图1: 各模板难度分布 ====================
  cat("生成难度分布图...\n")
  
  df_difficulty <- data.frame(
    题目 = 1:empirical_data$J,
    模板 = factor(empirical_data$item_info$template),
    难度 = empirical_data$difficulty,
    类型 = ifelse(empirical_data$item_info$is_typical == 1, "典型题", "变式题")
  )
  
  p1 <- ggplot(df_difficulty, aes(x = factor(模板), y = 难度, color = 类型)) +
    geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 5, 
                 color = "black", aes(group = 模板)) +
    labs(title = "各模板题目难度分布",
         x = "模板编号", y = "题目难度 (2PL估计)") +
    theme_minimal() +
    scale_color_manual(values = c("典型题" = "blue", "变式题" = "red"))
  
  ggsave("results/plots/各模板难度分布.png", p1, width = 8, height = 5)
  
  # ==================== 图2: 各模板难度-失误率散点图 ====================
  cat("生成难度-失误率散点图...\n")
  
  df_slip <- data.frame(
    Item       = 1:empirical_data$J,
    Template   = factor(empirical_data$item_info$template),
    Difficulty = empirical_data$difficulty,
    Slip       = s_est,
    Type       = ifelse(empirical_data$item_info$is_typical == 1,
                        "Reference item", "Variant item")
  )
  
  p2 <- ggplot(df_slip, aes(x = Difficulty, y = Slip,
                            color = Template, shape = Type)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                linetype = "dashed", alpha = 0.5) +
    facet_wrap(~ Template, ncol = 2, scales = "free",
               labeller = as_labeller(function(x) paste0("Template ", x))) +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.001)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.001)) +
    labs(x = "Item difficulty", y = "Slipping parameter") +
    theme_minimal(base_family = "sans", base_size = 11) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(size = 10),
      axis.title       = element_text(size = 11),
      axis.text        = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
  
  ggsave("results/plots/difficulty_vs_slip.pdf", p2,
         width = 10, height = 7, device = cairo_pdf)
  cat("  ✓ 已生成: difficulty_vs_slip.pdf\n")
  
  # ==================== 图3: 特征权重系数图 ====================
  cat("生成特征权重系数图...\n")
  
  basic_mat <- as.matrix(samples$samples_basic)
  phi_df <- data.frame()
  for(f in 1:empirical_data$F) {
    param_name <- paste0("phi[", f, "]")
    phi_df <- rbind(phi_df, data.frame(
      特征 = paste0("特征", f),
      估计值 = median(basic_mat[, param_name]),
      下限 = quantile(basic_mat[, param_name], 0.025),
      上限 = quantile(basic_mat[, param_name], 0.975)
    ))
  }
  
  p3 <- ggplot(phi_df, aes(x = 特征, y = 估计值)) +
    geom_point(size = 4, color = "steelblue") +
    geom_errorbar(aes(ymin = 下限, ymax = 上限), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "特征权重系数估计 (95% CI)",
         y = "φ 系数估计值") +
    theme_minimal()
  
  ggsave("results/plots/特征权重系数.png", p3, width = 6, height = 5)
  
  # ==================== 图4: 属性掌握比例 ====================
  cat("生成属性掌握比例图...\n")
  
  alpha_mat <- as.matrix(samples$samples_alpha)
  alpha_cols <- grep("^alpha\\[", colnames(alpha_mat), value = TRUE)
  alpha_eap <- matrix(apply(alpha_mat[, alpha_cols], 2, mean), 
                      nrow = empirical_data$N, 
                      ncol = empirical_data$K)
  mastery_rate <- colMeans(alpha_eap > 0.5)
  
  df_mastery <- data.frame(
    属性 = factor(paste0("属性", 1:empirical_data$K), 
                levels = paste0("属性", 1:empirical_data$K)),
    掌握比例 = mastery_rate
  )
  
  p4 <- ggplot(df_mastery, aes(x = 属性, y = 掌握比例, fill = 属性)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(掌握比例*100, 1), "%")), 
              vjust = -0.5) +
    labs(title = "各属性掌握比例",
         y = "掌握学生比例") +
    theme_minimal() +
    theme(legend.position = "none") +
    ylim(0, 1)
  
  ggsave("results/plots/属性掌握比例.png", p4, width = 7, height = 5)
  
  # ==================== 图5: 典型题 vs 变式题失误率对比 ====================
  cat("生成典型题vs变式题对比图...\n")
  
  p5 <- ggplot(df_slip, aes(x = 类型, y = 失误率, fill = 类型)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "典型题 vs 变式题失误率对比",
         x = "", y = "失误参数 s_j") +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave("results/plots/典型题vs变式题.png", p5, width = 5, height = 5)
  
  # ==================== 组合图 ====================
  library(patchwork)
  
  combined <- (p1 | p4) / (p2 | p3) / p5 +
    plot_annotation(title = "FC-DINA模型实证研究结果",
                    subtitle = "基于代数应用题测验数据")
  
  ggsave("results/plots/综合报告图.png", combined, width = 14, height = 12)
  
  cat("✅ 所有图表已生成: results/plots/\n")
  
  return(list(
    p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5
  ))
}

# 执行
if (sys.nframe() == 0) {
  if (!exists("samples") || !exists("empirical_data")) {
    load("results/data/empirical_results.RData")
  }
  
  plots <- generate_empirical_plots(samples, empirical_data)
}