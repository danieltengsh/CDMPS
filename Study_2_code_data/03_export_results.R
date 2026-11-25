# ==================== 导出实证研究结果 ====================

source("00_config.R")

export_empirical_results <- function(samples, empirical_data) {
  
  cat("=== 导出实证研究结果 ===\n")
  
  # 提取样本
  basic_mat <- as.matrix(samples$samples_basic)
  items_mat <- as.matrix(samples$samples_items)
  alpha_mat <- as.matrix(samples$samples_alpha)
  
  # ==================== 1. 截距和特征权重 ====================
  cat("生成特征权重表...\n")
  
  # 提取样本
  basic_mat <- as.matrix(samples$samples_basic)
  
  # 确保 basic_mat 有列名
  if(is.null(colnames(basic_mat))) {
    stop("样本矩阵没有列名")
  }
  
  # c0 - 可能的不同名称
  c0_cols <- grep("^c0$|^c0\\[", colnames(basic_mat), value = TRUE)
  if(length(c0_cols) == 0) {
    cat("警告: 未找到 c0 参数\n")
    c0_est <- NA
    c0_sd <- NA
    c0_ci <- c(NA, NA)
  } else {
    c0_est <- median(basic_mat[, c0_cols[1]])
    c0_sd <- sd(basic_mat[, c0_cols[1]])
    c0_ci <- quantile(basic_mat[, c0_cols[1]], c(0.025, 0.975))
  }
  
  # phi - 特征权重
  phi_results <- data.frame()
  for(f in 1:empirical_data$F) {
    # 尝试不同的命名方式
    param_names <- c(paste0("phi[", f, "]"), paste0("phi.", f), paste0("phi", f))
    param_name <- param_names[param_names %in% colnames(basic_mat)][1]
    
    if(is.na(param_name)) {
      cat(sprintf("警告: 未找到特征%d的参数\n", f))
      next
    }
    
    est <- median(basic_mat[, param_name])
    sd_val <- sd(basic_mat[, param_name])
    ci <- quantile(basic_mat[, param_name], c(0.025, 0.975))
    
    # 使用临时数据框，确保列名一致
    temp_df <- data.frame(
      参数 = paste0("特征", f),
      估计值 = round(est, 4),
      标准误 = round(sd_val, 4),
      下限 = round(ci[1], 4),
      上限 = round(ci[2], 4),
      stringsAsFactors = FALSE
    )
    
    # 如果是第一个特征，直接赋值；否则用 rbind
    if(f == 1) {
      phi_results <- temp_df
    } else {
      phi_results <- rbind(phi_results, temp_df)
    }
  }
  
  # sigma_epsilon
  sigma_cols <- grep("sigma_epsilon|sigma", colnames(basic_mat), value = TRUE)
  if(length(sigma_cols) == 0) {
    cat("警告: 未找到 sigma_epsilon 参数\n")
    sigma_est <- NA
    sigma_sd <- NA
    sigma_ci <- c(NA, NA)
  } else {
    sigma_est <- median(basic_mat[, sigma_cols[1]])
    sigma_sd <- sd(basic_mat[, sigma_cols[1]])
    sigma_ci <- quantile(basic_mat[, sigma_cols[1]], c(0.025, 0.975))
  }
  
  # 汇总 - 确保所有数据框列名一致
  c0_df <- data.frame(
    参数 = "c₀",
    估计值 = round(c0_est, 4),
    标准误 = round(c0_sd, 4),
    下限 = round(c0_ci[1], 4),
    上限 = round(c0_ci[2], 4),
    stringsAsFactors = FALSE
  )
  
  sigma_df <- data.frame(
    参数 = "σ_ε",
    估计值 = round(sigma_est, 4),
    标准误 = round(sigma_sd, 4),
    下限 = round(sigma_ci[1], 4),
    上限 = round(sigma_ci[2], 4),
    stringsAsFactors = FALSE
  )
  
  # 合并 - 确保列名一致
  param_table <- rbind(c0_df, phi_results, sigma_df)
  
  write.csv(param_table, "results/tables/特征权重估计.csv", row.names = FALSE)
  
  # ==================== 2. 模板难度 ====================
  cat("生成模板难度表...\n")
  
  beta_results <- data.frame()
  for(k in 1:empirical_data$K) {
    param_name <- paste0("beta_k[", k, "]")
    est <- median(basic_mat[, param_name])
    sd <- sd(basic_mat[, param_name])
    ci <- quantile(basic_mat[, param_name], c(0.025, 0.975))
    
    beta_results <- rbind(beta_results, data.frame(
      模板 = paste0("模板", k),
      估计值 = round(est, 4),
      标准误 = round(sd, 4),
      下限 = round(ci[1], 4),
      上限 = round(ci[2], 4)
    ))
  }
  
  write.csv(beta_results, "results/tables/模板难度估计.csv", row.names = FALSE)
  
  # ==================== 3. 题目失误参数 ====================
  cat("生成题目失误参数表...\n")
  
  s_cols <- grep("^s\\[", colnames(items_mat), value = TRUE)
  s_est <- apply(items_mat[, s_cols], 2, median)
  s_sd <- apply(items_mat[, s_cols], 2, sd)
  
  # 按题目排序
  s_numbers <- as.numeric(gsub("s\\[|\\]", "", names(s_est)))
  order_idx <- order(s_numbers)
  
  s_table <- data.frame(
    题目 = s_numbers[order_idx],
    模板 = empirical_data$item_info$template[order_idx],
    类型 = ifelse(empirical_data$item_info$is_typical[order_idx] == 1, "典型题", "变式题"),
    难度 = empirical_data$difficulty[order_idx],
    失误率 = round(s_est[order_idx], 4),
    标准误 = round(s_sd[order_idx], 4)
  )
  
  write.csv(s_table, "results/tables/题目失误参数.csv", row.names = FALSE)
  
  # ==================== 4. 核心假设验证 ====================
  cat("验证核心假设：难度-失误率相关\n")
  
  template_cor <- data.frame()
  for(t in 1:empirical_data$K) {
    idx <- which(empirical_data$item_info$template == t)
    cor_val <- cor(empirical_data$difficulty[idx], s_est[idx])
    
    template_cor <- rbind(template_cor, data.frame(
      模板 = paste0("模板", t),
      相关系数 = round(cor_val, 4),
      题目数 = length(idx)
    ))
  }
  
  write.csv(template_cor, "results/tables/模板内难度-失误相关.csv", row.names = FALSE)
  
  # ==================== 5. 学生知识状态 ====================
  cat("生成学生知识状态摘要...\n")
  
  alpha_cols <- grep("^alpha\\[", colnames(alpha_mat), value = TRUE)
  alpha_eap <- matrix(apply(alpha_mat[, alpha_cols], 2, mean), 
                      nrow = empirical_data$N, 
                      ncol = empirical_data$K)
  alpha_est <- ifelse(alpha_eap > 0.5, 1, 0)
  
  # 各属性掌握比例
  mastery_rate <- colMeans(alpha_est)
  mastery_table <- data.frame(
    属性 = paste0("属性", 1:empirical_data$K),
    掌握人数 = round(mastery_rate * empirical_data$N),
    掌握比例 = paste0(round(mastery_rate * 100, 2), "%")
  )
  
  write.csv(mastery_table, "results/tables/属性掌握比例.csv", row.names = FALSE)
  
  # ==================== 6. 保存Excel ====================
  library(writexl)
  
  excel_data <- list(
    "特征权重" = param_table,
    "模板难度" = beta_results,
    "题目失误参数" = s_table,
    "难度-失误相关" = template_cor,
    "属性掌握比例" = mastery_table
  )
  
  filename <- paste0("results/tables/FC-DIMA实证结果_", format(Sys.time(), "%Y%m%d"), ".xlsx")
  write_xlsx(excel_data, filename)
  
  cat("✅ 结果已导出到:", filename, "\n")
  
  return(list(
    param_table = param_table,
    beta_results = beta_results,
    s_table = s_table,
    template_cor = template_cor,
    mastery_table = mastery_table
  ))
}

# 执行
if (sys.nframe() == 0) {
  if (!exists("samples") || !exists("empirical_data")) {
    load("results/data/empirical_results.RData")
  }
  
  results <- export_empirical_results(samples, empirical_data)
}