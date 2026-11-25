# ==================== FC-DINA模型实证研究主控脚本 ====================

source("00_config.R")

cat("========================================\n")
cat("   FC-DINA模型实证研究\n")
cat("   代数应用题测验数据分析\n")
cat("========================================\n\n")

# 步骤1: 加载数据
cat("步骤1: 加载实证数据\n")
source("01_load_data.R")
empirical_data <- load_empirical_data()

# 步骤2: 运行模型
cat("\n步骤2: 运行FC-DINA模型\n")
source("02_run_model_empirical.R")
samples <- run_fc_dina_empirical(empirical_data)

# 步骤3: 导出结果表格
cat("\n步骤3: 导出结果表格\n")
source("03_export_results.R")
results <- export_empirical_results(samples, empirical_data)

# 步骤4: 生成图表
cat("\n步骤4: 生成图表\n")
source("04_generate_plots_empirical.R")
plots <- generate_empirical_plots(samples, empirical_data)

cat("\n========================================\n")
cat("   实证研究完成！\n")
cat("   结果位置: results/\n")
cat("========================================\n")