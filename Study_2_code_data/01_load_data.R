# ==================== 加载实证数据 ====================

source("00_config.R")

load_empirical_data <- function() {
  cat("=== 加载实证数据 ===\n")
  
  # 1. 读取作答矩阵
  cat("1. 读取作答矩阵...\n")
  response_raw <- read.csv("data/response.csv", header = FALSE)
  RESPONSE <- as.matrix(response_raw)
  N_PERSONS_ACTUAL <- nrow(RESPONSE)
  N_ITEMS_ACTUAL <- ncol(RESPONSE)
  
  cat(sprintf("   实际被试数: %d\n", N_PERSONS_ACTUAL))
  cat(sprintf("   实际题目数: %d\n", N_ITEMS_ACTUAL))
  
  # 检查缺失值
  missing_rate <- sum(is.na(RESPONSE)) / length(RESPONSE)
  cat(sprintf("   缺失率: %.2f%%\n", missing_rate * 100))
  
  # 2. 读取Q矩阵
  cat("2. 读取Q矩阵...\n")
  Q_raw <- read.csv("data/qmatrix.csv", header = FALSE)
  Q <- as.matrix(Q_raw)
  
  # 验证Q矩阵维度
  if(nrow(Q) != N_ITEMS_ACTUAL) {
    stop("Q矩阵行数不等于题目数")
  }
  
  # 3. 读取特征矩阵
  cat("3. 读取特征矩阵...\n")
  C_raw <- read.csv("data/features.csv", header = TRUE, row.names = 1)
  C <- as.matrix(C_raw)
  
  # 验证特征矩阵维度
  if(nrow(C) != N_ITEMS_ACTUAL) {
    stop("特征矩阵行数不等于题目数")
  }
  
  # 4. 读取题目难度（2PL估计）
  cat("4. 读取题目难度...\n")
  difficulty_raw <- read.csv("data/difficulty.csv", header = TRUE)
  DIFFICULTY <- difficulty_raw$difficulty  # 假设列名为difficulty
  
  if(length(DIFFICULTY) != N_ITEMS_ACTUAL) {
    stop("难度向量长度不等于题目数")
  }
  
  # 5. 读取题目元数据
  cat("5. 读取题目元数据...\n")
  item_info <- read.csv("data/item_info.csv", header = TRUE)
  
  # 汇总信息
  cat("\n=== 数据加载完成 ===\n")
  cat(sprintf("被试数: %d\n", N_PERSONS_ACTUAL))
  cat(sprintf("题目数: %d\n", N_ITEMS_ACTUAL))
  cat(sprintf("属性数: %d\n", ncol(Q)))
  cat(sprintf("特征数: %d\n", ncol(C)))
  
  # 各模板题目分布
  template_dist <- table(item_info$template)
  cat("\n各模板题目数:\n")
  print(template_dist)
  
  # 典型题/变式题分布
  typical_dist <- table(item_info$is_typical)
  cat("\n典型题 vs 变式题:\n")
  print(typical_dist)
  
  # 返回数据列表
  return(list(
    response = RESPONSE,
    Q = Q,
    C = C,
    difficulty = DIFFICULTY,
    item_info = item_info,
    N = N_PERSONS_ACTUAL,
    J = N_ITEMS_ACTUAL,
    K = ncol(Q),
    F = ncol(C)
  ))
}

# 执行加载
if (sys.nframe() == 0) {
  empirical_data <- load_empirical_data()
  list2env(empirical_data, globalenv())
  cat("\n数据已加载到全局环境\n")
}