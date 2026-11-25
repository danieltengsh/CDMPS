# ==================== 实证研究配置 ====================
PROJECT_NAME <- "FC-DINA模型实证研究 - 代数应用题"
AUTHOR <- "Your Name"
VERSION <- "1.0"
DATE <- format(Sys.Date(), "%Y-%m-%d")

# 加载必要的包
required_packages <- c("ggplot2", "rjags", "coda", "writexl", "patchwork", 
                       "dplyr", "tidyr")
invisible(lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}))

set.seed(1234)

# ==================== 实证数据维度 ====================
N_ITEMS <- 20                 # 题目数
N_PERSONS <- 3527             # 最大可能被试数
N_TEMPLATES <- 4               # 模板数（根据qmatrix的列数）
N_ATTRIBUTES <- 4              # 属性数（等于模板数）
N_FEATURES <- 3                # 特征数

# ==================== 固定参数设置 ====================
# 猜测参数 - 实证研究中通常设为固定值或估计
GUESSING <- rep(0.2, N_ITEMS)  # 可改为根据题目类型设定不同值

# ==================== 模型参数设置 ====================
MODEL_CONFIG <- list(
  n_chains = 2,
  n_adapt = 2000,
  n_burnin = 5000,
  n_iter_basic = 5000,
  n_iter_items = 5000,
  n_iter_alpha = 3000,
  thin_basic = 2,
  thin_items = 2,
  thin_alpha = 5
)

# 创建输出目录
create_directories <- function() {
  dirs <- c("results", "results/tables", "results/plots", "results/data")
  for(dir in dirs) {
    if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  }
}

create_directories()
cat("配置加载完成\n")