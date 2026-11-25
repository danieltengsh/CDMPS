# ==================== FC-DINA模型实证运行 ====================

source("00_config.R")
source("01_load_data.R")

run_fc_dina_empirical <- function(empirical_data) {
  
  cat("=== 运行FC-DINA模型（实证数据）===\n")
  
  # ==================== 准备JAGS数据 ====================
  cat("准备JAGS数据，处理缺失值...\n")
  
  # 获取原始作答矩阵
  X_raw <- empirical_data$response
  
  # 创建观测值矩阵（将NA替换为0，这些位置在模型中不会真的被用）
  X_obs <- X_raw
  #X_obs[is.na(X_obs)] <- 0
  
  # 检查缺失情况
  cat("缺失值数量:", sum(is.na(X_raw)), "\n")
  cat("缺失比例:", round(mean(is.na(X_raw)) * 100, 2), "%\n")
  
  jags_data <- list(
    N = empirical_data$N,                    # 实际被试数 (207)
    J = empirical_data$J,                     # 题目数 (20)
    K = empirical_data$K,                      # 属性数/模板数 (4)
    F = empirical_data$F,                      # 特征数 (2)
    X_obs = X_obs,                             # 观测值矩阵（NA替换为0）
    Q = empirical_data$Q,                       # Q矩阵 (20×4)
    C = empirical_data$C,                       # 特征矩阵 (20×2)
    g_fixed = rep(0.05, empirical_data$J),         # 猜测参数固定为0
    beta_k_true = rep(0, empirical_data$K)      # 模板难度先验均值
  )
  
  # ==================== 用数据计算合理的初始值 ====================
  cat("\n=== 用数据计算初始值 ===\n")
  
  # 计算每道题的答对率（忽略缺失值）
  p_j <- colMeans(empirical_data$response, na.rm = TRUE)
  cat("各题答对率:\n")
  print(round(p_j, 3))
  
  # 答对率高 → s 应该小；答对率低 → s 应该大
  # 因为猜测参数为0，所以 s_j 应该接近 1 - p_j
  target_s <- 1 - p_j
  
  # 避免 0 或 1 导致 logit 无限大
  target_s <- pmax(pmin(target_s, 0.99), 0.01)
  
  # 计算 logit(s) = log(s/(1-s))
  logit_s <- log(target_s / (1 - target_s))
  cat("\n目标 logit(s) 值:\n")
  print(round(logit_s, 3))
  
  # 粗略估计 c0 应该在 logit_s 的均值附近
  # 因为 logit_s[j] = 1.7 * (c0 + inprod(phi, C[j,]) + epsilon[j])
  # 初始时假设 phi=0, epsilon=0，所以 c0 ≈ logit_s / 1.7
  c0_init <- mean(logit_s) / 1.7
  cat("\n建议的 c0 初始值:", round(c0_init, 3), "\n")
  
  # ==================== 随机 alpha 初始值 ====================
  cat("\n=== 使用随机 alpha 初始值 ===\n")
  
  # 随机生成 alpha，每个属性50%概率掌握
  set.seed(123)  # 固定种子，保证可重复
  alpha_init <- matrix(rbinom(jags_data$N * jags_data$K, 1, 0.5), 
                       nrow = jags_data$N, ncol = jags_data$K)
  
  cat("alpha 初始值中 1 的比例:", mean(alpha_init), "\n")
  cat("alpha 初始值维度:", dim(alpha_init), "\n")
  
  # ==================== 完整模型初始值 ====================
  inits <- list(
    list(
      c0 = c0_init,
      phi = rep(0, jags_data$F),
      tau_epsilon = 0.1,
      beta_k = rep(0, jags_data$K),
      theta = rep(0, jags_data$N),
      alpha = alpha_init  # 保守的 alpha
    ),
    list(
      c0 = c0_init + 0.3,
      phi = rep(0.1, jags_data$F),
      tau_epsilon = 0.15,
      beta_k = rep(0.1, jags_data$K),
      theta = rnorm(jags_data$N, 0, 0.1),
      alpha = alpha_init  # 保守的 alpha
    )
  )
  
  # 打印初始值摘要
  cat("\n初始值设置完成\n")
  cat("c0 链1:", inits[[1]]$c0, "链2:", inits[[2]]$c0, "\n")
  cat("phi 链1:", inits[[1]]$phi, "链2:", inits[[2]]$phi, "\n")
  
  # ==================== 监控的参数 ====================
  params <- c("c0", "phi", "sigma_epsilon", "beta_k", "theta", "s", "alpha", "avg_s")
  
  # ==================== 创建模型 ====================
  cat("创建JAGS模型...\n")
  model <- jags.model(
    file = "models/fc_dina_model.jags",  # 您的模型文件
    data = jags_data,
    inits = inits,
    n.chains = MODEL_CONFIG$n_chains,
    n.adapt = MODEL_CONFIG$n_adapt
  )
  
  # ==================== 预热 ====================
  cat("预热阶段...\n")
  update(model, n.iter = MODEL_CONFIG$n_burnin)
  
  # ==================== 采样 ====================
  cat("开始采样...\n")
  
  # 基础参数 (c0, phi, sigma_epsilon, beta_k)
  samples_basic <- coda.samples(
    model = model,
    variable.names = c("c0", "phi", "sigma_epsilon", "beta_k"),
    n.iter = MODEL_CONFIG$n_iter_basic,
    thin = MODEL_CONFIG$thin_basic
  )
  
  # 题目参数 s
  samples_items <- coda.samples(
    model = model,
    variable.names = c("s", "avg_s"),
    n.iter = MODEL_CONFIG$n_iter_items,
    thin = MODEL_CONFIG$thin_items
  )
  
  # 学生参数 (alpha, theta)
  samples_alpha <- coda.samples(
    model = model,
    variable.names = c("alpha", "theta"),
    n.iter = MODEL_CONFIG$n_iter_alpha,
    thin = MODEL_CONFIG$thin_alpha
  )
  
  # ==================== 保存结果 ====================
  samples <- list(
    samples_basic = samples_basic,
    samples_items = samples_items,
    samples_alpha = samples_alpha
  )
  
  save(samples, empirical_data, file = "results/data/empirical_results.RData")
  cat("结果已保存到: results/data/empirical_results.RData\n")
  
  return(samples)
}

# 执行
if (sys.nframe() == 0) {
  # 加载数据（如果尚未加载）
  if (!exists("empirical_data")) {
    empirical_data <- load_empirical_data()
  }
  
  samples <- run_fc_dina_empirical(empirical_data)
  assign("samples", samples, globalenv())
}