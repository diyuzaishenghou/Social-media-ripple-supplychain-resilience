# 清理环境
rm(list = ls())
gc()

# 加载必要的包
library(ggplot2)
library(dplyr)
library(tidyr)

# 设置随机种子
set.seed(123)

# 1. 定义简单的需求函数（不使用类）
demand_function <- function(x, params) {
  # params = list(a, b_list, T_list, mu)
  a <- params$a
  b_list <- params$b_list
  T_list <- params$T_list
  mu <- params$mu
  
  # 确保参数有效
  a <- max(0.01, min(0.99, a))
  T_list <- sort(T_list)  # 确保递增
  
  if (x > max(T_list)) {
    return(mu)
  }
  
  for (i in 1:length(T_list)) {
    if (x <= T_list[i]) {
      return(-a^x + b_list[i])
    }
  }
  
  return(mu)
}

# 2. 运行蒙特卡洛模拟函数
run_monte_carlo <- function(
    baseline_params = list(a = 0.9, b_list = c(100, 85, 70), 
                           T_list = c(5, 10, 15), mu = 50),
    n_simulations = 200,
    n_time_points = 50,
    time_horizon = 20,
    perturbation_level = 0.1
) {
  
  # 生成时间点
  time_points <- seq(0.1, time_horizon, length.out = n_time_points)
  
  # 存储结果
  all_demand_paths <- matrix(NA, nrow = n_simulations, ncol = n_time_points)
  all_params_list <- vector("list", n_simulations)
  
  cat("开始蒙特卡洛模拟...\n")
  
  for (sim in 1:n_simulations) {
    # 生成扰动参数
    perturbed_params <- list(
      a = baseline_params$a * exp(rnorm(1, 0, perturbation_level * 0.1)),
      b_list = baseline_params$b_list + rnorm(length(baseline_params$b_list), 0, perturbation_level * 10),
      T_list = baseline_params$T_list * (1 + rnorm(length(baseline_params$T_list), 0, perturbation_level)),
      mu = baseline_params$mu + rnorm(1, 0, perturbation_level * 5)
    )
    
    # 确保参数有效性
    perturbed_params$a <- max(0.01, min(0.99, perturbed_params$a))
    perturbed_params$b_list <- pmax(1, perturbed_params$b_list)
    perturbed_params$T_list <- pmax(0.1, perturbed_params$T_list)
    perturbed_params$mu <- max(0, perturbed_params$mu)
    perturbed_params$T_list <- sort(perturbed_params$T_list)
    
    all_params_list[[sim]] <- perturbed_params
    
    # 计算需求路径
    for (t in 1:n_time_points) {
      all_demand_paths[sim, t] <- demand_function(time_points[t], perturbed_params)
    }
    
    if (sim %% 10 == 0) cat(sprintf("进度: %d/%d\n", sim, n_simulations))
  }
  
  # 计算基准路径
  baseline_path <- sapply(time_points, demand_function, params = baseline_params)
  
  cat("模拟完成!\n")
  
  return(list(
    time_points = time_points,
    baseline_path = baseline_path,
    all_demand_paths = all_demand_paths,
    all_params = all_params_list,
    n_simulations = n_simulations,
    baseline_params = baseline_params
  ))
}

# 3. 分析函数
analyze_results <- function(results) {
  demand_paths <- results$all_demand_paths
  
  # 计算基本统计
  mean_path <- colMeans(demand_paths, na.rm = TRUE)
  std_path <- apply(demand_paths, 2, sd, na.rm = TRUE)
  ci_90_lower <- apply(demand_paths, 2, quantile, 0.05, na.rm = TRUE)
  ci_90_upper <- apply(demand_paths, 2, quantile, 0.95, na.rm = TRUE)
  cv <- std_path / abs(mean_path)
  
  # 阶段统计
  stage_T <- results$baseline_params$T_list
  stage_stats <- list()
  
  for (i in 1:length(stage_T)) {
    idx <- which.min(abs(results$time_points - stage_T[i]))
    stage_demands <- demand_paths[, idx]
    stage_stats[[i]] <- list(
      stage = i,
      time = stage_T[i],
      mean = mean(stage_demands, na.rm = TRUE),
      sd = sd(stage_demands, na.rm = TRUE),
      cv = sd(stage_demands, na.rm = TRUE) / abs(mean(stage_demands, na.rm = TRUE))
    )
  }
  
  return(list(
    mean_path = mean_path,
    std_path = std_path,
    ci_90_lower = ci_90_lower,
    ci_90_upper = ci_90_upper,
    cv = cv,
    stage_stats = stage_stats
  ))
}

# 4. 生成报告函数
generate_report <- function(results, analysis) {
  report_data <- data.frame()
  
  # 基本信息
  report_data <- rbind(report_data, data.frame(
    指标 = "模拟次数",
    值 = as.character(results$n_simulations),
    单位 = "次",
    说明 = "蒙特卡洛模拟总次数"
  ))
  
  # 阶段统计
  for (i in 1:length(analysis$stage_stats)) {
    stats <- analysis$stage_stats[[i]]
    report_data <- rbind(report_data, data.frame(
      指标 = paste0("阶段", i, "转换点需求均值"),
      值 = sprintf("%.2f", stats$mean),
      单位 = "需求单位",
      说明 = paste0("时间点T=", stats$time, ", CV=", sprintf("%.3f", stats$cv))
    ))
  }
  
  # 总体统计
  report_data <- rbind(report_data, data.frame(
    指标 = "平均需求",
    值 = sprintf("%.2f", mean(results$all_demand_paths, na.rm = TRUE)),
    单位 = "需求单位",
    说明 = "所有模拟和时间点的平均需求"
  ))
  
  report_data <- rbind(report_data, data.frame(
    指标 = "需求标准差",
    值 = sprintf("%.2f", sd(results$all_demand_paths, na.rm = TRUE)),
    单位 = "需求单位",
    说明 = "需求的总标准差"
  ))
  
  return(report_data)
}

# 5. 可视化函数
visualize_results <- function(results, analysis, save_path = NULL) {
  plot_data <- data.frame(
    time = results$time_points,
    mean = analysis$mean_path,
    baseline = results$baseline_path,
    ci_lower = analysis$ci_90_lower,
    ci_upper = analysis$ci_90_upper
  )
  
  p <- ggplot(plot_data, aes(x = time)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                fill = "gray70", alpha = 0.5) +
    geom_line(aes(y = baseline), color = "red", linewidth = 1) +
    geom_line(aes(y = mean), color = "blue", linetype = "dashed", linewidth = 1) +
    labs(x = "时间", y = "需求", 
         title = "蒙特卡洛模拟结果",
         subtitle = paste0("模拟次数: ", results$n_simulations)) +
    theme_minimal()
  
  # 添加阶段转换线
  for (T in results$baseline_params$T_list) {
    p <- p + geom_vline(xintercept = T, linetype = "dotted", color = "darkgreen", alpha = 0.5)
  }
  
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 10, height = 6, dpi = 300)
    cat("图表已保存到:", save_path, "\n")
  }
  
  return(p)
}

# ================ 主程序 ================

cat("开始蒙特卡洛测试...\n")

# 运行模拟
results <- run_monte_carlo(
  n_simulations = 200,  # 模拟次数
  n_time_points = 50,   # 时间点数量
  time_horizon = 20,    # 时间范围
  perturbation_level = 0.1  # 扰动水平
)

# 分析结果
analysis <- analyze_results(results)

# 生成报告
report_df <- generate_report(results, analysis)
cat("\n===== 报告结果 =====\n")
print(report_df)

# 可视化
p <- visualize_results(results, analysis, save_path = "final_results.png")
print(p)

# 保存报告
write.csv(report_df, "final_report.csv", row.names = FALSE)
cat("\n报告已保存到: final_report.csv\n")
cat("图表已保存到: final_results.png\n")

# 显示阶段详细统计
cat("\n===== 阶段详细统计 =====\n")
for (i in 1:length(analysis$stage_stats)) {
  stats <- analysis$stage_stats[[i]]
  cat(sprintf("阶段%d (T=%.1f):\n", i, stats$time))
  cat(sprintf("  均值: %.2f\n", stats$mean))
  cat(sprintf("  标准差: %.2f\n", stats$sd))
  cat(sprintf("  变异系数: %.3f\n", stats$cv))
  cat("\n")

}
