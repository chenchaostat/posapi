#' @title Probability of Success
#' @description
#' Calculating probability of success in oncology clinical trials.
#'
#' @param hr_true True hazard ratio which you assumed.
#' @param hr_threshold Hazard ratio threshold. It will be used to calculate Pr(hr < hr_threshold).
#' @param n_events PFS/OS events planned.
#'
#' @returns Prints Pr(hr < hr_threshold) to console.
#' @export
#'
#' @examples
#' \dontrun{
#' posfunc(hr_true = 0.75, hr_threshold = 0.85, n_events = 220)
#' }
PoS <- function(
    hr_true = 0.75,          
    hr_threshold = 0.85,     
    n_events = 220           
){
  # 1. 计算对数尺度下的参数
  log_hr_true <- log(hr_true)
  log_hr_threshold <- log(hr_threshold)
  # 2. 计算标准误 (Standard Error)
  # 假设 1:1 分组，方差近似为 4/d
  se_log_hr <- sqrt(4 / n_events)
  # 3. 计算 Z 分数 (标准化得分)
  # Z = (观测阈值 - 真实均值) / 标准误
  z_score <- (log_hr_threshold - log_hr_true) / se_log_hr
  # 4. 计算累积概率 P(Z < z_score)
  probability <- pnorm(z_score)
  # 输出结果
  cat("--- 最终分析 HR <", hr_threshold, "的概率计算 ---\n")
  cat("真实 HR:", hr_true, "\n")
  cat("观察事件数:", n_events, "\n")
  cat("对数 HR 的标准误 (SE):", round(se_log_hr, 4), "\n")
  cat("Z 统计量:", round(z_score, 4), "\n")
  cat("最终分析时观察到 HR < ",hr_threshold, "的概率为:", round(probability * 100, 2), "%\n")
}
PoS(hr_true = 0.85, hr_threshold = 0.80, n_events = 126)
