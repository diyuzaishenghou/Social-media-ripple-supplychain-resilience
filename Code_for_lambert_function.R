# 加载必要的包
library(ggplot2)
library(ggtext)
library(pracma)  # 提供 lambertW 函数

# 创建 x 值范围
x_main <- seq(-1/exp(1), 3, length.out = 500)  # 主分支定义域 [-1/e, 3]
x_neg <- seq(-1/exp(1), -0.01, length.out = 200)  # 负分支定义域 [-1/e, 0)

# 计算朗博 W 函数值
w_main <- lambertWp(x_main)  # 主分支 (k=0)
w_neg <- lambertWn(x_neg)   # 负分支 (k=-1)

# 创建数据框
df_main <- data.frame(x = x_main, y = w_main, branch = "Principal branch (k=0)")
df_neg <- data.frame(x = x_neg, y = w_neg, branch = "Negative branch (k=-1)")
df <- rbind(df_main, df_neg)

# 特殊点数据
special_points <- data.frame(
  x = c(0, -1/exp(1)),
  y = c(0, -1),
  label = c("(0,0)", "(-1/e, -1)")
)

# 创建图像
ggplot(df, aes(x = x, y = y, color = branch, linetype = branch)) +
  geom_line(size = 1.2) +
  # 添加特殊点
  geom_point(data = special_points, aes(x = x, y = y), 
             color = "#F8766D", size = 3, inherit.aes = FALSE) +
  geom_text(data = special_points, aes(x = x, y = y, label = label),
            nudge_x = c(0.2, -0.15), nudge_y = c(0.2, -0.3),
            color = "black", size = 5, inherit.aes = FALSE) +
  # 添加渐近线
  geom_hline(yintercept = -1, linetype = "dotted", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray50") +
  geom_vline(xintercept = -1/exp(1), linetype = "dotted", color = "gray50") +
  # 添加分支说明
  annotate("text", x = 1.5, y = -0.3, 
           label = "Principal branch (k=0)\nmonotonic increase\nW(x) ≥ -1", 
           size = 4.5, color = "#FFD700", hjust = 0,
           fontface = "bold") +
  annotate("text", x = 0, y = -2.5, 
           label = "Negative branch (k=-1)\nmonotonically decreasing\nW(x) ≤ -1", 
           size = 4.5, color = "purple", hjust = 0,
           fontface = "bold") +
  # 添加公式说明
  annotate("text", x = 1.5, y = -3, 
           label = "W(x)*e^{W(x)} == x", 
           parse = TRUE, size = 6, fontface = "bold") +
  # 设置坐标轴范围
  coord_cartesian(xlim = c(-0.5, 3), ylim = c(-4.5, 1.5)) +
  # 标签和标题
  labs(
    title = "Lambert W function - principal branch and negative branch",
    x = "x",
    y = "W(x)",
   # color = "分支",
   # linetype = "分支"
  ) +
  # 颜色和线型设置
  scale_color_manual(values = c("#FFD700", "purple")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  # 主题设置
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = "white", color = "gray"),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray30", fill = NA, size = 0.5),
    plot.background = element_rect(fill = "white")
  )
