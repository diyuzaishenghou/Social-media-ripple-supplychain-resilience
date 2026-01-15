# 加载必要的包
library(ggplot2)
library(patchwork)

# 第一步读取数据框，并且筛选出相关的数据
result_data <- read.csv("C:/Users/Administrator/Desktop/【小论文A】新媒体传播条件下，如何快速部署供应链和销售网络？/final_result/final_result_Theta_NA_NA(调整版).csv", header = TRUE)

num_row <- nrow(result_data) # 获取数据框的真实行数

# 计算出符合要求的行数
a <- 2
b <- 6
temp_cal <- 0  # 临时计算用的变量

data_for_x <- vector("numeric", length = 0)  # 定义一个空的向量，用于储存图像的横坐标
data_for_y1 <- vector("numeric", length = 0) # 定义一个空的向量，用于储存图像的纵坐标1
data_for_y2 <- vector("numeric", length = 0) # 定义一个空的向量，用于储存图像的纵坐标2
data_for_y3 <- vector("numeric", length = 0) # 定义一个空的向量，用于储存图像的纵坐标3
data_for_y4 <- vector("numeric", length = 0) # 定义一个空的向量，用于储存图像的纵坐标4

for (i in 1:num_row) {  # 这个循环用于找出符合条件的自变量
  temp_cal <- a + (i - 1) * b  # 这一项用于计算临时的数值
  
  if (temp_cal <= num_row) {  # 这里放一个判断条件,计算的数值小于总行数
    data_for_x <- append(data_for_x, result_data[temp_cal, 1], after = 0)     # 在末尾添加元素  
  } else {
    break  # 超过行数，就结束循环
  }
}

# 计算符合条件的数值（确保相关的内容）
c <- 3
d <- 6
temp_cal_1 <- 0  # 临时计算y1用的变量   
temp_cal_2 <- 0  # 临时计算y2用的变量   Leader_P
temp_cal_3 <- 0  # 临时计算y3用的变量   Follower_P
temp_cal_4 <- 0  # 临时计算y4用的变量   Leader_Q
temp_cal_5 <- 0  # 临时计算y5用的变量   Follower_Q

# 这个循环用于找出符合条件的应变量1&2
for (j in 1:num_row) {  
  temp_cal_2 <- 0  # 参数归零  Leader_P
  temp_cal_3 <- 0  # 临时计算y3用的变量   Follower_P
  temp_cal_4 <- 0  # Leader_Q
  temp_cal_5 <- 0  # Follower_Q
  
  temp_cal_1 <- c + (j - 1) * d  # 这一项用于找出对应的序号 
  if (temp_cal_1 <= num_row) {  # 这里做一个判断条件，计算的数值小于总行数
    # 计算Leader平均售价
    temp_cal_2 <- temp_cal_2 + result_data[temp_cal_1, 2] + result_data[temp_cal_1 + 1, 2] + 
      result_data[temp_cal_1 + 2, 2] + result_data[temp_cal_1 + 3, 2] + result_data[temp_cal_1 + 4, 2]  # 五个数据结合起来
    temp_cal_2 <- temp_cal_2 / 5  # 计算平均数
    data_for_y1 <- append(data_for_y1, temp_cal_2, after = 0)
    
    # 计算Follower平均售价
    temp_cal_3 <- temp_cal_3 + result_data[temp_cal_1, 3] + result_data[temp_cal_1 + 1, 3] + 
      result_data[temp_cal_1 + 2, 3] + result_data[temp_cal_1 + 3, 3] + result_data[temp_cal_1 + 4, 3]  # 五个数据结合起来 
    temp_cal_3 <- temp_cal_3 / 5  # 计算平均数
    data_for_y2 <- append(data_for_y2, temp_cal_3, after = 0)  # 在数据框最后添加temp_cal_2所代表的值
    
    # 计算Leader的总销量
    temp_cal_4 <- temp_cal_4 + result_data[temp_cal_1, 4] + result_data[temp_cal_1 + 1, 4] + 
      result_data[temp_cal_1 + 2, 4] + result_data[temp_cal_1 + 3, 4] + result_data[temp_cal_1 + 4, 4]  # 五个数据结合起来
    data_for_y3 <- append(data_for_y3, temp_cal_4, after = 0)
    
    # 计算Follower的总销量
    temp_cal_5 <- temp_cal_5 + result_data[temp_cal_1, 5] + result_data[temp_cal_1 + 1, 5] + 
      result_data[temp_cal_1 + 2, 5] + result_data[temp_cal_1 + 3, 5] + result_data[temp_cal_1 + 4, 5]  # 五个数据结合起来 
    data_for_y4 <- append(data_for_y4, temp_cal_5, after = 0)
  } else {
    break  # 超过行数，结束循环
  }
}

# 创建数据框
df <- data.frame(
  PotentialMarketDemand = data_for_x,
  Leader_AvgPrice = data_for_y1,
  Follower_AvgPrice = data_for_y2,
  Leader_TotalSales = data_for_y3,
  Follower_TotalSales = data_for_y4
)

# 创建图像a：平均销售价格
p_a <- ggplot() +
  geom_line(aes(x = df$PotentialMarketDemand, y = df$Leader_AvgPrice, color = "IDL"), size = 1) +
  geom_line(aes(x = df$PotentialMarketDemand, y = df$Follower_AvgPrice, color = "ICF"), size = 1) +
  geom_point(aes(x = df$PotentialMarketDemand, y = df$Leader_AvgPrice, color = "IDL"), size = 2) +
  geom_point(aes(x = df$PotentialMarketDemand, y = df$Follower_AvgPrice, color = "ICF"), size = 2) +
  labs(x = "Competitive erosion rate", 
       y = "Average selling price") +
  scale_color_manual(values = c("IDL" = "purple", "ICF" = "#FFD700"),
                     name = "") +  # 设置图例标题为空字符串
  theme_classic() +  # 使用经典主题，无网格背景
  theme(legend.position = "top",
        legend.title = element_blank(),  # 确保图例标题为空
        axis.title = element_text(size = 11),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5))

# 创建图像b：总销售量
p_b <- ggplot() +
  geom_line(aes(x = df$PotentialMarketDemand, y = df$Leader_TotalSales, color = "IDL"), size = 1) +
  geom_line(aes(x = df$PotentialMarketDemand, y = df$Follower_TotalSales, color = "ICF"), size = 1) +
  geom_point(aes(x = df$PotentialMarketDemand, y = df$Leader_TotalSales, color = "IDL"), size = 2) +
  geom_point(aes(x = df$PotentialMarketDemand, y = df$Follower_TotalSales, color = "ICF"), size = 2) +
  labs(x = "Competitive erosion rate", 
       y = "Total sales") +
  scale_color_manual(values = c("IDL" = "purple", "ICF" = "#FFD700"),
                     name = "") +  # 设置图例标题为空字符串
  theme_classic() +  # 使用经典主题，无网格背景
  theme(legend.position = "top",
        legend.title = element_blank(),  # 确保图例标题为空
        axis.title = element_text(size = 11),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5))

# 将两个图像从左到右拼接
combined_plot <- p_a + p_b + 
  plot_layout(ncol = 2, widths = c(1, 1), guides = "collect") &
  theme(legend.position = "top")

# 基于您的数据文件路径，构建图像保存路径
data_path <- "C:/Users/Administrator/Desktop/【小论文A】新媒体传播条件下，如何快速部署供应链和销售网络？/￥【投稿期刊】￥/【进行中】IJPE/可编辑图表源文件/"
output_file <- paste0(data_path, "Figure_15_combined.pdf")

# 输出为高质量的PDF格式
ggsave(filename = output_file,
       plot = combined_plot,
       device = cairo_pdf,  # 使用cairo_pdf设备以获得更好的字体和图形质量
       width = 16,          # 总宽度（厘米）
       height = 8,          # 高度（厘米）
       units = "cm",
       dpi = 600)           # 高分辨率

# 在屏幕上显示组合图
print(combined_plot)

# 输出成功信息
cat("组合图已成功保存为 '", output_file, "'\n", sep = "")