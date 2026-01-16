# Load necessary packages
library(ggplot2)
library(ggtext)
library(pracma)  # Provides lambertW function

# Create x value range
x_main <- seq(-1/exp(1), 3, length.out = 500)  # Principal branch domain [-1/e, 3]
x_neg <- seq(-1/exp(1), -0.01, length.out = 200)  # Negative branch domain [-1/e, 0)

# Calculate Lambert W function values
w_main <- lambertWp(x_main)  # Principal branch (k=0)
w_neg <- lambertWn(x_neg)   # Negative branch (k=-1)

# Create data frame
df_main <- data.frame(x = x_main, y = w_main, branch = "Principal branch (k=0)")
df_neg <- data.frame(x = x_neg, y = w_neg, branch = "Negative branch (k=-1)")
df <- rbind(df_main, df_neg)

# Special points data
special_points <- data.frame(
  x = c(0, -1/exp(1)),
  y = c(0, -1),
  label = c("(0,0)", "(-1/e, -1)")
)

# Create plot
ggplot(df, aes(x = x, y = y, color = branch, linetype = branch)) +
  geom_line(size = 1.2) +
  # Add special points
  geom_point(data = special_points, aes(x = x, y = y), 
             color = "#F8766D", size = 3, inherit.aes = FALSE) +
  geom_text(data = special_points, aes(x = x, y = y, label = label),
            nudge_x = c(0.2, -0.15), nudge_y = c(0.2, -0.3),
            color = "black", size = 5, inherit.aes = FALSE) +
  # Add asymptotes
  geom_hline(yintercept = -1, linetype = "dotted", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray50") +
  geom_vline(xintercept = -1/exp(1), linetype = "dotted", color = "gray50") +
  # Add branch description
  annotate("text", x = 1.5, y = -0.3, 
           label = "Principal branch (k=0)\nmonotonic increase\nW(x) ≥ -1", 
           size = 4.5, color = "#FFD700", hjust = 0,
           fontface = "bold") +
  annotate("text", x = 0, y = -2.5, 
           label = "Negative branch (k=-1)\nmonotonically decreasing\nW(x) ≤ -1", 
           size = 4.5, color = "purple", hjust = 0,
           fontface = "bold") +
  # Add formula description
  annotate("text", x = 1.5, y = -3, 
           label = "W(x)*e^{W(x)} == x", 
           parse = TRUE, size = 6, fontface = "bold") +
  # Set coordinate axis range
  coord_cartesian(xlim = c(-0.5, 3), ylim = c(-4.5, 1.5)) +
  # Labels and title
  labs(
    title = "Lambert W function - principal branch and negative branch",
    x = "x",
    y = "W(x)",
    # color = "Branch",  # Uncomment if needed
    # linetype = "Branch"  # Uncomment if needed
  ) +
  # Color and line type settings
  scale_color_manual(values = c("#FFD700", "purple")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  # Theme settings
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
