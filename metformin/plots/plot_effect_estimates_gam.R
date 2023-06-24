library(ggplot2)
library(gridExtra)
library(cowplot)
library(data.table)

load("data/SNV_MET_ALL_BEFORE_POSITIVE.RData")
load("out/smooth_blip_results.RData")

# WAVE 1
plt_1 <- ggplot() + 
  geom_smooth(data = DT_1_PLT[PDC <= 0.9], aes(x = PDC, y = DIFF_PRED_10),
              color = "dodgerblue", size = 1.5, se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.5) +
  scale_x_continuous(limits = c(0.4, 1.0), breaks = seq(0.4, 1.0, 0.1)) +
  coord_cartesian(ylim = c(-0.05, 0.05)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) + 
  labs(title = "Wave 1",
       x = "",
       y = "Estimated change in COVID-19 mortality") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"))

# WAVE 2
plt_2 <- ggplot() + 
  geom_smooth(data = DT_2_PLT[PDC <= 0.9], aes(x = PDC, y = DIFF_PRED_10),
              color = "dodgerblue", size = 1.5, se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.5) +
  scale_x_continuous(limits = c(0.4, 1.0), breaks = seq(0.4, 1.0, 0.1)) +
  coord_cartesian(ylim = c(-0.05, 0.05)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) + 
  labs(title = "Wave 2",
       x = "",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"))

# WAVE 3
plt_3 <- ggplot() + 
  geom_smooth(data = DT_3_PLT[PDC <= 0.9], aes(x = PDC, y = DIFF_PRED_10),
              color = "dodgerblue", size = 1.5, se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.5) +
  scale_x_continuous(limits = c(0.4, 1.0), breaks = seq(0.4, 1.0, 0.1)) +
  coord_cartesian(ylim = c(-0.05, 0.05)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) + 
  labs(title = "Wave 3",
       x = "",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"))

## VISUALIZATION 2 -------------------------------------------------------------
# WAVE 1
hist_1 <- ggplot(data = DT[WAVE == 1]) +
  geom_histogram(aes(x = PDC), 
                 fill = "darkorange", 
                 alpha = 0.5,
                 boundary = 0,
                 closed = "right", 
                 binwidth = 0.02) +
  geom_histogram(aes(x = ifelse(PDC + 0.1 > 1, PDC, PDC + 0.1)),
                 fill = "dodgerblue", 
                 alpha = 0.5,
                 boundary = 0,
                 closed = "right", 
                 binwidth = 0.02) +
  scale_x_continuous(breaks = seq(0.4, 1.0, 0.1), lim = c(0.4, 1.0)) + 
  scale_y_continuous(breaks = seq(0, 4000, 1000), lim = c(0, 4300)) +
  labs(x = "PDC",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"))

# WAVE 2
hist_2 <- ggplot(data = DT[WAVE == 2]) +
  geom_histogram(aes(x = PDC), 
                 fill = "darkorange", 
                 alpha = 0.5,
                 boundary = 0,
                 closed = "right", 
                 binwidth = 0.02) +
  geom_histogram(aes(x = ifelse(PDC + 0.1 > 1, PDC, PDC + 0.1)),
                 fill = "dodgerblue", 
                 alpha = 0.5,
                 boundary = 0,
                 closed = "right", 
                 binwidth = 0.02) +
  scale_x_continuous(breaks = seq(0.4, 1.0, 0.1), lim = c(0.4, 1.0)) + 
  scale_y_continuous(breaks = seq(0, 4000, 1000), lim = c(0, 4300)) +
  labs(x = "PDC",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"))

# WAVE 3
hist_3 <- ggplot(data = DT[WAVE == 3]) +
  geom_histogram(aes(x = PDC), 
                 fill = "darkorange", 
                 alpha = 0.5,
                 boundary = 0,
                 closed = "right", 
                 binwidth = 0.02) +
  geom_histogram(aes(x = ifelse(PDC + 0.1 > 1, PDC, PDC + 0.1)),
                 fill = "dodgerblue", 
                 alpha = 0.5,
                 boundary = 0,
                 closed = "right", 
                 binwidth = 0.02) +
  scale_x_continuous(breaks = seq(0.4, 1.0, 0.1), lim = c(0.4, 1.0)) + 
  scale_y_continuous(breaks = seq(0, 4000, 1000), lim = c(0, 4300)) +
  labs(x = "PDC",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"))

p <- plot_grid(plt_1, plt_2, plt_3,
               hist_1, hist_2, hist_3,
               ncol = 3, align = "v", axis = "tb")

ggsave(filename = "plot_effect_estimates_gam.pdf", plot = p, device = "pdf", 
       path = "plots/", width = 12, height = 8, dpi = 300)
