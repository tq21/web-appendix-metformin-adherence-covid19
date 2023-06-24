library(ggplot2)

# load TMLE results
load("out/TMLE_results.RData")

# make data frame for plotting
wave_labels <- rep(c("Overall", "Wave 1", "Wave 2", "Wave 3"), 2)
shift_spec_labels <- c(rep("0.05 additive", 4), rep("0.10 additive", 4))
df_plt <- rbind(df_res_5, df_res_10)
df_plt$wave <- wave_labels
df_plt$shift_spec <- shift_spec_labels

# plot
p <- ggplot(df_plt, aes(x = wave, y = est, color = shift_spec, group = shift_spec)) +
  geom_point(position = position_dodge(width = 0.5), size = 3.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.5), width = 0.3, size = 1.5) +
  scale_y_continuous(limits = c(-0.01, 0.04), breaks = seq(-0.01, 0.04, 0.01),
                     labels = scales::percent_format(accuracy = .1)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.5) +
  labs(title = "Estimated Reduction in COVID-19 Mortality Rate", x = "", y = "", color = "Shift Magnitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14))

ggsave(filename = "plot_effect_estimates.pdf", plot = p, device = "pdf", 
       path = "plots/", width = 10, height = 6, dpi = 300)
