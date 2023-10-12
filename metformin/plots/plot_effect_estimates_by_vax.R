library(ggplot2)

# load TMLE results
load("out/TMLE_results_by_vax.RData")

# plot
p <- ggplot(res, aes(x = vax_status, y = est, color = shift_spec, group = shift_spec)) +
  geom_point(position = position_dodge(width = 0.5), size = 3.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.5), width = 0.3, size = 1.5) +
  scale_y_continuous(limits = c(-0.015, 0.05), breaks = seq(-0.04, 0.05, 0.01),
                     labels = scales::percent_format(accuracy = .1)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.5) +
  labs(title = "Estimated Reduction in COVID-19 Mortality Rate by Vaccination Status",
       x = "", y = "", color = "Shift Magnitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14))

ggsave(filename = "plot_effect_estimates_by_vax.pdf", plot = p, device = "pdf",
       path = "plots/", width = 12, height = 6, dpi = 300)
