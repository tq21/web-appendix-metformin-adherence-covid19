library(ggplot2)
library(data.table)

load("data/SNV_MET_ALL_BEFORE_POSITIVE.RData")

# discretize PDC into groups by 0.1 increment
DT[, PDC_group := cut(PDC, breaks = seq(0, 1, by = 0.1))]
DT[, DEAD := as.numeric(DEAD) - 1]

# Calculate mean mortality rate for each PDC group
DT_mortality_rate <- DT[, .(mortality_rate = mean(DEAD)), by = PDC_group]
DT_original_counts <- DT[, .(N = .N), by = PDC_group]
DT_mortality_rate <- merge(DT_mortality_rate, DT_original_counts, by = "PDC_group")
DT_mortality_rate[, se := sqrt(mortality_rate * (1 - mortality_rate) / N)]

# Create the scatter plot
p <- ggplot(DT_mortality_rate, aes(x = PDC_group, y = mortality_rate)) +
  geom_point(color = "#619CFF", size = 4, shape = 19) +
  geom_errorbar(aes(ymin = mortality_rate - 1.96*se, ymax = mortality_rate + 1.96*se), 
                width = 0.3, color = "#619CFF", linewidth = 1.5) +
  theme_minimal() +
  labs(x = "", y = "Mortality Rate", 
       title = "Mortality Rate by Proportion of Days Covered Group") +
  scale_y_continuous(name = "Mortality Rate", 
                     labels = scales::percent_format(accuracy = .1)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12))

ggsave(filename = "plot_mortality_vs_pdc.pdf", plot = p, device = "pdf", 
       path = "plots/", width = 10, height = 6, dpi = 300)
