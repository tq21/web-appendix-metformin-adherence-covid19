library(ggplot2)
library(data.table)

load("data/SNV_MET_ALL_BEFORE_POSITIVE.RData")

# discretize age
DT[, AGE_GROUP := cut(AGE, 
                      breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 100),
                      labels = c("10-19y", "20-29y", "30-39y", "40-49y", "50-59y", "60-69y", "70-79y", ">=80y"))]

p <- ggplot(DT, aes(x = AGE_GROUP, y = PDC)) +
  geom_boxplot(aes(fill = AGE_GROUP), outlier.shape = NA, width = 0.5, linewidth = 1) +
  labs(x = "Age Group", y = "Proportion of Days Covered") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12)) +
  ggtitle("Adherence to Metformin Across Age Groups")

print(p)

ggsave(filename = "plot_pdc_vs_age.pdf", plot = p, device = "pdf", 
       path = "plots/", width = 10, height = 6, dpi = 300)
