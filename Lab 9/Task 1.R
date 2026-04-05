library(ggcorrplot)
data("ToothGrowth")

# filter only numeric columns for correlation
tg_numeric <- ToothGrowth[, c("len", "dose")]

# compute the correlation matrix
cor_matrix <- cor(tg_numeric)
print("Correlation Matrix for ToothGrowth:")
print(cor_matrix)

# plotting correlation heatmap using ggcorrplot
print(ggcorrplot::ggcorrplot(cor_matrix))

cat("Observation: There is a strong positive correlation between tooth length and the vitamin dosage. As the dose increases, tooth length tends to increase.\n")