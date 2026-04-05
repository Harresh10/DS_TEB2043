library(caret)
data("mtcars")

# normalize data with log transformation
scaled_log <- log(mtcars)

# normalize data with standard scaling
scaled_std <- as.data.frame(scale(mtcars))

# normalize data using min-max scaling
minmax_model <- preProcess(as.data.frame(mtcars), method=c("range"))
scaled_minmax <- predict(minmax_model, as.data.frame(mtcars))

# compare and contrast the summaries
cat("1. Summary: Original mtcars Data\n")
print(summary(mtcars))

cat("\n2. Summary: Log Transformed Data\n")
print(summary(scaled_log))

cat("\n3. Summary: Standard Scaled Data\n")
print(summary(scaled_std))

cat("\n4. Summary: Min-Max Scaled Data\n")
print(summary(scaled_minmax))

cat("\nDiscuss the findings:\n")
cat("- Log Transformation: Reduces the skewness and compresses the scale. Noticeable in columns like 'disp' and 'hp' where the maximum values are pulled closer to the mean.\n")
cat("- Standard Scaling: Centers the data perfectly. All variables now have a mean of exactly 0, and you can see negative values indicating data points below the mean.\n")
cat("- Min-Max Scaling: Bounded scaling. The Summary clearly shows the 'Min.' is exactly 0 and the 'Max.' is exactly 1 for every single column, preserving the original distribution shape strictly within this range.\n")