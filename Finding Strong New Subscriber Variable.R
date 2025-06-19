# Load dataset
yt <- read.csv("youtube_channel_real_performance_analytics.csv")

# Check structure
str(yt)

# 1. Select only numeric columns
numeric_cols <- yt[sapply(yt, is.numeric)]

# 2. Remove constant (zero variance) columns
numeric_cols <- numeric_cols[, sapply(numeric_cols, function(x) sd(x, na.rm = TRUE) != 0)]

# 3. Correlation with New.Subscribers
cor_with_subs <- cor(numeric_cols, use = "complete.obs")[, "New.Subscribers"]

# 4. Exclude itself
cor_with_subs <- cor_with_subs[names(cor_with_subs) != "New.Subscribers"]

# 5. Get top 3
top3 <- sort(abs(cor_with_subs), decreasing = TRUE)[1:6]
top3_names <- names(top3)
top3_values <- cor_with_subs[top3_names]

# 6. Show result
data.frame(Variable = top3_names, Correlation = round(top3_values, 6))

# 7. Visualize with base R
par(mfrow = c(2, 3))
for (var in top3_names) {
  plot(yt[[var]], yt$New.Subscribers,
       main = paste(var, "\nCorr:", round(top3_values[var], 2)),
       xlab = var,
       ylab = "New.Subscribers",
       pch = 19, col = "steelblue")
}
