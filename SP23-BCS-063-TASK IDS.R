# -------------------------------------------
# 1. Load and Prepare the Dataset
# -------------------------------------------
data("EuStockMarkets")               # Load built-in dataset
df <- as.data.frame(EuStockMarkets) # Convert to data frame

# Basic information
str(df)                  # Structure of the data
head(df)                 # First 6 rows
summary(df)              # Summary statistics
dim(df)                  # Dimensions (rows x columns)

# -------------------------------------------
# 2. Line Plot for All Indices
# -------------------------------------------
matplot(EuStockMarkets, type = "l", lty = 1, col = 1:4,
        main = "European Stock Indices (1991-1998)",
        ylab = "Index Value", xlab = "Time")
legend("topleft", legend = colnames(EuStockMarkets), col = 1:4, lty = 1)

# -------------------------------------------
# 3. Individual Time Series Plots
# -------------------------------------------
par(mfrow = c(2, 2))  # 2 rows x 2 columns layout
for(i in 1:4) {
  plot(df[,i], type = "l", col = i,
       main = paste("Time Series of", colnames(df)[i]),
       ylab = "Index Value", xlab = "Time")
}
par(mfrow = c(1, 1))  # Reset layout

# -------------------------------------------
# 4. Correlation Matrix & Heatmap
# -------------------------------------------
cor_matrix <- cor(df)
print(cor_matrix)

# Heatmap (optional visualization)
heatmap(cor_matrix, col = colorRampPalette(c("white", "blue"))(100),
        main = "Correlation Heatmap", symm = TRUE)

# -------------------------------------------
# 5. Descriptive Statistics
# -------------------------------------------

# Step 1: Install psych package if not already installed
if (!require(psych)) {
  install.packages("psych")
  library(psych)
} else {
  library(psych)
}

# Step 2: Get detailed descriptive statistics
describe(df)

# -------------------------------------------
# 6. Boxplots to Visualize Distribution & Outliers
# -------------------------------------------
boxplot(df, main = "Boxplot of European Stock Indices", col = rainbow(4))

# -------------------------------------------
# 7. Density Plots (Distribution Shapes)
# -------------------------------------------
plot(density(df$DAX), main = "Density Plot of Stock Indices", col = "blue", lwd = 2)
lines(density(df$SMI), col = "red", lwd = 2)
lines(density(df$CAC), col = "green", lwd = 2)
lines(density(df$FTSE), col = "purple", lwd = 2)
legend("topright", legend = colnames(df),
       col = c("blue", "red", "green", "purple"), lty = 1, lwd = 2)

# -------------------------------------------
# 8. Rolling Mean for Trend Smoothing (e.g., DAX)
# -------------------------------------------

# Step 1: Install zoo package if not already installed
if (!require(zoo)) {
  install.packages("zoo")
  library(zoo)
} else {
  library(zoo)
}

# Step 2: Calculate and plot rolling mean
rolling_dax <- rollmean(df$DAX, k = 50, fill = NA)
plot(df$DAX, type = "l", col = "gray", main = "DAX with 50-Day Rolling Mean",
     ylab = "Index Value", xlab = "Time")
lines(rolling_dax, col = "blue", lwd = 2)
legend("topleft", legend = c("DAX", "Rolling Mean"), col = c("gray", "blue"), lty = 1)

# -------------------------------------------
# 9. Pairwise Relationships (Scatterplot Matrix)
# -------------------------------------------
pairs(df, main = "Pairwise Scatter Plots of Stock Indices", col = "darkgreen")

# -------------------------------------------
# 10. Optional: Summary Report using skimr
# -------------------------------------------

# Step 1: Install skimr if not already installed
if (!require(skimr)) {
  install.packages("skimr")
  library(skimr)
} else {
  library(skimr)
}

# Step 2: Show skim summary
skim(df)

# -------------------------------------------
# END OF FULL EDA CODE
# -------------------------------------------
