# Load libraries
library(readr)       # For read_csv
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(caret)       # For train/test split
library(factoextra)  # For PCA visualization
library(nnet)        # For multinomial logistic regression
library(ggrepel)     # For better label positioning

# Read data
df <- read_csv("protein.csv")

# Separate predictors and target
x <- df %>% select(-Country)
y <- df$Country

# Standardize the features
x_scaled <- scale(x)

# Perform PCA with 2 components
pca_res <- prcomp(x_scaled, center = TRUE, scale. = TRUE)
pca_df <- as.data.frame(pca_res$x[, 1:2])
colnames(pca_df) <- c("PC1", "PC2")
pca_df$Country <- y

# Train-test split (70% train, 30% test)
set.seed(42)
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- pca_df[train_index, 1:2]
X_test  <- pca_df[-train_index, 1:2]
y_train <- y[train_index]
y_test  <- y[-train_index]

# Logistic Regression (multinomial for multiple countries)
model <- multinom(Country ~ ., data = data.frame(Country = y_train, X_train))

# Predictions
y_pred <- predict(model, newdata = X_test)

# Confusion matrix
conf_mat <- confusionMatrix(as.factor(y_pred), as.factor(y_test))
print(conf_mat)

# Plot before PCA (using first 2 standardized original features)
orig_df <- as.data.frame(x_scaled[, 1:2])
colnames(orig_df) <- c("Feature1", "Feature2")
orig_df$Country <- y

p1 <- ggplot(orig_df, aes(x = Feature1, y = Feature2, color = Country, label = Country)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +
  theme_minimal() +
  ggtitle("Before PCA: Using First 2 Standardized Features")

# Plot after PCA
p2 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Country, label = Country)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +
  theme_minimal() +
  ggtitle("After PCA: Projected onto 2 Principal Components")

# Display plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
