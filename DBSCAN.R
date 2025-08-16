install.packages("dbscan")
install.packages("ggplot2")
install.packages("mlbench")
install.packages("scales")

library(dbscan)
library(ggplot2)
library(mlbench)
library(scales)

# Generate synthetic "two moons" data
set.seed(42)
moon_data = mlbench.2dnormals(n = 300, cl = 2)  # mlbench doesn't have make_moons directly
# We'll simulate moons manually
theta = runif(150, 0, pi)
x1 = cbind(cos(theta), sin(theta))
theta2 = runif(150, 0, pi)
x2 = cbind(1 - cos(theta2), 0.5 - sin(theta2))
X = rbind(x1, x2)

X_scaled = scale(X)

db= dbscan(X_scaled, eps = 0.3, minPts = 5)

X_df = as.data.frame(X_scaled)
X_df$cluster = factor(db$cluster)  


ggplot(X_df, aes(x = X_scaled[,1], y = X_scaled[,2], color = cluster)) +
  geom_point(size = 3) +
  scale_color_manual(values = rainbow(length(unique(db$cluster)))) +
  labs(title = "DBSCAN Clustering", x = "Feature 1", y = "Feature 2") +
  theme_minimal()

