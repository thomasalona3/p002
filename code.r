# Step 1: Load necessary libraries
library(lme4)  # for mixed models
library(dplyr)  # for data manipulation
library(ggplot2)  # for data visualization
library(cluster)  # for clustering analysis

# Step 2: Read the data
data <- read.csv("C:/Users/Aaron George/OneDrive/Desktop/data/crime_data.csv")

# Step 3: Perform Linear Mixed Model Analysis
# Simplified model without Year and Region interactions
simplified_model <- lmer(CrimeRate ~ Year + (1 | Region/Country), data = data)
print(summary(simplified_model))

# Step 4: Clustering Analysis
# Perform k-means clustering - assuming you've already determined the number of clusters (k) 
# Let's assume k = 4 for demonstration
set.seed(123)  # Ensure reproducibility
kmeans_result <- kmeans(data$CrimeRate, centers = 4)
data$Cluster <- as.factor(kmeans_result$cluster)

# Calculate and display cluster statistics
cluster_stats <- data %>%
  group_by(Cluster) %>%
  summarise(
    AvgCrimeRate = mean(CrimeRate),
    MinCrimeRate = min(CrimeRate),
    MaxCrimeRate = max(CrimeRate),
    SD = sd(CrimeRate),
    Count = n()
  )
print(cluster_stats)

# Identify and display countries within each cluster
countries_in_clusters <- data %>%
  group_by(Cluster, Country) %>%
  summarise(AvgCrimeRate = mean(CrimeRate), .groups = 'drop') %>%
  arrange(Cluster, AvgCrimeRate)
print(countries_in_clusters, n = Inf)

# Step 5: Visualization of Crime Rate Clusters
p <- ggplot(data, aes(x = Year, y = CrimeRate, color = Cluster, group = Country)) +
  geom_line() +  # Connect points year by year for each country
  geom_point() + # Add points on top of lines
  facet_wrap(~ Country, scales = "free_y") + # Facet plot by country
  scale_x_continuous(breaks = unique(data$Year), labels = unique(data$Year)) + # Ensure all years are displayed
  theme_minimal() + # Use a minimal theme for better visuals
  theme(axis.text.x = element_text(angle = 90, hjust = 1), # Rotate x-axis labels for better readability
        legend.position = "bottom") + # Move legend to the bottom
  labs(title = "Crime Rate Clusters by Country and Year",
       x = "Year",
       y = "Crime Rate",
       color = "Cluster")
print(p)