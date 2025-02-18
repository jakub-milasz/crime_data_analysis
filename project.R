library(sf)
library(dbscan)
library(ggplot2)
library(dplyr)

# Set working directory
setwd("~/ArcGIS/Projects/project")

# Load data
osiedla <- read_sf(dsn="dane/osiedla/", layer = "osiedla")
przestepstwa <- read_sf(dsn="dane/projected_data/", layer = "projected_data")

# Copy crime table for HDBSCAN analysis
przestepstwa_hdbscan <- przestepstwa

# Extract crime coordinates
coords <- st_coordinates(przestepstwa)

# Function for DBSCAN analysis with selected parameters
analyze_dbscan <- function(eps, minPts) {
  # Perform DBSCAN analysis using dbscan() function
  dbscan_result <- dbscan(coords, eps = eps, minPts = minPts)
  # Add cluster results to the crime data frame
  przestepstwa$cluster <- dbscan_result$cluster
  # Remove noise
  przestepstwa <- filter(przestepstwa, przestepstwa$cluster != 0)
  # Display results
  print(table(przestepstwa$cluster))
  # Visualization
  library(ggplot2)
  ggplot() +
    geom_sf(data = osiedla, fill = NA, color = 'blue') + # Neighborhoods
    geom_sf(data = przestepstwa, aes(color = factor(cluster)), size = 2) + # Crimes
    scale_color_manual(values = rainbow(length(unique(przestepstwa$cluster)))) + # Cluster colors
    theme_minimal() +
    labs(title = paste0("Crime Clusters and Neighborhoods\n(DBSCAN minPts = ", minPts,", eps = ", eps, ")"),
         color = "Cluster") +
    theme(legend.position = "right")
}

# Several DBSCAN examples with different parameters
analyze_dbscan(200, 4)
analyze_dbscan(500, 4)
analyze_dbscan(200, 10)


# Function for HDBSCAN analysis with selected parameter
analyze_hdbscan <- function(minPts) {
  # Perform HDBSCAN analysis using hdbscan() function
  hdbscan_result <- hdbscan(coords, minPts = minPts)
  # Add cluster results to the crime data frame
  przestepstwa_hdbscan$cluster <- as.factor(hdbscan_result$cluster)
  # Remove noise
  przestepstwa_hdbscan <- filter(przestepstwa_hdbscan, przestepstwa_hdbscan$cluster != 0)
  # Display results
  print(table(przestepstwa_hdbscan$cluster))
  # Visualization
  ggplot() +
    geom_sf(data = osiedla, fill = NA, color = 'blue') + # Neighborhoods
    geom_sf(data = przestepstwa_hdbscan, aes(color = cluster), size = 2) + # Crimes
    scale_color_manual(values = rainbow(length(unique(przestepstwa_hdbscan$cluster)))) +
    theme_minimal() +
    labs(title = paste0("Crime Clusters and Neighborhoods (HDBSCAN minPts = ", minPts, ")"),
         color = "Cluster") +
    theme(legend.position = "right")
}

# Several HDBSCAN examples with different parameters
analyze_hdbscan(30)
analyze_hdbscan(10)
analyze_hdbscan(60)
