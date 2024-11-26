# Ładowanie bibliotek
library(sf)
library(dbscan)
library(dplyr)

# Ustawienie katalogu roboczego
setwd("~/ArcGIS/Projects/adp_lab")

# Wczytanie danych
przestepstwa <- read_sf(dsn = "./zestaw1_data", layer = "zestaw1_transformed")
osiedla <- read_sf(dsn = './osiedla', layer = "osiedla")

# Skopiowanie tabeli przestępstw na potrzeby hdbscan
przestepstwa_hdbscan <- przestepstwa

# Ekstrakcja współrzędnych przestępstw
coords <- st_coordinates(przestepstwa)

# Ustalenie parametrów DBSCAN
eps <- 200
minPts <- 4  # minimalna liczba punktów w sąsiedztwie

# Przeprowadzenie analizy DBSCAN za pomocą funkcji dbscan()
dbscan_result <- dbscan(coords, eps = eps, minPts = minPts)

# Dodanie wyników klastra do ramki danych przestępstw
przestepstwa$cluster <- dbscan_result$cluster
# Usunięcie szumu
przestepstwa <- filter(przestepstwa, przestepstwa$cluster != 0)

# Wyświetlenie wyników
print(table(przestepstwa$cluster))

# Wizualizacja
library(ggplot2)
ggplot() +
  geom_sf(data = osiedla, fill = NA, color = 'blue') + # Osiedla
  geom_sf(data = przestepstwa, aes(color = factor(cluster)), size = 2) + # Przestępstwa
  scale_color_manual(values = rainbow(length(unique(przestepstwa$cluster)))) + # Kolory klastrów
  theme_minimal() +
  labs(title = "Klastry Przestępstw i Osiedla\n(DBSCAN minPts = 4, eps = 200)",
       color = "Klaster") +
  theme(legend.position = "right")

# Przeprowadzenie analizy HDBSCAN za pomocą funkcji hdbscan()
hdbscan_result <- hdbscan(coords, minPts = 30)

# Dodanie wyników klastra do ramki danych przestępstw
przestepstwa_hdbscan$cluster <- as.factor(hdbscan_result$cluster)

# Usunięcie szumu
przestepstwa_hdbscan <- filter(przestepstwa_hdbscan, przestepstwa_hdbscan$cluster != 0)

# Wyświetlenie wyników
print(table(przestepstwa_hdbscan$cluster))

# Wizualizacja
ggplot() +
  geom_sf(data = osiedla, fill = NA, color = 'blue') + # Osiedla
  geom_sf(data = przestepstwa_hdbscan, aes(color = cluster), size = 2) + # Przestępstwa
  scale_color_manual(values = rainbow(length(unique(przestepstwa_hdbscan$cluster)))) +
  theme_minimal() +
  labs(title = "Klastry Przestępstw i Osiedla (HDBSCAN minPts = 30)",
       color = "Klaster") +
  theme(legend.position = "right")