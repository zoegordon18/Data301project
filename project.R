#load libraries
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(zoo)
library(lubridate)
library(tsfeatures)
library(tidyr)
library(dtwclust)
library(factoextra)
library(purrr)
library(TSclust)
#part 1

#read files
file1 <- read.csv("daily-temperature-for-30-sites-to-2022-part1.csv")
file2 <- read.csv("daily-temperature-for-30-sites-to-2022-part2.csv")
file3 <- read.csv("daily-temperature-for-30-sites-to-2022-part3.csv")

# Combine 
temperature_data <- bind_rows(file1, file2, file3)

#Display 
str(temperature_data)

#get unique site locations
site_locations <- temperature_data %>%
  select(site, lat, lon) %>%
  distinct()

#convert to an sf object for mapping
site_locations_sf <- st_as_sf(site_locations, coords = c("lon", "lat"), crs = 4326)

#map of New Zealand
new_zealand_map <- ne_countries(scale = "medium", country = "New Zealand", returnclass = "sf")

#plot site locations
ggplot(data = new_zealand_map) +
  geom_sf(fill = "lightblue", color = "black") +
  geom_sf(data = site_locations_sf, color = "red", size = 2) +
  coord_sf(xlim = c(165, 180), ylim = c(-48, -34), expand = FALSE) + 
  labs(title = "Sites in New Zealand Where Temperature Measurements Were Collected", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()

#part 2

#organize the data
temperature_data_clean <- temperature_data %>%
  rename(City = site, 
         TempType = statistic) %>% 
  mutate(Date = as.Date(date, format = "%Y-%m-%d"))

#split the data by city and temperature type (Min, Max, Average)
temperature_split <- temperature_data_clean %>%
  group_by(City, TempType) %>%
  arrange(Date)

#display
head(temperature_split)

#part3


#time series for each city and statistic
ggplot(temperature_data_clean, aes(x = Date, y = temperature, color = TempType)) +
  geom_line() +
  facet_wrap(~ City + TempType, scales = "free_y") +
  labs(title = "Time Series of Daily Temperatures", 
       x = "Date", 
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "none") 





#first four 
first_four_series <- temperature_data_clean %>%
  distinct(City, TempType) %>%
  slice(1:4) %>%
  inner_join(temperature_data_clean, by = c("City", "TempType"))

# Plot first four time series
ggplot(first_four_series, aes(x = Date, y = temperature, color = TempType)) +
  geom_line() +
  facet_wrap(~ City + TempType, scales = "free_y") +
  labs(title = "Time Series of Daily Temperatures (First 4 Series)", 
       x = "Date", 
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "none") 



#part4

#seasonal behaviour 
#prep the data
seasonal_data <- temperature_data %>%
  rename(City = site, 
         TempType = statistic) %>% 
  mutate(Date = as.Date(date, format = "%Y-%m-%d"),
         Month = month(Date, label = TRUE, abbr = TRUE), 
         Year = year(Date)) 

#aggregate the data by month and temperature type 
seasonal_summary <- seasonal_data %>%
  group_by(City, TempType, Month) %>%
  summarise(AvgTemperature = mean(temperature, na.rm = TRUE)) %>%
  ungroup()

#plot seasonal behavior for all locations
ggplot(seasonal_summary, aes(x = Month, y = AvgTemperature, color = TempType, group = TempType)) +
  geom_line() +
  facet_wrap(~ City, scales = "free_y") + 
  labs(title = "Seasonal Temperature Trends by City",
       x = "Month",
       y = "Average Temperature (°C)",
       color = "Temperature Type") +
  theme_minimal()




# first four 
first_four_locations <- seasonal_summary %>%
  distinct(City) %>%
  slice(1:4) %>%
  inner_join(seasonal_summary, by = "City")


# Plot first 4
ggplot(first_four_locations, aes(x = Month, y = AvgTemperature, color = TempType, group = TempType)) +
  geom_line() +
  facet_wrap(~ City, scales = "free_y") + 
  labs(title = "Seasonal Temperature Trends for the First 4 Locations",
       x = "Month",
       y = "Average Temperature (°C)",
       color = "Temperature Type") +
  theme_minimal()




#part 5



#clustering the sites by time series data


seasonal_wide <- seasonal_summary %>%
  pivot_wider(names_from = TempType, values_from = AvgTemperature) %>%
  arrange(City, Month)


seasonal_wide <- na.omit(seasonal_wide)

#get time series for clustering (only the months)
ts_data_seasonal <- seasonal_wide %>%
  select(-City, -Month)

#calculate DTW distance matrix

dtw_dist_seasonal <- dist(ts_data_seasonal, method = "dtw") 

# hierarchical clustering
hc_seasonal <- hclust(dtw_dist_seasonal, method = "ward.D2")

#cut the dendrogram into clusters 
clusters_seasonal <- cutree(hc_seasonal, k = 4)  # Adjust the number of clusters as needed

site_locations_renamed <- site_locations %>%
  rename(City = site)

#create cluster assignments
site_locations_seasonal <- seasonal_summary %>%
  select(City) %>%
  distinct() %>%
  mutate(Cluster = clusters_seasonal[match(City, unique(seasonal_wide$City))])

#join to add latitude and longitude
site_locations_final <- site_locations_renamed %>%
  left_join(site_locations_seasonal, by = "City")  # Now they match

#sf object for mapping
site_locations_sf_seasonal <- st_as_sf(site_locations_final, coords = c("lon", "lat"), crs = 4326)

#map clusters
ggplot(data = new_zealand_map) +
  geom_sf(fill = "lightblue", color = "black") +
  geom_sf(data = site_locations_sf_seasonal, aes(color = as.factor(Cluster)), size = 3) +
  coord_sf(xlim = c(165, 180), ylim = c(-48, -34), expand = FALSE) + 
  labs(title = "Temperature Measurement Sites Clusters in New Zealand (Seasonal Data)", 
       x = "Longitude", y = "Latitude", color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right")












#method 2

# DTW distance matrix
dtw_dist_seasonal <- dist(ts_data_seasonal, method = "dtw")

#k-means clustering on the distance matrix
set.seed(123)
kmeans_result <- kmeans(as.matrix(dtw_dist_seasonal), centers = 4)


#add cluster assignments back to site locations
site_locations_kmeans <- seasonal_summary %>%
  select(City) %>%
  distinct() %>%
  mutate(Cluster = kmeans_result$cluster[match(City, unique(seasonal_wide$City))])

#rename for joining
site_locations_renamed <- site_locations %>%
  rename(City = site)

#join
site_locations_final_kmeans <- site_locations_renamed %>%
  left_join(site_locations_kmeans, by = "City")


#convert to sf object
site_locations_sf_kmeans <- st_as_sf(site_locations_final_kmeans, coords = c("lon", "lat"), crs = 4326)

#map clusters
ggplot(data = new_zealand_map) +
  geom_sf(fill = "lightblue", color = "black") +
  geom_sf(data = site_locations_sf_kmeans, aes(color = as.factor(Cluster)), size = 3) +
  coord_sf(xlim = c(165, 180), ylim = c(-48, -34), expand = FALSE) + 
  labs(title = "K-Means Clustering of Temperature Measurement Sites in New Zealand", 
       x = "Longitude", y = "Latitude", color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right")






















