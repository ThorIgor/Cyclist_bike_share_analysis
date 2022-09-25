library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(geosphere)
library(sf)

#Number of trips by day of the week
ggplot(data = trips_data) + 
  geom_bar(mapping = aes(x = weekday, fill = member_casual), position = "dodge") +
  labs(title = "Number of trips by day of the week", subtitle = "Casual riders vs members") +
  ylab(label = "Number of trips thousands") + xlab(label = "Day of the week") + 
  guides(fill=guide_legend(title="")) +
  scale_y_continuous(labels = label_number(scale = 0.001)) +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())

#Number of trips by month
ggplot(data = trips_data) + 
  geom_bar(mapping = aes(x = month(started_at), fill = member_casual), position = "dodge") +
  labs(title = "Number of trips by month", subtitle = "Casual riders vs members") +
  ylab(label = "Number of trips thousands") + xlab(label = "Month") + 
  guides(fill=guide_legend(title="")) +
  scale_y_continuous(labels = label_number(scale = 0.001)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())

#Number of trips by time of day
ggplot(data = trips_data) + 
  geom_bar(mapping = aes(x = hour(started_at), fill = member_casual), position = "dodge") +
  labs(title = "Number of trips by time of day", subtitle = "Casual riders vs members") +
  ylab(label = "Number of trips in thousands") + xlab(label = "hour") + 
  guides(fill=guide_legend(title="")) +
  scale_y_continuous(labels = label_number(scale = 0.001)) +
  scale_x_continuous(breaks = 0:23) +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())

#Median and standard deviation of trip duration by member_casual
trips_data %>%
  group_by(member_casual) %>%
  summarise(median = median(trip_duration_sec), sd = sd(trip_duration_sec))

#Trips duration distribution
ggplot(data = trips_data) +
  geom_violin(mapping = aes(x = member_casual, y = trip_duration_sec, fill = member_casual), 
               width = 0.4, outlier.shape = NA) +
  labs(title = "Trips duration distribution", subtitle = "Casual riders vs members") +
  ylab(label = "Duration in sec") + xlab("") +
  scale_y_continuous(limit = c(0, 3000)) +
  theme(panel.background = element_blank(), panel.border = element_blank(), legend.position = "none")


potential_members <- trips_data[trips_data$member_casual == "casual", ]

#Number of trips by day of the week casuals
ggplot(data = potential_members) + 
  geom_bar(mapping = aes(x = weekday, fill = member_casual), position = "dodge") +
  labs(title = "Number of trips by day of the week", subtitle = "Casuals") +
  ylab(label = "Number of trips thousands") + xlab(label = "Day of the week") + 
  guides(fill=guide_legend(title="")) +
  scale_y_continuous(labels = label_number(scale = 0.001)) +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), legend.position = "none")

#Number of trips by month casuals
ggplot(data = potential_members) + 
  geom_bar(mapping = aes(x = month(started_at), fill = member_casual), position = "dodge") +
  labs(title = "Number of trips by month", subtitle = "Casuals") +
  ylab(label = "Number of trips thousands") + xlab(label = "Month") + 
  guides(fill=guide_legend(title="")) +
  scale_y_continuous(labels = label_number(scale = 0.001)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), legend.position = "none")

#Number of trips by time of day casuals
ggplot(data = potential_members) + 
  geom_bar(mapping = aes(x = hour(started_at), fill = member_casual), position = "dodge") +
  labs(title = "Number of trips by time of day", subtitle = "Casuals") +
  ylab(label = "Number of trips in thousands") + xlab(label = "hour") + 
  guides(fill=guide_legend(title="")) +
  scale_y_continuous(labels = label_number(scale = 0.001)) +
  scale_x_continuous(breaks = 0:23) +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), legend.position = "none")


#Downloading map of Chicago
chi_map <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson")

#Getting all points from potential_members
s_geometry <- mapply(function(x,y) st_point(c(x,y)), potential_members$start_lng, potential_members$start_lat, SIMPLIFY = F)
e_geometry <- mapply(function(x,y) st_point(c(x,y)), potential_members$end_lng, potential_members$end_lat, SIMPLIFY = F)
geometry <- c(s_geometry, e_geometry)

#Delete unnecessary variables
rm(s_geometry)
rm(e_geometry)

#Converting list of points to data frame
g <- st_sfc(geometry)
st_crs(g) = 4326
g <- st_sf(id = 1:3755728, geometry = g)

#Merging data frame of points with Chicago regions
j <- st_join(g, chi_map)
s <- group_by(j, by = community) %>% 
  summarise(number_of_rides = n())
r <- st_join(chi_map, s)

#Delete unnecessary variables
rm(geometry)
rm(g)
rm(j)
rm(s)

#Regions with the most trips
head((r[order(r$number_of_rides, decreasing = T), ])[,c("community", "number_of_rides")], 20)


#Number of trips by region
ggplot(data = r) + 
  geom_sf(mapping = aes(fill = number_of_rides)) +
  labs(title = "Number of trips by region", subtitle = "Casuals") +
  scale_fill_continuous(labels = label_number(scale = 0.001)) +
  guides(fill = guide_legend(title = "Number of rides in thousands")) +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  


