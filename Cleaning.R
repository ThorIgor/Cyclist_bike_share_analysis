library(tidyverse)
library(lubridate)

Sys.setlocale("LC_TIME", "English")

#Get file names from Trips_data_2022
files <- list.files(path = "Trips_data", full.name = TRUE)

#Import data 
trips_data = data.frame()
for (file in files){
  trips_data <- rbind(read_csv(file), trips_data)
}

#Delete unnecessary variables
rm(files)
rm(file)

#How many NA values in each column
na_in_columns <- data.frame()
for (i in 1:ncol(trips_data)){
  na_in_columns[1, colnames(trips_data)[i]] <- sum(is.na(trips_data[ , i]))
}
print(na_in_columns, row.names = F)

#Delete unnecessary variables
rm(na_in_columns)
rm(i)

#Drop rows with NA in "end_lat" and "end_lng"
trips_data <- trips_data[!is.na(trips_data$end_lat),]

#Drop rows with NA in start_station or end_station
trips_data <- trips_data[!(is.na(trips_data$start_station_id) |
                             is.na(trips_data$end_station_id) |
                             is.na(trips_data$start_station_name) |
                             is.na(trips_data$end_station_name)), ]

#Create variable trip duration in secs
trips_data$trip_duration_sec <- difftime(trips_data$ended_at, trips_data$started_at, units = "secs")

#How many rows with negative duration
nrow(trips_data[trips_data$trip_duration_sec <= 0, ])

#Drop rows with negative duration
trips_data <- trips_data[trips_data$trip_duration_sec > 0, ]

#Change type of "member_casual" from char to factor
trips_data$member_casual <- factor(trips_data$member_casual)

#Change type of "rideable_type" from char to factor
trips_data$rideable_type <- factor(trips_data$rideable_type)

#Sort data in descending order by "started_at" variable
trips_data <- trips_data[order(rev(trips_data$started_at)),]

#Create variable weekday
trips_data$weekday <- wday(trips_data$started_at, week_start = 1, label = T)

#Drop variables start_station_id and end_station_id
trips_data = subset(trips_data, select = -c(start_station_id, end_station_id))

