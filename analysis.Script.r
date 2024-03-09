
#step 1 load package

library(tidyr)
library(dplyr)
library(janitor)
library(skimr)
library(readr)
library(conflicted)

#step 2 collected data

q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#step 3 Wrangle data and combine into a single file

colnames(q1_2019)
colnames(q1_2020)

# Rename columns to make them consistent with q1_2020
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype))

# Inspect the dataframes and look for incongruencies
str(q1_2019)
str(q1_2020)

# Convert ride_id and rideable_type to character so that they can stack correctly

q1_2019 <- mutate(q1_2019,ride_id=as.character(ride_id)
                  ,rideable_type=as.character(rideable_type))

# Stack individual quarter's data frames into one big data frame

all_trips <-bind_rows(q1_2019,q1_2020)#,q4_2019),q4_2019,q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020

all_trips <- all_trips %>% 
  select(-c(start_lat,start_lng, end_lat, end_lng, birthyear, gender,"tripduration"))

# STEP 4: Clean up and prepare for data analysis

colnames(all_trips) #List of column names
nrow(all_trips)   #How many rows are in data frame?
dim(all_trips)    #Dimensions of the data frame?
    head(all_trips)  #See the first 6 rows of data frame. Also tail(all_trips)
    str(all_trips)   #See list of columns and data types (numeric, character, etc)
    summary(all_trips) #Statistical summary of data. Mainly for numerics
    
# Begin by seeing how many observations fall under each usertype
  table(all_trips$member_casual)
    
# Reassign to the desired values (we will go with the current 2020 labels)
    
  all_trips <- all_trips %>% 
      mutate(member_casual=recode(member_casual,"Subscriber"="member","Customer"="casual"))
    
# Check to make sure the proper number of observations were reassigned
    table(all_trips$member_casual)
    
# Add columns that list the date, month, day, and year of each ride
    all_trips$date <- as.Date(all_trips$started_at)
    all_trips$month <- format(as.Date(all_trips$date), "%m") 
    all_trips$day <- format(as.Date(all_trips$date), "%d")
    all_trips$year <- format(as.Date(all_trips$date),"%y")
    all_trips$day_of_week <-format(as.Date(all_trips$date), "%A")

 # Add a "ride_length" calculation to all_trips (in seconds)
 all_trips$ride_length <-difftime(all_trips$ended_at,all_trips$started_at)
 
 # Inspect the structure of the columns
 str(all_trips)
 
 # Convert "ride_length" from Factor to numeric so we can run calculations on the data
 
 is.factor(all_trips$ride_length)
 all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
 is.numeric(all_trips$ride_length)
 
 # Remove "bad" data
 all_trips_v2 <- all_trips[!(all_trips$start_station_name =="HQ QR"| all_trips$ride_length<0),]
 
 # STEP 5: CONDUCT DESCRIPTIVE ANALYSIS
 
 # Descriptive analysis on ride_length (all figures in seconds)
    
  mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
  median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
  max(all_trips_v2$ride_length) #longest ride
  min(all_trips_v2$ride_length) #shortest ride 
 
# You can condense the four lines above to one line using summary() on the specific attribute
  summary(all_trips_v2$ride_length)
  
# Compare members and casual users
  
  aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=mean)
  aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=median)
  aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=max)
  aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=min)
  
# See the average ride time by each day for members vs casual users
  aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual +all_trips_v2$day_of_week,FUN=mean)
  
# Notice that the days of the week are out of order. Let's fix that.
  all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
  aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$day_of_week,FUN=mean)
  
# analyze ridership data by type and weekday
  all_trips_v2 %>% 
    mutate(weekday = wday(started_at, label = TRUE)) %>%  
    group_by(member_casual, weekday) %>%  
    summarise(number_of_rides = n()
              ,average_duration = mean(ride_length)) %>%    
    arrange(member_casual, weekday)
  
# Let's visualize the number of rides by rider type  
    
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
  
# Let's create a visualization for average duration

  all_trips_v2 %>% 
    mutate(weekday=wday(started_at,label=TRUE)) %>% 
    group_by(member_casual,weekday) %>% 
    summarise(number_of_ride=n()
    ,average_duration=mean(ride_length)) %>% 
      arrange(member_casual, weekday) %>%
      ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
      geom_col(position = "dodge")
  
# EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
 
   # Total and Average number of weekly rides by rider type
  summary_wd <- all_trips_v2 %>% 
    mutate(weekday= wday(started_at,label=TRUE)) %>% 
    group_by(member_casual,weekday) %>% 
    summarise(number_of_ride=n()
    ,average_duration = mean(ride_length)) %>%    
    arrange(member_casual, weekday)
    write_csv(summary_wd, "summary_ride_length_weekday.csv")   
  
     # Total and Average number of monthly rides by rider type  
  summary_month <- all_trips_v2 %>% 
    mutate(month=month(started_at,label=TRUE)) %>% 
    group_by(month,member_casual) %>% 
    summarise(number_of_ride=n()
    ,average_duration = mean(ride_length)) %>% 
    arrange(month,member_casual)
    write_csv(summary_month,"summary_ride_lenth_month.csv")
   
    
     # Stations most used by each user group
  summary_station <- all_trips_v2 %>% 
    mutate(station=start_station_name) %>% 
    drop_na(start_station_name) %>%
    group_by(start_station_name,member_casual) %>% 
    summarise(number_of_ride=n()) %>% 
    arrange(number_of_ride)
  write_csv(summary_station,"summary_station_ride.csv")
