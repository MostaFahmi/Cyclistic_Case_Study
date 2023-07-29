#Installing the packages
install.packages("tidyverse")
install.packages("readr")
install.packages("janitor")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
#Loading the packages
library(tidyverse)
library(readr)
library(janitor)
library(tidyr)
library(dplyr)
library(ggplot2)


#Importing & naming the datasets| name = read_csv("file_name")
Jan2021= read_csv("CyclisticTripdata202101.csv")
Feb2021= read_csv("CyclisticTripdata202102.csv")
Mar2021= read_csv("CyclisticTripdata202103.csv")
Apr2021= read_csv("CyclisticTripdata202104.csv")
May2021= read_csv("CyclisticTripdata202105.csv")
June2021= read_csv("CyclisticTripdata202106.csv")
July2021= read_csv("CyclisticTripdata202107.csv")
Aug2021= read_csv("CyclisticTripdata202108.csv")
Sep2021= read_csv("CyclisticTripdata202109.csv")
Oct2021= read_csv("CyclisticTripdata202110.csv")
Nov2021= read_csv("CyclisticTripdata202111.csv")
Dec2021= read_csv("CyclisticTripdata202112.csv")
#Check the data type of the datasets' fields| str("dataset_name")
str(Jan2021)
str(Feb2021)
str(Mar2021)
str(Apr2021)
str(May2021)
str(June2021)
str(July2021)
str(Aug2021)
str(Sep2021)
str(Oct2021)
str(Nov2021)
str(Dec2021)

#Creating the dataframe| name = bind_rows(all_datasets)
MonthlyTrip_df = bind_rows(Jan2021,Feb2021, Mar2021, Apr2021,May2021,June2021,July2021,Aug2021,Sep2021,Oct2021,Nov2021,Dec2021)

#Cleaning columns names
MonthlyTrip_df = clean_names(MonthlyTrip_df)

#Removing all the full empty columns and rows
MonthlyTrip_df = remove_empty(MonthlyTrip_df, which = c())

#Removing duplicates
MonthlyTrip_df = unique(MonthlyTrip_df)

#Check on the set of values in member_casual field
all(MonthlyTrip_df$member_casual %in% c("member","casual"))

#Removing leading & trailing spaces
MonthlyTrip_df %>% 
  mutate_all(trimws)
#Removing in between excessive spaces
MonthlyTrip_df %>% 
  mutate_all(~gsub("\\s+"," ", .))


#Extracting weekday with wday() and creating day_of_week new column
MonthlyTrip_df$day_of_week= wday(MonthlyTrip_df$started_at, label = T, abbr = F)


#Extracting Time-Hour with as.POSIXct() and creating starting_hour new column
MonthlyTrip_df$starting_hour = format(as.POSIXct(MonthlyTrip_df$started_at),"%H")


#Extracting month with as.Date()
MonthlyTrip_df$starting_month = format(as.Date(MonthlyTrip_df$started_at), "%m")



#Calculate tripduration = ended_at - started_at
MonthlyTrip_df$tripduration = difftime(MonthlyTrip_df$ended_at,MonthlyTrip_df$started_at,units= "min")
#Converting "tripduration" from difftime to numeric
MonthlyTrip_df$tripduration = as.numeric(MonthlyTrip_df$tripduration)


#Removing data with tripduration =< 0
MonthlyTrip_df = MonthlyTrip_df[!(MonthlyTrip_df$tripduration<=0),]

#Check for tripduration outliers using boxplot()
boxplot(MonthlyTrip_df$tripduration)
q1 = quantile(MonthlyTrip_df$tripduration,0.25)
q3= quantile(MonthlyTrip_df$tripduration,0.75)
iqr = IQR(MonthlyTrip_df$tripduration)
Cleaned_MonthlyTrip_df = MonthlyTrip_df %>% 
  filter(!(MonthlyTrip_df$tripduration> q3 +1.5*iqr | MonthlyTrip_df$tripduration< q1 -1.5*iqr))
#Check on tripduration statistics summary after filtration
summary(MonthlyTrip_df$tripduration)
summary(Cleaned_MonthlyTrip_df$tripduration)


#Creating cleaned csv data file to use it later in Tableau
write.csv(Cleaned_MonthlyTrip_df, file= "Cyclistic_trips.csv")




#Creating viz for Number of Rides for Member Type in Weekdays
#We use options(scipen =) to remove scientific values from our graphs
options(scipen = 1000)
ggplot(data = Cleaned_MonthlyTrip_df) + 
  geom_bar(mapping = aes(x= day_of_week, fill = member_casual), position = "dodge") +
  labs(x = "Weekday", y = "Number of Rides", fill = "Member Type", title = "Number of Rides for Member Type in Weekdays")+
  theme(axis.text = element_text(size = 6))
ggsave("number_of_rides_for_member_type_weekdays.png")



#Creating viz for Number of Rides for Member Type in Months
options(scipen = 1000)
ggplot(data = Cleaned_MonthlyTrip_df) + 
  geom_bar(mapping = aes(x= starting_month, fill = member_casual), position = "dodge") +
  labs(x = "Month", y = "Number of Rides", fill = "Member Type", title = "Number of Rides for Member Type in Months") +
  theme(axis.text = element_text(size = 7))
ggsave("number_of_rides_for_member_type_months.png")



#Creating viz for Number of Rides for Member Type per daily hrs
options(scipen = 1000)
ggplot(data = Cleaned_MonthlyTrip_df) + 
  geom_bar(mapping = aes(x= starting_hour, fill = member_casual), position = "dodge") +
  labs(x = "Hour", y = "Number of Rides", fill = "Member Type", title = "Number of Rides for Member Type in Daily Hours") +
  theme(axis.text = element_text(size = 7)) + 
  facet_wrap(~day_of_week)
ggsave("number_of_rides_for_member_type_hours.png")



#The average trip duration within weekdays viz between casuals & members
average_tripduration = Cleaned_MonthlyTrip_df %>% 
  group_by(day_of_week, member_casual) %>% 
  summarise(avg_tripduration= mean(tripduration))

options(scipen = 1000)
ggplot(data = average_tripduration) + 
  geom_col(mapping = aes(x=day_of_week, y= avg_tripduration, fill = member_casual), position = "dodge") +
  labs(x = "Weekday", y = "AVG Trip Duration in sec.", fill = "Member Type", title = "Average Trip Duration in Seconds") +
  theme(axis.text = element_text(size = 7))
ggsave("avg_tripduration_seconds.png")


#arrange the top 10 stations for the number of trips for members
Cleaned_MonthlyTrip_df %>%
  filter(member_casual=="member") %>% 
  count(start_station_name) %>%
  top_n(10) %>%
  arrange(desc(n)) %>%
  ggplot() +
  geom_col(mapping = aes(y = reorder(start_station_name, n), x = n)) +
  geom_text(mapping = aes(y = reorder(start_station_name, n), x = n, label = n), vjust = -0.5) +
  labs(x = "Count") +
  theme(axis.text = element_text(size = 7))










