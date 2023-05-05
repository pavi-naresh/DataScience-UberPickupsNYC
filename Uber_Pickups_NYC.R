#installing Package
install.packages("lubridate") 
install.packages("dplyr")
install.packages("DT")
install.packages("ggplot2")
install.packages("scales")
install.packages("ggthemes")
install.packages("sqldf")

#Loading Package
library(lubridate)
library(dplyr)
library(DT)
library(ggplot2)
library(scales)
library(ggthemes)
library(sqldf)

# Importing Data sets
data_apr14 <- read.csv("D:/MCT/Data Analytics - R/Project/Uber/Dataset/uber-raw-data-apr14.csv")
data_may14 <- read.csv("D:/MCT/Data Analytics - R/Project/Uber/Dataset/uber-raw-data-may14.csv")
data_jun14 <- read.csv("D:/MCT/Data Analytics - R/Project/Uber/Dataset/uber-raw-data-jun14.csv")
data_jul14 <- read.csv("D:/MCT/Data Analytics - R/Project/Uber/Dataset/uber-raw-data-jul14.csv")
data_aug14 <- read.csv("D:/MCT/Data Analytics - R/Project/Uber/Dataset/uber-raw-data-aug14.csv")
data_sep14 <- read.csv("D:/MCT/Data Analytics - R/Project/Uber/Dataset/uber-raw-data-sep14.csv")

#Merging all the data sets                                      
data_14<-rbind(data_apr14,data_may14,data_jun14,data_jul14,data_aug14,data_sep14)

#Identifying the column names and Data types
colnames(data_14);
typeof(data_14$Date.Time) 
typeof(data_14$Lat) 
typeof(data_14$Lon)  
typeof(data_14$Base)  

str(data_14)


#Data Cleaning
data_14$Date.Time<-as.POSIXct(data_14$Date.Time,format= "%m/%d/%Y %H:%M:%S")
data_14$Time<-format(as.POSIXct(data_14$Date.Time,format="%m/%d/%Y %H:%M:%S"),format = "%H:%M:%S")
data_14$Date.Time<-ymd_hms(data_14$Date.Time)


#Extracting the day number,month name and day name from Date.Time Column to New Column
data_14$Day<-format(day(data_14$Date.Time))
data_14$Month<-format(month(data_14$Date.Time,label=TRUE))
data_14$dayoftheweek<-format(wday(data_14$Date.Time,label=TRUE))


#Extracting the Hour,Minute and Second from Date.Time Column to New column
data_14$Time<-format(as.POSIXct(data_14$Date.Time,format="%m/%d/%Y %H:%M:%S"),format = "%H:%M:%S")
data_14$Date.Time<-ymd_hms(data_14$Date.Time)
data_14$hour<-factor(hour(hms(data_14$Time)))
data_14$minute<-factor(minute(hms(data_14$Time)))
data_14$second<-factor(second(hms(data_14$Time)))
head(data_14)


##Q1----------------------------------------------------------------
#What time of the day most booking happens?

hour_data<-data_14 %>%
  group_by(hour)%>%
  dplyr:: summarise(Total=n())

datatable(hour_data)


ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "tomato3", color = "black") +
  ggtitle("Trips by the Hour") +
  xlab("Hour")+
  ylab("Rides")+
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


##Q2-----------------------------------------------------------------------
#How many rides were booked from April to September?

month_data<-data_14 %>%
  group_by(Month) %>%
  summarise(Total=n())

datatable(month_data)

ggplot(month_data,aes(Month,Total))+
  geom_point(color="Dark Red",size=5)+
  ylim(0,1030000)+
  xlab("Months")+
  ylab("Rides")+
  ggtitle("Trips by Months") +
  theme(legend.position = "none")


##Other way Representation
ggplot(month_data, mapping=aes(x=reorder(Month,-Total),y=Total)) + 
  xlab("Months")+
  ylab("Rides")+
  geom_bar( stat = "identity", mapping=aes(fill = Month)) +
  ggtitle("Trips by Months") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

##Q3----------------------------------------------------------------------------
#How many rides were booked at each time of the day, split by all 6 months?


month_hour <- data_14 %>%
  group_by(Month,hour) %>%
  dplyr::summarise(Total = n())

datatable(month_hour)

ggplot(month_hour, aes(hour,Total, fill = Month)) +
  xlab("Hour")+
  ylab("Rides")+
  geom_bar( stat = "identity",color="black",size=1) +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

##Q4----------------------------------------------------------------------------
#How many rides were booked in the highest booked month ,split by time of the day.

sept_hour<-data_14%>%
  group_by(hour,Month) %>%
  filter(Month=="Sep")%>%
  summarise(Total=n())


datatable(sept_hour)



ggplot(sept_hour,aes(hour,Total,fill=hour))+
  xlab("Hours-September")+
  ylab("Rides")+
  ggtitle("Trip by hours in September")+
  geom_histogram(stat="identity",fill="Blue",color="black",binwidth = 30)+
  scale_y_continuous(labels=comma)

##Q5----------------------------------------------------------------------------
#How many rides were booked in the lowest booked month, split by the time of the day
april_hour<-data_14%>%
  group_by(hour,Month) %>%
  filter(Month=="Apr")%>%
  summarise(Total=n())

datatable(april_hour)


ggplot(april_hour,aes(hour,Total))+
  xlab("Hours-April")+
  ylab("Rides")+
  ggtitle("Trip by hours in April")+
  geom_point(stat="identity",color="Red",size=3)+
  ylim(0,46000)

#Other way
ggplot(april_hour,aes(hour,Total,fill=hour))+
  geom_bar(stat = 'identity')+
  ggtitle("Trip by hours in April")+
  xlab("Hours-April")+
  ylab("Rides")+
  theme_minimal()+
  scale_y_continuous()


##Q6------------------------------------------------------------------------------
#How many trips occur in each month, split by the day of the week?

month_weekday<-data_14%>%
  group_by(Month,dayoftheweek)%>%
  summarise(Total=n())


datatable(month_weekday)

ggplot(month_weekday,aes(Month,Total,fill=dayoftheweek))+
  geom_bar(stat="identity",width=0.4)+
  xlab("Months")+
  ylab("Rides")+
  ggtitle("Trip by Month, Split by Weekday")
scale_y_continuous(labels=comma)


##checking
a<-sqldf("select sum(Total) as Monday from month_weekday where dayoftheweek='Mon'");a
b<-sqldf("select sum(Total) as Tuesday from month_weekday where dayoftheweek='Tue'");b
c<-sqldf("select sum(Total) as Wednesday from month_weekday where dayoftheweek='Wed'");c
d<-sqldf("select sum(Total) as Thursday from month_weekday where dayoftheweek='Thu'");d
e<-sqldf("select sum(Total) as Friday from month_weekday where dayoftheweek='Fri'");e
f<-sqldf("select sum(Total) as Saturday from month_weekday where dayoftheweek='Sat'");f
g<-sqldf("select sum(Total) as Sunday from month_weekday where dayoftheweek='Sun'");g


##Q7----------------------------------------------------------------------------
#How many rides are associated with each of five Uber base.

unique(data_14$Base)

base_data<-data_14 %>%
  group_by(Base)%>%
  summarise(Total=n())

datatable(base_data)

ggplot(base_data,aes(Base,Total))+
  ggtitle("Trip by Bases")+
  xlab("Base")+
  ylab("Rides")+
  theme_minimal()+
  geom_bar(stat="identity",fill ="Dark green")+
  geom_label(mapping=aes(label=Total),size=3,color='white',fill='black')+
  scale_y_continuous(label=comma)

##Q8-------------------------------------------------------------------
#Is there any changes over the months for rides associated with each base.

ggplot(data_14,aes(Base,fill=Month))+
  geom_bar(position='dodge2',color='black',size=1)+
  theme_minimal()+
  ggtitle("Trips by Bases and Month")+
  ylab("Rides")+
  theme(plot.title = element_text(hjust=1))+
  scale_y_continuous(labels=comma)


##Trial
ggplot(data_14,aes(Base,fill=Month))+
  geom_boxplot()



##Q9----------------------------------------------------------------------------
#Is there change over days of week, for rides associated with each base.

base_data_1<-data_14 %>%
  group_by(Base,dayoftheweek)%>%
  summarise(Total=n())


datatable(base_data_1)

ggplot(base_data_1,aes(Base,Total,fill=dayoftheweek))+
  geom_line(aes(group=dayoftheweek),color="grey")+
  geom_point(aes(color=dayoftheweek))+
  ggtitle("Trips by Bases and Days of the Week")+
  theme(plot.title = element_text(hjust=1))+
  theme_minimal()+
  ylab("Rides")+
  scale_y_continuous(labels=comma)

#Other Trial
ggplot(data_14,aes(Base,fill=dayoftheweek))+
  geom_bar(position='dodge2')+
  theme_minimal()+
  ggtitle("Trips by Bases and Days of the week")+
  ylab("Rides")+
  theme(plot.title = element_text(hjust=0.5))+
  scale_y_continuous(labels=comma)

#Q10----------------------------------------------------------------
#Heat Visualization based on Day,Month,Hour,Day of Week and Base.
#Heat map by day and hour
day_hour<-data_14%>%
  group_by(Day,hour)%>%
  summarise(Total=n())
datatable(day_hour)
ggplot(day_hour,aes(Day,hour,fill=Total))+
  geom_tile(color='White')+
  ggtitle("Heat Map by Hour and Day")+
  theme_minimal()

#Heat map by Month and day
day_month<-data_14%>%
  group_by(Day,Month)%>%
  summarise(Total=n())
datatable(day_month)
ggplot(day_month,aes(Day,Month,fill=Total))+
  geom_tile(color='White')+
  ggtitle("Heat Map by Month and Day")

#Heat map by Month and Day of Week
dayofweek_month<-data_14%>%
  group_by(dayoftheweek,Month)%>%
  summarise(Total=n())
datatable(dayofweek_month) 
ggplot(dayofweek_month,aes(dayoftheweek,Month,fill=Total))+
  geom_tile(color='White')+
  ggtitle("Heat Map by Month and Day of the week")

#Heat map by Month and Base
month_base<-data_14%>%
  group_by(Month,Base)%>%
  summarise(Total=n())
datatable(month_base)    
ggplot(month_base,aes(Month,Base,fill=Total))+
  geom_tile(Color='White',size=1)+
  ggtitle("Heat Map by Month and Base")


#Geographical Representation---------------------------


min(data_14$Lat)
max(data_14$Lat)    
min(data_14$Lon)
max(data_14$Lon)

min_long<- -74.929
max_long<- -72.0666
min_lat<- 39.6569
max_lat<- 42.1166

ggplot(data_14, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  theme_classic()+
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING APR-SEP by BASE")

