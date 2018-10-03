Uber<- read.csv("Uber Request Data.csv") #Loading Uber dataset into a dataframe

library(stringr)
library(lubridate)

Uber$Request.timestamp<- str_replace_all(Uber$Request.timestamp, "\\-", "/") #replacing all - by / to keep all in common format

Uber$Request.timestamp<-as.POSIXct(Uber$Request.timestamp, format="%d/%m/%Y %H:%M") #Converting to similar format

Uber$Drop.timestamp<- str_replace_all(Uber$Drop.timestamp, "\\-", "/") #replacing all - by / to keep all in common format

Uber$Drop.timestamp<- as.POSIXct(Uber$Drop.timestamp, format="%d/%m/%Y %H:%M") #Converting to similar format

Uber$Request_Day<- day(as.Date(Uber$Request.timestamp, format="%d/%m/%Y %H:%M")) #picking out date value from Request_Day

Uber$Request_Hour<- format(Uber$Request.timestamp,"%H") #picking out hour value from Request_Day

library(ggplot2)

#lets visualize demands of cabs from both pickups based on all three status
P1<- ggplot(Uber, aes(x=Uber$Status, fill=Uber$Pickup.point))+geom_bar(stat="count")+labs(title="Frequency of Cab Status for both pickups", x="Cab Status", y="Frequency of Cab Status")
P1
ggsave("Frequency of Cab Status for both pickups.png")

#Based on graphical analysis, many cars are not available at pickup where as many cars cancel from city as pickup. We have to 1st anaylze a reason about this.

#Lets understad how bookings of cabs goes over through the day:

P2<-ggplot(Uber, aes(x=Uber$Request_Hour, fill=Uber$Status))+geom_bar(stat="count")+labs(title="Frequency of cab status requested each hour", x="Car Request Hour", y="Frequency of Requests")
P2
ggsave("Frequency of cab status requested each hour.png")

#As you can see there are spikes for high request from 5am to 10am and later in evening 05pm to 10pm. So considering them as peak hours and updating the dataframe
Uber$Request_Hour<-as.numeric(Uber$Request_Hour) #converting to numeric
Uber$PeakHour<- factor(if_else(Uber$Request_Hour>=05 & Uber$Request_Hour<=10, "Morning_Peak", ifelse(Uber$Request_Hour>=17 & Uber$Request_Hour<=22, "Evening_Peak","Not_Peak"))) #creating column with peak hr or not

#lets analyze how cabs are requested each hour both ways
P3<-ggplot(Uber, aes(x=Uber$Request_Hour, fill=Uber$Pickup.point))+geom_histogram(stat="count")+labs(title="Frequency of cars requested each hour based on Pickup point", x="Car Request Hour", y="Frequency of Requests")
P3
ggsave("Frequency of cars requested each hour based on Pickup point.png")

# On analysis based on above plot, we could observe that requests are high for cabs during morning from city to airport and a much bigger number of cars are cancelling requests in the morning peak hours mostly from City as pickup.
# Also, in evening, requests for cabs are high from airport to city and a majority of cabs are marking them "Not Avaliable" in the evening peak hours majorly from Airport as Pickup.
# This reason has to be evaluated now:

#calculate traveltime in minutes
Uber$TimeDiff<-difftime(Uber$Drop.timestamp, Uber$Request.timestamp, format("%Y-%m-%d %H:%M:%S"))

str(Uber$Request_Hour)
#We need to convert it to factors

Uber$Request_Hour<-factor(Uber$Request_Hour)
str(Uber$Request_Hour)

#Creating boxplot to see per hour, on an average how much is the traveltime
P4<-ggplot(Uber, aes(x=Uber$Request_Hour, y=Uber$TimeDiff, fill=Uber$Pickup.point))+geom_boxplot()+labs(title="Spread of TravelTime for each hour for all cabs", x="Request Hour", y="Travel Time")
P4
ggsave("Spread of TravelTime.png")

#From the boxplot, it can be concluded that time taken to travel from city to airport in morning peak hours is high.
#So this is a primary reason for cabs cancelling in morning hours for a trip to airport. Uber needs to work on this.

#Based on observation, we can conlcude that in morning peak hours, cars from city to airport cancels majority time
# as time taken to travel is more than average during that time.
#Uber can take care of this by providing extra pay or bonus to drivers who take rides to airport during morning peak hrs.
#for No Cars Available during evening hours at airport, Uber has to somehow keep extra cars waiting for that time or provide extra cars.

#Demand can be defined as no of cars requested(Cars Cancelled+No Cars Available+Trip Completed)
#Supply can be defined as only Trip Completed cars

library(dplyr)

#To analyse count of trip status for Airport pickup, during evening peak hours: 
Car_Analysis_Airport<- filter(Uber, Uber$Pickup.point=="Airport", Uber$PeakHour=="Evening_Peak")

CarUnavailable_Airport_Percent<- sum(Car_Analysis_Airport$Status=="No Cars Available")/length(Car_Analysis_Airport$Status)
#this result comes out to be 71.6%, which means this % of cars are not available at airport

#To analyse count of trip status for City pickup, during morning peak hours: 
Car_Analysis_City<- filter(Uber, Uber$Pickup.point=="City", Uber$PeakHour=="Morning_Peak")

CarCancelled_City_Percent<- sum(Car_Analysis_City$Status=="Cancelled")/length(Car_Analysis_City$Status)
#this result comes out to be 47.3%, which means this % of cars are cancelling for a trip to airport


# You can clearly see that cabs are cancelled at morning peak hours at city possibly due to the travel time they need to cover to airport.

#So we can infer from our overall plot that, "Due to long morning peak hour trips from city to airport,
#drivers hesitate and hence cancel rides to airport. As a result of which, there is a shortage of cars
# at airport and during evening peak hours, pasengers face unavailability of cars.

#Uber should take this seriously as its impacting their business.
# Way1: Uber can try providing incentives and special allowances for airport trips during peak hours
#And also some way to make additional cars available at airport during evening peak hours.

# Way2: Uber can track intime of cars to airport by drop time of a rider at airport as starting time and hence calculate
#idle time at airport, if the car is idle for a long time, he should receive a notification that he has to take a ride soon.
#By this, Uber can check on driver if they halt for long hours at airport causing unavailbility.

# Way3: When rider books a car, he/she will just get an OTP and reach Uber pickup points, where each Uber car has to take turn by default
#and pick up riders in a queue and based on their OTP, he/she has to be dropped, hereby engaging all cars at airport, reducing them to make long halts.