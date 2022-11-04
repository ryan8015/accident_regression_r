library(dplyr)
library(tidyverse)
library(ggthemes)
library(GGally)
library(scales)
library(ggrepel)
#part1----
#Load in dataset
#change all possible blank cells to NA
accident <- read.csv("data/accidents.csv",na.strings=c(""," ",NA))
summary(accident)
#Examine each column with dplyr
accident %>%
  dplyr::summarise_each(funs(dplyr::n_distinct))

accident %>%
  group_by(Weather.Conditions) %>%
  summarise(.groups = 'drop')

accident %>%
  group_by(Lighting.Conditions) %>%
  summarise(.groups = 'drop')

accident %>%
  group_by(Casualty.Class) %>%
  summarise(.groups = 'drop')

accident %>%
  group_by(Casualty.Severity) %>%
  summarise(.groups = 'drop')

accident %>%
  group_by(Type.of.Vehicle) %>%
  summarise(.groups = 'drop')

#changing date column to a Date type
newdate <- accident %>%
  select(Accident.Date) %>%
  mutate(as.Date(Accident.Date,format="%d/%m/%Y"))

newdate <- newdate %>%
  select(-c(Accident.Date))

accident["Accident.Date"] <- newdate

#Examine data for any missing values 
na_data <- accident[!complete.cases(accident),]
na_data
count(na_data)
#Examine and fix anomalies in “Road Surface” and “1st Road Class
accident %>%
  group_by(Road.Surface) %>%
  summarise(.groups = 'drop')

accident %>%
  group_by(Local.Authority) %>%
  summarise(.groups = 'drop')

accident %>%
  group_by(X1st.Road.Class) %>%
  summarise(.groups = 'drop')
#Change Numbers to Letters
accident$Road.Surface[accident$Road.Surface == "1"] <- "Dry"
accident$Road.Surface[accident$Road.Surface == "2"] <- "Wet/Damp"
accident$Road.Surface[accident$Road.Surface == "3"] <- "Snow"
accident$Road.Surface[accident$Road.Surface == "4"] <- "Frost/Ice"
accident$Road.Surface[accident$Road.Surface == "5"] <- "Flood"
accident$Road.Surface[accident$Road.Surface == "Wet ¨ Damp"] <- "Wet/Damp"

accident$Weather.Conditions[accident$Weather.Conditions == "1"] <- "Fine, no high winds"
accident$Weather.Conditions[accident$Weather.Conditions == "2"] <- "Raining, no high winds"
accident$Weather.Conditions[accident$Weather.Conditions == "3"] <- "Snowing, no high winds"
accident$Weather.Conditions[accident$Weather.Conditions == "4"] <- "Fine, high winds"
accident$Weather.Conditions[accident$Weather.Conditions == "5"] <- "Raining, high winds"
accident$Weather.Conditions[accident$Weather.Conditions == "6"] <- "Snowing, high winds"
accident$Weather.Conditions[accident$Weather.Conditions == "7"] <- "Fog or Mist - if hazard"
accident$Weather.Conditions[accident$Weather.Conditions == "8"] <- "Other"
accident$Weather.Conditions[accident$Weather.Conditions == "9"] <- "Unknown"


accident$X1st.Road.Class[accident$X1st.Road.Class == "1"] <- "Motorway"
accident$X1st.Road.Class[accident$X1st.Road.Class == "2"] <- "A(M)"
accident$X1st.Road.Class[accident$X1st.Road.Class == "3"] <- "A"
accident$X1st.Road.Class[accident$X1st.Road.Class == "4"] <- "B"
accident$X1st.Road.Class[accident$X1st.Road.Class == "5"] <- "C"
accident$X1st.Road.Class[accident$X1st.Road.Class == "6"] <- "Unknown"

accident$Sex.of.Casualty[accident$Sex.of.Casualty == "1"] <- "Male"
accident$Sex.of.Casualty[accident$Sex.of.Casualty == "2"] <- "Female"

accident$Lighting.Conditions[accident$Lighting.Conditions == "1"] <- "Daylight: Street Lights Present"
accident$Lighting.Conditions[accident$Lighting.Conditions == "2"] <- "Daylight: No Street Lights Present"
accident$Lighting.Conditions[accident$Lighting.Conditions == "3"] <- "Daylight: Street Lights Unknown"
accident$Lighting.Conditions[accident$Lighting.Conditions == "4"] <- "Darkness: Street Lights Present::On"
accident$Lighting.Conditions[accident$Lighting.Conditions == "5"] <- "Darkness: Street Lights Present::Off"
accident$Lighting.Conditions[accident$Lighting.Conditions == "6"] <- "Darkness: No Street Lights Present"
accident$Lighting.Conditions[accident$Lighting.Conditions == "7"] <- "Darkness: Street Lights Unknown"

#Removed Local Authority variable and Duplicates 
removed <- select(accident,-c(Local.Authority))
count(distinct(removed))
cleaner_data <- distinct(removed)
write.csv(cleaner_data,'clean_accident.csv')




#3signma
sd <- sd(cleaner_data$Age.of.Casualty, na.rm = TRUE)
mean <- mean(cleaner_data$Age.of.Casualty, na.rm = TRUE)

upper_bound <- mean + (3*sd)
lower_bound <- mean - (3*sd)

outliers_sigma <-cleaner_data %>%
  filter((Age.of.Casualty > upper_bound)| 
           (Age.of.Casualty < lower_bound))

outliers_sigma$Age.of.Casualty


#boxplot
sex <- (as.character(cleaner_data$Sex.of.Casualty))
boxplot.stats(cleaner_data$Age.of.Casualty)$out

#hampel
median_value <- median(cleaner_data$Age.of.Casualty, na.rm = TRUE)
MAD_value <- mad (cleaner_data$Age.of.Casualty, na.rm = TRUE)

upper_bound2 <- median_value + (3*MAD_value)
lower_bound2 <- median_value - (3*MAD_value)

outliers_sigma2 <-cleaner_data %>%
  filter((Age.of.Casualty > upper_bound2)| 
           (Age.of.Casualty < lower_bound2))

outliers_sigma2$Age.of.Casualty

#part2----
#Q1 Weather conditions gender, print how many  (e.g., "males drivers have -----more than females when weather is
ggplot(cleaner_data,aes(x=Sex.of.Casualty, fill=Weather.Conditions,label = scales::percent(prop.table(stat(count))))) + 
  geom_bar(position = 'dodge', stat='count') +
  geom_text_repel(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) +
  labs(x="Gender/Weather Condition",
       y="Collisions") +
  theme_bw()

#Q2Which year has the highest casualties
p <- ggplot(cleaner_data, aes(x = Accident.Date)) + 
  geom_line(stat = "count",color = "steelblue")
p + scale_x_date(date_labels = "%Y")

yearsplit <- split(cleaner_data, format(as.Date(cleaner_data$Accident.Date), "%Y"))
y2014 <- yearsplit$'2014'$Accident.Date
#616 observations
y2015 <- yearsplit$'2015'$Accident.Date
#547 observations
y2016 <- yearsplit$'2016'$Accident.Date
#554 observations
y2017 <- yearsplit$'2017'$Accident.Date
#336 observations
years <- c("2014","2015","2016","2017")
casualties <- c(616,547,554,336)
pyears <- data.frame(years,casualties)

ggplot(data=pyears, aes(x=years, y=casualties,fill=years)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=casualties), vjust=1.6, color="white", size=3.5)+
  theme_bw()


ggplot(data=pyears,aes(x=years,y=casualties,group=1)) +
  geom_point() +
  geom_line() +
  geom_text_repel(aes(label=casualties), vjust=1.6, color="black", size=5.5)+
  theme_bw()
#Q3 lighting conditions vs severity
#plot number of observations, grouped by casualty severity, weather conditions on x axis
plotdata <- cleaner_data %>%
  group_by(Casualty.Severity, Lighting.Conditions) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
#//////////////////////////////#
#barplot of lighting conditions by casualty severity
ggplot(plotdata, aes(x = Casualty.Severity, y = pct,fill = Lighting.Conditions)) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  geom_text_repel(aes(label = lbl), 
                  size = 4, 
                  position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", 
       fill = "Drive Train",
       x = "Casualty Severity::Fatal to Slight Injury",
       title = "Lighting Conditons by Casualty Severity") +
  theme_minimal()

#//////////////////////////////#
#Q4 weatherconditons number of vehicles

plotdata2 <- cleaner_data %>%
  group_by(Number.of.Vehicles, Weather.Conditions) %>%
  summarize(.groups = 'drop',n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
#putting average into vectors 
weather1 <- cleaner_data %>%
  group_by(Number.of.Vehicles, Weather.Conditions) %>%
  filter(Weather.Conditions == 'Fine, high winds')
Fine.high.winds <- mean(weather1[["Number.of.Vehicles"]])

weather2 <- cleaner_data %>%
  group_by(Number.of.Vehicles, Weather.Conditions) %>%
  filter(Weather.Conditions == 'Fine, no high winds')
Fine.no.high.winds <- mean(weather2[["Number.of.Vehicles"]])

weather3 <- cleaner_data %>%
  group_by(Number.of.Vehicles, Weather.Conditions) %>%
  filter(Weather.Conditions == 'Fog or Mist - if hazard')
Fog.or.Mist.if.hazard <- mean(weather3[["Number.of.Vehicles"]])

weather4 <- cleaner_data %>%
  group_by(Number.of.Vehicles, Weather.Conditions) %>%
  filter(Weather.Conditions == 'Other')
Other <- mean(weather4[["Number.of.Vehicles"]])

weather5 <- cleaner_data %>%
  group_by(Number.of.Vehicles, Weather.Conditions) %>%
  filter(Weather.Conditions == 'Raining, high winds')
Raining.high.winds <- mean(weather5[["Number.of.Vehicles"]])

weather6 <- cleaner_data %>%
  group_by(Number.of.Vehicles, Weather.Conditions) %>%
  filter(Weather.Conditions == 'Raining, no high winds')
Raining.no.high.winds <- mean(weather6[["Number.of.Vehicles"]])

weather7 <- cleaner_data %>%
  group_by(Number.of.Vehicles, Weather.Conditions) %>%
  filter(Weather.Conditions == 'Snowing, high winds')
Snowing.high.winds <- mean(weather7[["Number.of.Vehicles"]])

weather8 <- cleaner_data %>%
  group_by(Number.of.Vehicles, Weather.Conditions) %>%
  filter(Weather.Conditions == 'Snowing, no high winds')
Snowing.no.high.winds <- mean(weather8[["Number.of.Vehicles"]])

weather9 <- cleaner_data %>%
  group_by(Number.of.Vehicles, Weather.Conditions) %>%
  filter(Weather.Conditions == 'Unknown')
Unknown<- mean(weather9[["Number.of.Vehicles"]])
#create dataframe with average
average_v <- data.frame(Fine.high.winds,
                        Fine.no.high.winds,
                        Fog.or.Mist.if.hazard,
                        Other,
                        Raining.high.winds,
                        Raining.no.high.winds,
                        Snowing.high.winds,
                        Snowing.no.high.winds,
                        Unknown)
#pivot into longer
longaverage <- pivot_longer(average_v,cols=c(Fine.high.winds,
                                             Fine.no.high.winds,
                                             Fog.or.Mist.if.hazard,
                                             Other,
                                             Raining.high.winds,
                                             Raining.no.high.winds,
                                             Snowing.high.winds,
                                             Snowing.no.high.winds,
                                             Unknown),
                            names_to = "WeatherConditions",
                            values_to= "average")
#//////////////////////////////#
#average no.vehicles barplot

ggplot(longaverage,aes(x=WeatherConditions, y=average,fill=WeatherConditions)) +
  geom_bar(stat='identity') +
  labs(y="Average Number of Vehicles involved") +
  coord_flip() +
  geom_text(label=round(longaverage$average, digits = 3)) +
  theme_bw()

#//////////////////////////////#
#No vehicles by weather condition bubble plot
plotdata2 %>%
  ggplot(aes(x=Weather.Conditions, y=Number.of.Vehicles, size=n, color=n)) +
  geom_point(alpha=0.5) +
  geom_text(label=plotdata2$n, size = 5) + 
  scale_y_continuous(breaks = seq(7))+
  labs(color="Number of Collisions by Colour",
       y="Number of Vehicles Involved") +
  scale_size(range = c(10, 30), name="Number of Collisions by Size") + 
  scale_color_gradient(low="blue", high="red") + 
  theme_fivethirtyeight()

#//////////////////////////////#

#part3----
#clean data no missing values 
regression <- cleaner_data
regression$Weather.Conditions[regression$Weather.Conditions == "Fine, no high winds"] <- "1"
regression$Weather.Conditions[regression$Weather.Conditions == "Raining, no high winds"] <- "2"
regression$Weather.Conditions[regression$Weather.Conditions == "Snowing, no high winds"] <- "3"
regression$Weather.Conditions[regression$Weather.Conditions == "Fine, high winds"] <- "4"
regression$Weather.Conditions[regression$Weather.Conditions == "Raining, high winds"] <- "5"
regression$Weather.Conditions[regression$Weather.Conditions == "Snowing, high winds"] <- "6"
regression$Weather.Conditions[regression$Weather.Conditions == "Fog or Mist - if hazard"] <- "7"
regression$Weather.Conditions[regression$Weather.Conditions == "Other"] <- "8"
regression$Weather.Conditions[regression$Weather.Conditions == "Unknown"] <- "9"

Casualty.Class <- cleaner_data$Casualty.Class
Casualty.Severity<- cleaner_data$Casualty.Severity
Type.of.Vehicle <- cleaner_data$Type.of.Vehicle
Weather.Conditions <- regression$Weather.Conditions
Age.of.Casualty <- cleaner_data$Age.of.Casualty
Weather.Conditions <- as.numeric(Weather.Conditions)

#The function will take a vector as an argument and returns a number
#It will return 0 if it finds a missing value and 1 if it finds a known value.

regression<- data.frame(Casualty.Class,Age.of.Casualty,Weather.Conditions,Type.of.Vehicle,Casualty.Severity)
missDummy <- function(t)
{
  x <- dim(length(t))
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}
regression$dummy <- missDummy(regression$Age.of.Casualty)
# Choose the values with known y values as training data
TrainData<- regression [regression ['dummy']==1,]
# Choose the missing values with(NA) y values as testing data
TestData<- regression [regression ['dummy']==0,]


model<- lm(Age.of.Casualty~Casualty.Severity+Casualty.Class+Weather.Conditions+Type.of.Vehicle, TrainData)

pred<- predict(model, TestData)
pred
pred <- round(pred)

regression$Age.of.Casualty[is.na(Age.of.Casualty)]
regression$Age.of.Casualty[is.na(Age.of.Casualty)]<- pred
regression <- regression[-c(6)]
write.csv(regression,'regression.csv')
