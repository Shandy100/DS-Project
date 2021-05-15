library(tidyverse)
list.files(path = "../New folder")
city <- read.csv("../New folder/city_day.csv")
head(city)
dim(city)
unique(city$AQI_Bucket)
city$AQI_Bucket <- factor(city$AQI_Bucket , levels = c("Poor" , "Very Poor" , "Severe" , "Moderate" , "Satisfactory" , "Good"))
new_data <- city %>%
  separate(Date, sep="-", into = c("Year", "month", "day"))

#visualizing data

ggplot(new_data , aes(x = Year , y= City , fill = AQI_Bucket ))+
  geom_tile()+
  ggtitle("Air Quality Of Cities From 2015-2020")+
  theme_classic()+
  theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold"),
    title = element_text(size=14,face="bold"))+
  xlab("Years")+
  ylab("Cities")
#getting required columns for industrial pollution
indst_pollution <- city%>%
  select(City , Date , NO , CO , SO2 , O3, AQI_Bucket)%>%
  filter(AQI_Bucket == c("Poor" , "Very Poor" , "Severe" , "Moderate" , "Satisfactory" , "Good"))%>%
  group_by(AQI_Bucket)%>%
  arrange(City)
indst_pollution <- na.omit(indst_pollution)
indst_pollution$AQI_Bucket = factor(indst_pollution$AQI_Bucket , levels =  c("Poor" , "Very Poor" , "Severe" , "Moderate" , "Satisfactory" , "Good"))
indst_pollution$Date = as.Date(indst_pollution$Date)
head(indst_pollution)
#getting required columns for vehicle pollution
veh_pollution <- city%>%
  select(City , Date ,NO2 , CO , Benzene , Toluene , Xylene , O3 , AQI_Bucket)%>%
  filter(AQI_Bucket == c("Poor" , "Very Poor" , "Severe" , "Moderate" , "Satisfactory" , "Good"))%>%
  group_by(AQI_Bucket)%>%
  arrange(City)
veh_pollution <- na.omit(veh_pollution)
veh_pollution$AQI_Bucket = factor(veh_pollution$AQI_Bucket , levels =  c("Poor" , "Very Poor" , "Severe" , "Moderate" , "Satisfactory" , "Good"))
veh_pollution$Date = as.Date(veh_pollution$Date)
head(veh_pollution)
veh_year <- veh_pollution %>%
  separate(Date, sep="-", into = c("Year", "month", "day"))%>%
  group_by(Year)%>%
  summarize(
    NO2 = sum(NO2),
    CO= sum(CO),
    Benzene= sum(Benzene),
    Toluene= sum(Toluene),
    Xylene= sum(Xylene),
    Ozone= sum(O3))
veh_year

library(GGally)
ggpairs(data=veh_year, columns=2:7, title="AQI data(Vehicle Pollution)")

#PRE AND POST CORONA EFFECT ON INDUSTRIAL POLLUTION 
indst_pollution_pre <- subset(indst_pollution,
                              Date >= "2015-01-01" & Date <= "2019-12-31")
indst_pollution_post <- subset(indst_pollution,
                               Date >= "2020-01-01")
#PRE AND POST CORONA EFFECT ON VEHICLE POLLUTION 
veh_pollution_pre <- subset(veh_pollution,
                            Date >= "2015-01-01" & Date <= "2019-12-31")
veh_pollution_post <- subset(veh_pollution,
                             Date >= "2020-01-01")
veh_pre_percentage = veh_pollution_pre %>% group_by(AQI_Bucket) %>%
  summarise(count=n()) %>%
  mutate(Percentage=count/sum(count)) 

veh_post_percentage = veh_pollution_post %>% group_by(AQI_Bucket) %>%
  summarise(count=n()) %>%
  mutate(Percentage=count/sum(count))

#visualizing data
ggplot(veh_pre_percentage , aes(x=AQI_Bucket , y=Percentage ))+
  geom_bar(fill = "skyblue " , stat="identity")+
  ggtitle("Pre COVID-19 (Vehicle Pollution)")+
  theme_classic()+
  theme(legend.position = "None",
        axis.text.x=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size = 15),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank())+
  xlab("Air Quality Index")+
  scale_y_continuous(limits=c(0,0.5)) + 
  geom_text(data=veh_pre_percentage, aes(label=paste0(round(Percentage*100,1),"%"),
                                         y=Percentage+0.012), size=4)


ggplot(veh_post_percentage , aes(AQI_Bucket , y=Percentage ))+
  geom_bar(fill = "lightblue", stat="identity")+
  ggtitle("Post COVID-19 (Vehicle Pollution)")+
  theme_classic()+
  theme(legend.position = "None",
        axis.text.x=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size = 15),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank())+
  xlab("Air Quality Index")+ 
  geom_text(data=veh_post_percentage, aes(label=paste0(round(Percentage*100,1),"%"),
                                          y=Percentage+0.012), size=4)
indst_year <- indst_pollution %>%
  separate(Date, sep="-", into = c("Year", "month", "day"))%>%
  group_by(Year)%>%
  summarize(
    NO = sum(NO),
    CO= sum(CO),
    SO2= sum(SO2),
    Ozone= sum(O3))
ggpairs(data=indst_year, columns=2:5, title="AQI data(Industrial Pollution)")


indst_pre_percentage = indst_pollution_pre %>% group_by(AQI_Bucket) %>%
  summarise(count=n()) %>%
  mutate(Percentage=count/sum(count)) 

indst_post_percentage = indst_pollution_post %>% group_by(AQI_Bucket) %>%
  summarise(count=n()) %>%
  mutate(Percentage=count/sum(count))

#visualizing data
ggplot(indst_pre_percentage , aes(x=AQI_Bucket , y=Percentage ))+
  geom_bar(fill = "skyblue " , stat="identity")+
  ggtitle("Pre COVID-19 (Industrial Pollution)")+
  theme_classic()+
  theme(legend.position = "None",
        axis.text.x=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size = 15),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank())+
  xlab("Air Quality Index")+
  scale_y_continuous(limits=c(0,0.5)) + 
  geom_text(data=indst_pre_percentage, aes(label=paste0(round(Percentage*100,1),"%"),
                                           y=Percentage+0.012), size=4)


ggplot(indst_post_percentage , aes(AQI_Bucket , y=Percentage ))+
  geom_bar(fill = "lightblue", stat="identity")+
  ggtitle("Post COVID-19 (Industrial Pollution)")+
  theme_classic()+
  theme(legend.position = "None",
        axis.text.x=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size = 15),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank())+
  xlab("Air Quality Index")+ 
  geom_text(data=indst_post_percentage, aes(label=paste0(round(Percentage*100,1),"%"),
                                            y=Percentage+0.012), size=4)
