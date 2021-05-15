library("readxl")
library(rvest)
library(robotstxt)
library(dplyr)
library(ggplot2)
DE<-read_excel("Per capita vs country1.xlsx")
View(DE)
Wo<-read_excel("world.xlsx")
I<-read_excel("india.xlsx")
View(I)
Con<-read_excel("Continent.xlsx")
View(Con)
Ci<-read_excel("C.xlsx")
View(Ci)
Sca<-read_excel("Sca.xlsx")
View(Sca)
P1<-read_excel("Piechart1.xlsx")
aa<-read_excel("aa.xlsx")
C<-read_excel("china.xlsx")


#
a<-ggplot(DE[tail(order(DE$A), 11), ], aes(x = Entity, y = A))+
  geom_bar(stat = "identity", fill = "#A9A9A9")+
  labs(x = "Country",
       y = "Annual CO2 Emission(Million Tonnes)",
       title = "Top 10 vs World Annual Co2 Emission")
a + theme_light() 
#
b<-ggplot(DE[tail(order(DE$A), 11), ], aes(x = Entity, y = P))+
  geom_bar(stat = "identity", fill = "#2F4F4F")+ 
  labs(x = "Country",
       y = "PerCapita(Million Tonnes)",
       title = "Top 10 vs World PerCapita Co2 Emission")
b + coord_flip()+theme(
  panel.background = element_rect(fill = "#FFE4B5", colour = "#6D9EC1",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
ggplot()+
  geom_line(I, mapping=aes(x = Year , y = B,group=1,color = "India",), size = 2)+
  geom_line(C, mapping=aes(x = Year , y = AB,group=1,color = "China"), size = 2)+
  labs(x = "Year",
       y = "Annual CO2 Emission(Million Tonnes)",
       title = "India Vs China Line Graph(2009-2019)")
#
ggplot(I, aes(x=Year))+
  geom_line(mapping=aes(y = W,group=1,color = "World"), size = 5)+
  geom_line(mapping=aes(y = B,group=1,color = "India"), size = 2)+
  geom_line(mapping=aes(y = China,group=1,color = "China"), size = 2)+
  geom_line(mapping=aes(y = USA,group=1,color = "USA"), size = 2)+
  geom_line(mapping=aes(y = Russia,group=1,color = "Russia"), size = 2)+
  geom_line(mapping=aes(y = Japan,group=1,color = "Japan"), size = 2)+
  labs(x = "Year",
       y = "Annual CO2 Emission(Million Tonnes)",
       title = "India Vs World Line Graph(2009-2019)")
#pichart for total con
pie <-  ggplot(Con, aes(x = "", y=Ann, fill = factor(Continents))) +
  geom_bar(width = 1, stat ="identity")+
  labs(fill="Legend",
       x=NULL, y=NULL, title="Continents Annual Co2 Emission")
pie + coord_polar(theta = "y", start = 0)
#Piechart for con
pie <-  ggplot(Ci, aes(x = "", y=Ann, fill = factor(Continents))) +
  geom_bar(width = 1, stat ="identity")+
  labs(fill="Legend",
       x=NULL, y=NULL, title="Continents Annual Co2 Emission")
pie + coord_polar(theta = "y", start = 0)
#
pie <-  ggplot(P1, aes(x = "", y=Annn, fill = factor(Continents))) +
  geom_bar(width = 1, stat ="identity")+
  labs(fill="Legend",
       x=NULL, y=NULL, title="Asia Vs Ind&CHN AnnualCo2 Emission")
pie + coord_polar(theta = "y", start = 0)
pie <-  ggplot(P1, aes(x = "", y=I, fill = factor(Continents))) +
  geom_bar(width = 1, stat ="identity")+
  labs(fill="Legend",
       x=NULL, y=NULL, title="Asia Vs Ind&CHN PerCap Emission")
pie + coord_polar(theta = "y", start = 0)

ggplot(data = Wo, aes(x = Year))+
  geom_line(aes (y = ANNC,color="World"),size = 2)+
  geom_line(aes (y = Pop,color="Population"),size = 2)+
  labs(x = "Year",
       y = "Population",
       title = "Population Vs Annual Co2 Emission")
#histogram
#ggplot(Sca, aes(x=Diff)) +
#  geom_histogram(binwidth = 3, fill = "blue")

ggplot(Sca, aes(x = Country, y = Diff))+
  geom_bar(stat = "identity", fill = "purple")+
  labs(x = "Country",
       y = "Growth of Co2 Emission",
       title = "Co2 Emission Growth(2009-19)")



library(forecast)
library(MLmetrics)
IPCCE <- c(4.7,4.5,4.76,4.85,4.87,4.85,4.83,4.77,4.71,4.72,4.77,4.72)
timeseries <- ts(IPCCE,start = c(2019,1),frequency = 12)
fit <- auto.arima(timeseries)
forecasting <- forecast(fit,5)
plot(forecasting, main="Per Capita of India")
