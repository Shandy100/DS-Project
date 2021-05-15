library(rvest)
library(robotstxt)
library(dplyr)
library(ggplot2)
dataset = read.csv("pi.csv")
da=read.csv("pi sum of digit.csv")
View(dataset)
#str(dataset)

one<-dataset[1,2:5]
a1<-sum(one)
two<-dataset[2,2:5]
a2<-sum(two)
three<-dataset[3,2:5]
a3<-sum(three)
four<-dataset[4,2:5]
a4<-sum(four)
five<-dataset[5,2:5]
a5<-sum(five)
six<-dataset[6,2:5]
a6<-sum(six)
seven<-dataset[7,2:5]
a7<-sum(seven)
eight<-dataset[8,2:5]
a8<-sum(eight)
nine<-dataset[9,2:5]
a9<-sum(nine)
zero<-dataset[10,2:5]
a10<-sum(zero)


detail <- c("One", "Two","Three","Four","Five","Six","Seven","Eight","Nine","Zero")
total <- c(a1, a2,a3,a4,a5,a6,a7,a8,a9,a10)

da<- data.frame(detail,total)
View(da)

ggplot(data = da, aes(x = detail, y = total))+
  geom_bar(stat = "identity", fill = "purple")+
  labs(x = "Digit",
       y = "Number of Occurrence",
       title = "PI DIgits")
pie <-  ggplot(da, aes(x = "", y=Occ, fill = factor(Digit))) +
  geom_bar(width = 1, stat ="identity")+
  labs(fill="Sum of Digit",
       x=NULL, y=NULL, title="Occurrence of each digit")
pie + coord_polar(theta = "y", start = 0)
