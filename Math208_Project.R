#Math208 Final Project

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(knitr)


## @knitr 1
###########Code chunk 1###############
Temp<-as_tibble(read.csv("/Users/apple/Documents/U3/Math208/Final_Project/Project_Happiness_data.csv"))

#Task 1
#create a summary table
Happiness<-Temp%>%filter(Year==2019)%>%group_by(Region)%>%
  select(Region,GDPperCap,TrustGov,Family,Freedom,HealthLifeExp,Generosity)%>%
  summarise(GDP=mean(GDPperCap),
            TrustGov=mean(TrustGov),
            Family=mean(Family),
            Freedom=mean(Freedom),
            LifeExp=mean(HealthLifeExp),
            Generosity=mean(Generosity)
            )%>%mutate_if(is.numeric, ~round(., 3))

Happiness%>%kable()

## @knitr 2
###########Code chunk 2###############
#Bar plot aggregated by region
Happiness%>%gather("feature","value",-Region)%>%
  ggplot(aes(x=reorder(Region,value),y=value,fill=feature))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x = element_text(face = "bold", color = "#993333",
                                   size = 7, angle = 45,hjust = 1))+
  labs(x="Region",y="score value",title = "Six scores in region")
  

## @knitr 3
###########Code chunk 3###############
#GDP score in Region
GDP<-ggplot(Happiness,aes(x=reorder(Region,GDP),y=GDP))+
  geom_bar(stat="identity",width=0.5)+
  theme(axis.text.x = element_text(face = "bold", color = "#993333", size = 7, angle = 45,hjust = 1),
        plot.title = element_text(colour = "#993333"))+
  labs(x="Region",y="GDP per capita",title = "Average GDP score in region")+
  geom_text(aes(label=GDP), vjust=-0.3,  size=2.5)

#Freedom score in Region
Freedom<-ggplot(Happiness,aes(x=reorder(Region,Freedom),y=Freedom))+
  geom_bar(stat="identity",width=0.5)+
  theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 7, angle = 45,hjust = 1),
        plot.title = element_text(colour = "#993333"))+
  labs(x="Region",y="Freedom",title = "Average Freedom score in region")+
  geom_text(aes(label=Freedom), vjust=-0.3,  size=2.5)

grid.arrange(grobs=list(GDP,Freedom),nrow=1,ncol=2)

## @knitr 4
###########Code chunk 4###############
#Life Expectation score in Region
LifeExp<-ggplot(Happiness,aes(x=reorder(Region,LifeExp),y=LifeExp))+
  geom_bar(stat="identity",width=0.5)+
  theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 7, angle = 45,hjust = 1),
        plot.title = element_text(colour = "#993333"))+
  labs(x="Region",y="LifeExp",title = "Average LifeExp score in region")+
  geom_text(aes(label=LifeExp), vjust=-0.3,  size=2.5)

#Family score in Region
Family<-ggplot(Happiness,aes(x=reorder(Region,Family),y=Family))+
  geom_bar(stat="identity",width=0.5)+
  theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 7, angle = 45,hjust = 1),
        plot.title = element_text(colour = "#993333"))+
  labs(x="Region",y="Family",title = "Average Family score in region")+
  geom_text(aes(label=Family), vjust=-0.3,  size=2.5)

Temp%>%filter(Year==2019)%>%filter(Country %in% c("Finland","United States"))%>%
  select(Country,GDPperCap,Family)%>%kable()

grid.arrange(grobs=list(LifeExp,Family),nrow=1,ncol=2)


## @knitr 5
###########Code chunk 5###############
#TrustGov score in Region
TrustGov<-ggplot(Happiness,aes(x=reorder(Region,TrustGov),y=TrustGov))+
  geom_bar(stat="identity",width=0.5)+
  theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 7, angle = 45,hjust = 1),
        plot.title = element_text(colour = "#993333"))+
  labs(x="Region",y="TrustGov",title = "Average TrustGov score in region")+
  geom_text(aes(label=TrustGov), vjust=-0.3,  size=2.5)

# Generosity score in Region
Generosity<-ggplot(Happiness,aes(x=reorder(Region,Generosity),y=Generosity))+
  geom_bar(stat="identity",width=0.5)+
  theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 7, angle = 45,hjust = 1),
        plot.title = element_text(colour = "#993333"))+
  labs(x="Region",y="Generosity",title = "Average Generosity score in region")+
  geom_text(aes(label=Generosity), vjust=-0.3,  size=2.5)

grid.arrange(grobs=list(TrustGov,Generosity),nrow=1,ncol=2)



## @knitr 6
#Task2
###########Code chunk 6###############
#Each year a separate tibble
Happy_2015<-Temp%>%
  mutate(Year2015=ifelse(Year==2015,GDPperCap+TrustGov+Family+Freedom+HealthLifeExp+Generosity,0))%>%
  filter(Year2015!=0)

Happy_2016<-Temp%>%
  mutate(Year2016=ifelse(Year==2016,GDPperCap+TrustGov+Family+Freedom+HealthLifeExp+Generosity,0))%>%
  filter(Year2016!=0)

Happy_2017<-Temp%>%
  mutate(Year2017=ifelse(Year==2017,GDPperCap+TrustGov+Family+Freedom+HealthLifeExp+Generosity,0))%>%
  filter(Year2017!=0)

Happy_2018<-Temp%>%
  mutate(Year2018=ifelse(Year==2018,GDPperCap+TrustGov+Family+Freedom+HealthLifeExp+Generosity,0))%>%
  filter(Year2018!=0)

Happy_2019<-Temp%>%
  mutate(Year2019=ifelse(Year==2019,GDPperCap+TrustGov+Family+Freedom+HealthLifeExp+Generosity,0))%>%
  filter(Year2019!=0)

# tibble "new" as each country, each year's average
new<-tibble(  Country=c(rep("",141)),
              Region=c(rep("",141)),
              Year2015=c(rep(0,141)),
              Year2016=c(rep(0,141)),
              Year2017=c(rep(0,141)),
              Year2018=c(rep(0,141)),
              Year2019=c(rep(0,141)))

new$Country=Happy_2015$Country
new$Region=Happy_2015$Region

#fill in the data
for(i in 1:141){
  for(j in 1:141){
    if(new$Country[i]==Happy_2015$Country[j]){
      new$Year2015[i]=Happy_2015$Year2015[j]
    }
    if(new$Country[i]==Happy_2016$Country[j]){
      new$Year2016[i]=Happy_2016$Year2016[j]
    }
    if(new$Country[i]==Happy_2017$Country[j]){
      new$Year2017[i]=Happy_2017$Year2017[j]
    }
  }
  for(j in 1:140){
    if(new$Country[i]==Happy_2018$Country[j]){
      new$Year2018[i]=Happy_2018$Year2018[j]
    }
  }
  for(j in 1:141){
    if(new$Country[i]==Happy_2019$Country[j]){
      new$Year2019[i]=Happy_2019$Year2019[j]
    }
  }
}

#recode country United Arab Emirate's as NA
new[[20,6]]=NA
kable(head(new,n=10))

## @knitr 7
###########Code chunk 7###############
#min_max helper function
min_max <- list(
  min = ~min(.x, na.rm = TRUE), 
  max = ~max(.x, na.rm = TRUE)
)

#summary table of each region, each year, min and max
new2<-new %>% group_by(Region)%>%
  summarise(across(where(is.numeric), min_max))%>%
  pivot_longer(cols = c(contains("min"),contains("max")))%>%
  mutate(year=rep(c("2015","2016","2017","2018","2019"),12))%>%
  mutate(min_max=rep(c("min","min","min","min","min","max","max","max","max","max"),6))%>%
  select(-name)

kable(head(new2,n=10))

## @knitr 8
###########Code chunk 8###############
#min score plot
min<-new2%>%filter(min_max=="min")%>%group_by(Region)
min_plot<-ggplot(min,aes(x=year,y=value,group=Region,col=Region))+
  labs(title = "Min score of region over time")+
  theme(plot.title = element_text(colour = "#993333"))+
  geom_point()+geom_line()

min_plot

## @knitr 9
###########Code chunk 9###############
#Max score plot
max<-new2%>%filter(min_max=="max")%>%group_by(Region)
max_plot<-ggplot(max,aes(x=year,y=value,group=Region,col=Region))+
  labs(title = "Max score of region over time")+
  theme(plot.title = element_text(colour = "#993333"))+
  geom_point()+geom_line()

max_plot

## @knitr 10
###########Code chunk 10###############
#Task 3
#Top 10 countries with largest average
new3<-new%>%group_by(Country)%>%
  mutate(Avg=(Year2015+Year2016+Year2017+Year2018+Year2019)/5)%>%
  select(Country,Avg)%>%arrange(desc(Avg))

kable(head(new3,n=10))

## @knitr 11
###########Code chunk 11###############
#Top 10 countries with largest positive difference between 2015 and 2019
new4<-new%>%mutate(Diff=Year2019-Year2015)%>%select(Country,Diff)%>%arrange(desc(Diff))

kable(head(new4,n=10))


