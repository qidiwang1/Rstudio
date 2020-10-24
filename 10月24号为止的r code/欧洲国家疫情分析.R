merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
Europe <- merge[merge$Region=='Europe',]
Europe <- Europe[!is.na(Europe$Total_tests),]
test <- aggregate(Europe$Total_tests,by=factor(Europe$Date),FUN = sum)
install.packages("sqldf")
library(sqldf)

Europe$Total_cases
Europe$Total_tests
Europe$Total_death
Europe$New_death
new_data <- sqldf("select Date,sum(Total_cases),sum(Total_tests),sum(New_death),sum(Population) from Europe group by Date")
library(ggplot2)
ggplot()+
  geom_line(aes(x=as.Date(new_data$Date),y=new_data$`sum(Total_cases)`/new_data$`sum(Total_tests)`))
ggplot()+
  geom_line(aes(as.Date(new_data$Date),y=new_data$`sum(New_death)`))
ggplot()+
  geom_line(aes(x=as.Date(new_data$Date),y=new_data$`sum(Total_tests)`/new_data$`sum(Population)`*1000000))
library(dplyr)
library(zoo)
Europe <- Europe%>%
  group_by(Country)%>%
  mutate(newcase_7d = rollmean(New_case,7,fill=NA,align='right'),
         newdeath_7d = rollmean(New_death,7,fill=NA,align='right'))
View(Europe[Europe$index_dt==20200925,c('Country','newcase_7d','newdeath_7d')])

india <- merge[merge$Country=='India',]
ggplot()+
  geom_line(aes(x=as.Date(india$Date),y=india$Total_cases/india$Total_tests))

ggplot()+
  geom_line(aes(x=as.Date(india$Date),y=india$New_death))
ggplot()+
  geom_line(aes(x=as.Date(india$Date),y=india$Total_death/india$Total_cases))


Africa <- merge[merge$Region=='Africa',]
Africa <- Africa[!is.na(Africa$Total_tests),]

new_data <- sqldf("select Date,sum(Total_cases),sum(Total_tests),sum(New_death),sum(Population) from Africa group by Date")
ggplot()+
  geom_line(aes(x=as.Date(new_data$Date),y=new_data$`sum(Total_cases)`/new_data$`sum(Total_tests)`))
ggplot()+
  geom_line(aes(as.Date(new_data$Date),y=new_data$`sum(New_death)`))
ggplot()+
  geom_line(aes(x=as.Date(new_data$Date),y=new_data$`sum(Total_tests)`/new_data$`sum(Population)`*1000000))

Africa <- Africa%>%
  group_by(Country)%>%
  mutate(newcase_7d = rollmean(New_case,7,fill=NA,align='right'),
         newdeath_7d = rollmean(New_death,7,fill=NA,align='right'))
View(Africa[Africa$index_dt==20200925,c('Country','newcase_7d','newdeath_7d')])


merge$Country
spain <- merge[merge$Country=='Spain',]
spain <- spain[spain$New_death>=0,]
spain <- spain%>%
  mutate(newdeath_7d=rollmean(New_death,7,fill=NA,align='right'),
         newcase_7d = rollmean(New_case,7,fill=NA,align='right'))
ggplot()+
  geom_line(aes(x=as.Date(spain$Date),y=spain$newcase_7d))  
ggplot()+
  geom_line(aes(x=as.Date(spain$Date),y=spain$Total_cases/spain$Total_tests))

ggplot()+
  geom_line(aes(x=as.Date(spain$Date),y=spain$newdeath_7d))

ggplot()+
  geom_line(aes(x=as.Date(spain$Date),y=spain$Total_death/spain$Total_cases))  
  
france <- merge[merge$Country=='France',]
france <- france%>%
  mutate(newdeath_7d = rollmean(New_death,7,fill=NA,align='right'),
         newcase_7d = rollmean(New_case,7,fill=NA,align='right'))

ggplot()+
  geom_line(aes(x=as.Date(france$Date),y=france$newdeath_7d))
ggplot()+
  geom_line(aes(x=as.Date(france$Date),y=france$newcase_7d))   

russia <- merge[merge$Country=='Russia',]
russia <- russia%>%
  mutate(newdeath_7d = rollmean(New_death,7,fill=NA,align='right'),
         newcase_7d = rollmean(New_case,7,fill=NA,align='right'))
ggplot()+
  geom_line(aes(x=as.Date(russia$Date),y=russia$Total_cases/russia$Total_tests))
ggplot()+
  geom_line(aes(x=as.Date(russia$Date),y=russia$newdeath_7d))

ggplot()+
  geom_line(aes(x=as.Date(russia$Date),y=russia$newcase_7d))  
  
UK <- merge[merge$Country=='United_Kingdom',]
UK <- UK%>%
  mutate(newdeath_7d = rollmean(New_death,7,fill=NA,align='right'),
         newcase_7d = rollmean(New_case,7,fill=NA,align='right')) 
ggplot()+
  geom_line(aes(x=as.Date(UK$Date),y=UK$Total_cases/UK$Total_tests))
ggplot()+
  geom_line(aes(x=as.Date(UK$Date),y=UK$newdeath_7d))

ggplot()+
  geom_line(aes(x=as.Date(UK$Date),y=UK$newcase_7d))   
  
  
  
  
Germany <- merge[merge$Country=='Germany',]
Germany <- Germany%>%
  mutate(newdeath_7d = rollmean(New_death,7,fill=NA,align='right'),
         newcase_7d = rollmean(New_case,7,fill=NA,align='right')) 
ggplot()+
  geom_line(aes(x=as.Date(Germany$Date),y=Germany$Total_cases/Germany$Total_tests))
ggplot()+
  geom_line(aes(x=as.Date(Germany$Date),y=Germany$newdeath_7d))

ggplot()+
  geom_line(aes(x=as.Date(Germany$Date),y=Germany$newcase_7d))   

ukraine <- merge[merge$Country=='Ukraine',]
ukraine <- ukraine%>%
  mutate(newdeath_7d = rollmean(New_death,7,fill=NA,align='right'),
         newcase_7d = rollmean(New_case,7,fill=NA,align='right')) 
ggplot()+
  geom_line(aes(x=as.Date(ukraine$Date),y=ukraine$Total_cases/ukraine$Total_tests))
ggplot()+
  geom_line(aes(x=as.Date(ukraine$Date),y=ukraine$newdeath_7d))

ggplot()+
  geom_line(aes(x=as.Date(ukraine$Date),y=ukraine$newcase_7d))   

netherlands <- merge[merge$Country=='Netherlands',]
netherlands <- netherlands%>%
  mutate(newdeath_7d = rollmean(New_death,7,fill=NA,align='right'),
         newcase_7d = rollmean(New_case,7,fill=NA,align='right')) 
ggplot()+
  geom_line(aes(x=as.Date(netherlands$Date),y=netherlands$Total_cases/netherlands$Total_tests))
ggplot()+
  geom_line(aes(x=as.Date(netherlands$Date),y=netherlands$newdeath_7d))

ggplot()+
  geom_line(aes(x=as.Date(netherlands$Date),y=netherlands$newcase_7d))   


italy <- merge[merge$Country=='Italy',]
italy <- italy%>%
  mutate(newdeath_7d = rollmean(New_death,7,fill=NA,align='right'),
         newcase_7d = rollmean(New_case,7,fill=NA,align='right')) 
ggplot()+
  geom_line(aes(x=as.Date(italy$Date),y=italy$Total_cases/italy$Total_tests))
ggplot()+
  geom_line(aes(x=as.Date(italy$Date),y=italy$newdeath_7d))

ggplot()+
  geom_line(aes(x=as.Date(italy$Date),y=italy$newcase_7d)) 



czechia <- merge[merge$Country=='Czechia',]

czechia <- czechia%>%
  mutate(newdeath_7d = rollmean(New_death,7,fill=NA,align='right'),
         newcase_7d = rollmean(New_case,7,fill=NA,align='right')) 
ggplot()+
  geom_line(aes(x=as.Date(czechia$Date),y=czechia$Total_cases/czechia$Total_tests))
ggplot()+
  geom_line(aes(x=as.Date(czechia$Date),y=czechia$newdeath_7d))

ggplot()+
  geom_line(aes(x=as.Date(czechia$Date),y=czechia$newcase_7d))   

romania <- merge[merge$Country=='Romania',]
romania <- romania%>%
  mutate(newdeath_7d = rollmean(New_death,7,fill=NA,align='right'),
         newcase_7d = rollmean(New_case,7,fill=NA,align='right')) 
ggplot()+
  geom_line(aes(x=as.Date(romania$Date),y=romania$Total_cases/romania$Total_tests))
ggplot()+
  geom_line(aes(x=as.Date(romania$Date),y=romania$newdeath_7d))

ggplot()+
  geom_line(aes(x=as.Date(romania$Date),y=romania$newcase_7d))  


East_Europe <- merge[merge$Sub_region=='Eastern Europe'&merge$Country!='Russia',]
East_Europe <- East_Europe[!is.na(Europe$Total_tests),]
levels(factor(East_Europe$Country))
View(East_Europe)
install.packages("sqldf")
library(sqldf)

new_data <- sqldf("select Date,sum(New_case),sum(Total_cases),sum(Total_tests),sum(New_death),sum(Population) from East_Europe group by Date")

new_data<-new_data%>%
  mutate(newcase_7d = rollmean(`sum(New_case)`,7,fill=NA,align='right'),
         newdeath_7d = rollmean(`sum(New_death)`,7,fill=NA,align='right'))
library(ggplot2)
ggplot()+
  geom_line(aes(x=as.Date(new_data$Date),y=new_data$`sum(Total_cases)`/new_data$`sum(Total_tests)`))
ggplot()+
  geom_line(aes(as.Date(new_data$Date),y=new_data$newcase_7d))
ggplot()+
  geom_line(aes(as.Date(new_data$Date),y=new_data$newdeath_7d))
ggplot()+
  geom_line(aes(x=as.Date(new_data)))
ggplot()+
  geom_line(aes(x=as.Date(new_data$Date),y=new_data$`sum(Total_tests)`/new_data$`sum(Population)`*1000000))
library(dplyr)
library(zoo)
Europe <- Europe%>%
  group_by(Country)%>%
  mutate(newcase_7d = rollmean(New_case,7,fill=NA,align='right'),
         newdeath_7d = rollmean(New_death,7,fill=NA,align='right'))
View(Europe[Europe$index_dt==20200925,c('Country','newcase_7d','newdeath_7d')])
