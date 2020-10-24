library(dplyr)
merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
ICL <- read.csv('/Users/qidiwang1/Desktop/ICL.csv')
merge$Date<-as.Date(merge$Date)
ICL$Date <- as.Date(ICL$Date,'%m/%d/%y')
data <- merge%>%
  left_join(ICL,by=c('Date','iso3_name'))

library(sqldf)
data <- data[!is.na(data$ICL),]
AZ <- sqldf('select Date,sum(ICL) as aus_zea, sum(Population)  from data where Sub_region =="Australia and New Zealand" group by Date')
EA <- sqldf('select Date,sum(ICL) as east_asia, sum(Population) from data where Sub_region =="Eastern Asia" group by Date')
LA <- sqldf('select Date,sum(ICL) as latin_america, sum(Population) from data where Sub_region =="Latin America and the Caribbea" group by Date')
NNA <- sqldf('select Date,sum(ICL) as north_america, sum(Population) from data where Sub_region =="Northern America" group by Date')
SEA <- sqldf('select Date,sum(ICL) as south_east_asia, sum(Population) from data where Sub_region =="South-eastern Asia" group by Date')
SA <- sqldf('select Date,sum(ICL) as south_asia, sum(Population) from data where Sub_region =="Southern Asia" group by Date')
SAF <- sqldf('select Date,sum(ICL) as sub_saharan_africa, sum(Population) from data where Sub_region =="Sub-Saharan Africa" group by Date')
WA <- sqldf('select Date,sum(ICL) as west_asia, sum(Population) from data where Sub_region =="Western Asia" group by Date')
Europe <- data[data$Region=='Europe',]
Europe <- Europe%>%
  group_by(Date)%>%
  mutate(europe = sum(ICL),
         europe_Population=sum(Population))
Europe$Date <- as.Date(Europe$Date)
#EE <- sqldf('select Date,sum(ICL) as europe, sum(Population) from Europe group by Date')
East_Asia <- data[data$Sub_region=='Eastern Asia',]
East_Asia <- East_Asia%>%
  group_by(Date)%>%
  mutate(east_asia = sum(ICL),
         east_asia_Population = sum(Population))
East_Asia$Date <- as.Date(East_Asia$Date)
View(East_Asia)

Latin <- data[data$Sub_region=='Latin America and the Caribbean',]

Latin <- Latin%>%
  group_by(Date)%>%
  mutate(latin = sum(ICL),
         latin_Population = sum(Population))

SAF <- data[data$Sub_region=='Sub-Saharan Africa',]
SAF <- SAF%>%
  group_by(Date)%>%
  mutate(SAF = sum(ICL),
         saf_Population = sum(Population))

AZ$Date <- as.Date(AZ$Date)
#EA$Date <- as.Date(EA$Date)
#LA$Date <- as.Date(LA$Date)
NNA$Date <- as.Date(NNA$Date)
SEA$Date <- as.Date(SEA$Date)
SA$Date <- as.Date(SA$Date)
#SAF$Date <- as.Date(SAF$Date)
WA$Date <- as.Date(WA$Date)
#EE$Date <- as.Date(EE$Date)

data2 <- AZ%>%
  left_join(NNA,by ='Date')%>%
  left_join(SEA,by ='Date')%>%
  left_join(SA,by ='Date')%>%
  left_join(WA,by ='Date')%>%
  left_join(Europe[Europe$Country=='Albania',c('Date','europe','europe_Population')],by ='Date')%>%
  left_join(East_Asia[East_Asia$Country=='Japan',c('Date','east_asia','east_asia_Population')],by ='Date')%>%
  left_join(Latin[Latin$Country=='Argentina',c('Date','latin','latin_Population')],by ='Date')%>%
  left_join(SAF[SAF$Country=='Angola',c('Date','SAF','saf_Population')],by ='Date')
library(writexl)
write_xlsx(data2,'/Users/qidiwang1/Desktop/ICL_million_case.xlsx')



  