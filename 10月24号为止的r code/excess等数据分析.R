estimate <- read.csv('/Users/qidiwang1/Desktop/ourworldindata.csv')
merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
merge$iso3_name <- as.character(merge$iso3_name)
estimate$iso3_name <- as.character(estimate$Code)
estimate$index_dt <- as.numeric(as.character(as.Date(estimate$Date,'%b %d, %Y'),'%Y%m%d'))
library(dplyr)
library(zoo)
merge <- merge[merge$iso3_name%in%levels(factor(estimate$iso3_name)),]
merge <- merge%>%
  left_join(estimate,by=c('iso3_name','index_dt'))
merge$iso3_name <- factor(merge$iso3_name)

merge <- merge%>%
  group_by(iso3_name)%>%
  mutate(newcase_7day = rollmean(New_case,7,fill=NA,align='right'),
         newdeath_7day = rollmean(New_death,7,fill=NA,align='right'))
merge <- merge[,-c(1,6:18)]
merge$iso3_name <- factor(merge$iso3_name)

data <- c()
for (a in levels(merge$iso3_name)){
  country <- merge[merge$iso3_name==a,]

  if (sum(country$ICL..mean...estimated.infections.,na.rm = TRUE)>0 & sum(country$IHME..mean...estimated.infections.,na.rm = TRUE)>0 & sum(country$LSHTM..median.,na.rm = TRUE)>0)
    data <- c(data,as.character(a))
}
merge <- merge[merge$iso3_name%in%data,]
  

time_series <- data.frame()
merge$Date.x <- factor(merge$Date.x)
for (a in levels(merge$Date.x)){
  certain_date <- merge[merge$Date.x==a,]
  south_asia_case <- sum(certain_date$newcase_7day[certain_date$Sub_region=='Southern Asia'],na.rm = TRUE)
  south_asia_death <- sum(certain_date$newdeath_7day[certain_date$Sub_region=='Southern Asia'],na.rm = TRUE)
  south_asia_ICL <- sum(certain_date$ICL..mean...estimated.infections.[certain_date$Sub_region=='Southern Asia'],na.rm = TRUE)
  south_asia_IHME <- sum(certain_date$IHME..mean...estimated.infections.[certain_date$Sub_region=='Southern Asia'],na.rm = TRUE)
  south_asia_LSHTM <- sum(certain_date$LSHTM..median.[certain_date$Sub_region=='Southern Asia'],na.rm = TRUE)
  europe_case <- sum(certain_date$newcase_7day[certain_date$Region=='Europe'],na.rm = TRUE)
  europe_death <- sum(certain_date$newdeath_7day[certain_date$Region=='Europe'],na.rm = TRUE)
  europe_ICL <- sum(certain_date$ICL..mean...estimated.infections.[certain_date$Region=='Europe'],na.rm = TRUE)
  europe_IHME <- sum(certain_date$IHME..mean...estimated.infections.[certain_date$Region=='Europe'],na.rm = TRUE)
  europe_LSHTM <- sum(certain_date$LSHTM..median.[certain_date$Region=='Europe'],na.rm = TRUE)
  latin_case <- sum(certain_date$newcase_7day[certain_date$Sub_region=='Latin America and the Caribbean'],na.rm = TRUE)
  latin_death <- sum(certain_date$newdeath_7day[certain_date$Sub_region=='Latin America and the Caribbean'],na.rm = TRUE)
  latin_ICL <- sum(certain_date$ICL..mean...estimated.infections.[certain_date$Sub_region=='Latin America and the Caribbean'],na.rm = TRUE)
  latin_IHME <- sum(certain_date$IHME..mean...estimated.infections.[certain_date$Sub_region=='Latin America and the Caribbean'],na.rm = TRUE)
  latin_LSHTM <- sum(certain_date$LSHTM..median.[certain_date$Sub_region=='Latin America and the Caribbean'],na.rm = TRUE)
  time_series <- rbind(time_series,data.frame(Date = as.character(a),
                                              south_asia_case = south_asia_case,
                                              south_asia_death = south_asia_death,
                                              south_asia_ICL = south_asia_ICL,
                                              south_asia_IHME = south_asia_IHME,
                                              south_asia_LSHTM = south_asia_LSHTM,
                                              europe_case = europe_case,
                                              europe_death = europe_death,
                                              europe_ICL = europe_ICL,
                                              europe_IHME = europe_IHME,
                                              europe_LSHTM = europe_LSHTM,
                                              latin_case = latin_case,
                                              latin_death = latin_death,
                                              latin_ICL = latin_ICL,
                                              latin_IHME = latin_IHME,
                                              latin_LSHTM = latin_LSHTM))
}

time_series$Date <- as.Date(time_series$Date)
for (i in c(1:nrow(time_series))){
  for (j in c(1:ncol(time_series))){
    if (time_series[i,j]==0)
      time_series[i,j] <- as.double(NA)
  }
}

write_xlsx(time_series,'/Users/qidiwang1/Desktop/estimate.xlsx')

library(ggplot2)
ggplot()+
  geom_line(aes(x=time_series$Date,y=time_series$south_asia_case,color = 'cases'))+
  geom_line(aes(x=time_series$Date,y=time_series$south_asia_death,color = 'death'))+
  geom_line(aes(x=time_series$Date,y=time_series$south_asia_ICL,color = 'ICL'))+
  geom_line(aes(x=time_series$Date,y=time_series$south_asia_IHME,color = 'IHME'))+
  geom_line(aes(x=time_series$Date,y=time_series$south_asia_LSHTM,color = 'LSHTM'))


merge$ICL..mean...estimated.infections.
spain <- merge[merge$Country=='Spain',]
spain$New_case
spain <- spain%>%
  mutate(newcase_sum = rollsum(New_case,7,fill=NA,align='right'),
         ICL_sum = rollsum(ICL..mean...estimated.infections.,7,fill=NA,align='right'))
excess_deaths <- read.csv('/Users/qidiwang1/Desktop/excess_deaths.csv')
excess_deaths$Date <- as.Date(excess_deaths$end_date,'%m/%d/%y')
spain$Date <- as.Date(spain$Date.x)
data <- excess_deaths%>%
  left_join(spain[,c('Date','newcase_sum','ICL_sum')],by='Date')
write_xlsx(data,'/Users/qidiwang1/Desktop/excess_data.xlsx')

data <- merge%>%
  group_by(Country)%>%
  mutate(newcase_7day = rollmean(New_case,7,fill=NA,align = 'right'))
newcase = data.frame(Date=data$Date[data$Country=='Spain'],
                     Spain = data$newcase_7day[data$Country=='Spain'],
                     France = data$newcase_7day[data$Country=='France'],
                     Netherlands = data$newcase_7day[data$Country=='Netherlands'],
                     Belgium = data$newcase_7day[data$Country=='Belgium'],
                     Poland = data$newcase_7day[data$Country=='Poland'],
                     Czechia = data$newcase_7day[data$Country=='Czechia'],
                     Israel = data$newcase_7day[data$Country=='Israel'],
                     Greece = data$newcase_7day[data$Country=='Greece'])
write_xlsx(newcase,'/Users/qidiwang1/Desktop/newcase.xlsx')

sum(merge$Total_death[merge$index_dt==20200828&merge$Region=='Europe'])-sum(merge$Total_death[merge$index_dt==20200701&merge$Region=='Europe'])
merge$Total_death











