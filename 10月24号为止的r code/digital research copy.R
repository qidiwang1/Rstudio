library(readxl)
ITU <- read_excel('/Users/qidiwang1/Desktop/ITU_2018_data.xlsx')


ITU_data <- ITU[ITU$Indicator%in%c('Proportion of households with a fixed line telephone',
                                   'Proportion of households with a mobile cellular telephone',
                                   'Proportion of households with a radio',
                                   'Proportion of households with a TV',
                                   'Proportion of households with electricity',
                                   'Proportion of individuals who used a computer (from any location) in the last 12 months',
                                   'Proportion of individuals who used a mobile cellular telephone'),c(1,2,33:ncol(ITU))]

ITU_data$iso3_name <-    countrycode::countrycode(ITU_data$Country,'country.name','iso3c')
ITU_data<- ITU_data[!is.na(ITU_data$iso3_name),]
ITU_data$latest_value <- NA
ITU_data$max_value <- NA


for (i in c(3:19)){
  for (j in c(1:nrow(ITU_data))){
    if (is.na(ITU_data[j,i])){
      ITU_data$latest_value[j] <- ITU_data$latest_value[j]
    } else {
      ITU_data$latest_value[j] <- ITU_data[j,i]
    }

  }
}
ITU_data$latest_value <- unlist(ITU_data$latest_value)

for (i in c(1:nrow(ITU_data))){
  ITU_data$max_value[i] <- max(t(ITU_data[i,c(3:19)]),na.rm = TRUE)
}

data <- data.frame(Country = ITU_data$Country[ITU_data$Indicator=='Proportion of households with a fixed line telephone'],
                   iso3_name = ITU_data$iso3_name[ITU_data$Indicator=='Proportion of households with a fixed line telephone'])


for (a in levels(factor(ITU_data$Indicator))){
  indicator <- ITU_data[ITU_data$Indicator==a,]
  names(indicator)[21] <- paste(as.character(a),'_',names(indicator)[21],sep='')
  names(indicator)[22] <- paste(as.character(a),'_',names(indicator)[22],sep='')
  data <- data%>%
    left_join(indicator[,c(20,21,22)],by='iso3_name')
}


age <- read_excel('/Users/qidiwang1/Desktop/digital research/age.xlsx')

rural <- read_excel('/Users/qidiwang1/Desktop/digital research/rural.xlsx')
age$iso3_name <- countrycode::countrycode(age$Country,'country.name','iso3c')
rural$iso3_name <- countrycode::countrycode(rural$Country,'country.name','iso3c')


data <- data%>%
  left_join(age,by='iso3_name')%>%
  left_join(rural,by='iso3_name')

education <- read_excel('/Users/qidiwang1/Desktop/digital research/education.xlsx')
education$latest_value <- ''
education$max_value <- ''



for (i in c(4:64)){
  for (j in c(1:nrow(education))){
    if (is.na(education[j,i])){
      education$latest_value[j] <- education$latest_value[j]
    } else {
      education$latest_value[j] <- education[j,i]
    }
    
  }
}
education$latest_value <- unlist(education$latest_value)
for (i in c(1:nrow(education))){
  education$max_value[i] <- max(t(education[i,c(4:64)]),na.rm = TRUE)
}

for (a in levels(factor(education$`Indicator Name`))){
  indicator <- education[education$`Indicator Name`==a,]
  names(indicator)[65] <- paste(as.character(a),'_',names(indicator)[65],sep='')
  names(indicator)[66] <- paste(as.character(a),'_',names(indicator)[66],sep='')
  data <- data%>%
    left_join(indicator[,c(2,65,66)],by='iso3_name')
}



#write_xlsx(data,'/Users/qidiwang1/Desktop/digital research/digital research.xlsx')
levels(factor(merge$C7_Restrictions.on.internal.movement))
merge <- read_excel('/Users/qidiwang1/Desktop/PET_DATA_20201013.xlsx')

merge <- merge%>%
  group_by(Country)%>%
  mutate(Total_Economics_Policy_Score=cumsum(Daily_Economics_Policy_Score))

China <- merge[merge$Country=='China',]
aaa <- China[China$C7_Restrictions.on.internal.movement==2,]
if (nrow(aaa)==0){
  China$Lockdown_days <- 0
} else {
  China$Lockdown_days <- China$JDATE-aaa$JDATE[1]
}

for (a in levels(factor(merge$Country))){
  country <- merge[merge$Country==a,]
  aaa <- country[country$C7_Restrictions.on.internal.movement==2,]
  if (nrow(aaa)==0){
    country$Lockdown_days <- 0
  } else {
    country$Lockdown_days <- country$JDATE-aaa$JDATE[1]
  }
  China <- rbind(China,country)

}
merge <- China[-c(1:275),]
merge$Lockdown_days[merge$Lockdown_days<0] <- 0

ICL <-read.csv('/Users/qidiwang1/Desktop/阿里/1012教授演讲/ICL.csv')
ICL <- ICL[!is.na(ICL$ICL),]
ICL <- ICL%>%
  group_by(iso3_name)%>%
  mutate(ICL_total = cumsum(ICL))
merge$Date <- as.Date(merge$Date)
ICL$Date <- as.Date(ICL$Date,'%m/%d/%y')
data2 <- merge%>%
  left_join(ICL[,c('Date','iso3_name','ICL','ICL_total')],by=c('iso3_name','Date'))

last_last <- data2[data2$index_dt==20200331,]
last <- data2[data2$index_dt==20200630,]
today <- data2[data2$index_dt==20200930,]
data3 <- data%>%
 left_join(last_last, by='iso3_name')
data4 <- data%>%
  left_join(today,by='iso3_name')
data5 <- data%>%
  left_join(last,by='iso3_name')




write_xlsx(list(data3,data4,data5),'/Users/qidiwang1/Desktop/digital research.xlsx')

