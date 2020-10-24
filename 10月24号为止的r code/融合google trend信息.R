library(countrycode)
contact <- read.csv('/Users/qidiwang1/Desktop/contact.csv')
vaccine <- read.csv('/Users/qidiwang1/Desktop/vaccine.csv')
contact$iso3_name <- countrycode(contact$Country, origin = 'country.name', destination = 'iso3c')
vaccine$iso3_name <- countrycode(vaccine$Country, origin = 'country.name', destination = 'iso3c')
library(readxl)
merge <- read_excel('/Users/qidiwang1/Desktop/PET_DATA_20200823.xlsx')

data <- merge[,c('Date','index_dt','Country','iso3_name','New_case','confirm_ratio','Region')]
data$iso3_name <- as.character(data$iso3_name)
data$Date <- as.Date(data$Date)
contact$iso3_name <- as.character(contact$iso3_name)
contact$Date <- as.Date(contact$Date,'%m/%d/%y')
vaccine$iso3_name <- as.character(vaccine$iso3_name)
vaccine$Date <- as.Date(vaccine$Date,'%m/%d/%y')
library(dplyr)
data <- data%>%
  left_join(contact[,c('Date','contact','iso3_name')],by=c('Date','iso3_name'))%>%
  left_join(vaccine[,c('Date','vaccine','iso3_name')],by=c('Date','iso3_name'))

library(zoo)
data2 <- data%>%
  group_by(Country)%>%
  mutate(Newcase_7day = rollmean(New_case, k = 7, align = 'right',fill = NA),
         Contact_7day = rollmean(contact, k = 7, align = 'right',fill = NA),
         Vaccine_7day = rollmean(vaccine, k = 7, align = 'right',fill = NA))

data3 <- data2%>%
  group_by(Country)%>%
  mutate(newcase_ratio = Newcase_7day/max(Newcase_7day,na.rm = TRUE),
         contact_ratio = Contact_7day/max(Contact_7day,na.rm = TRUE),
         vaccine_ratio = Vaccine_7day/max(Vaccine_7day,na.rm = TRUE))

today <- data3[data3$index_dt==20200822,]
library(ggplot2)
ggplot()+
  geom_point(aes(x=today$newcase_ratio,y=today$contact_ratio,color = today$Region))
ggplot()+
  geom_point(aes(x=today$newcase_ratio,y=today$vaccine_ratio,color = today$Region))

ggplot()+
  geom_point(aes(x=log(today$Newcase_7day),y=log(today$Contact_7day),color = today$Region))

data3$Country <- factor(data3$Country)
for (a in levels(data3$Country)){
  country <- data3[data3$Country==a,]
  p <- ggplot()+
    geom_line(aes(x=country$Date,y=country$newcase_ratio,color = 'newcase'))+
    geom_line(aes(x=country$Date,y=country$contact_ratio,color = 'contact'))+
    geom_line(aes(x=country$Date,y=country$vaccine_ratio,color = 'vaccine'))+
    xlab('')+
    ylab('Ratio to max num')+
    ggtitle(as.character(a))
  ggsave(p,filename = paste('/Users/qidiwang1/Desktop/google_trend/',as.character(a),'.png'))
}


library(writexl)
write_xlsx(data3,'/Users/qidiwang1/Desktop/google_trend.xlsx')
