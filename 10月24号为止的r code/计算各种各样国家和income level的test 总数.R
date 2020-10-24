httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
data <- readr::read_csv(tf)
data <- data[order(as.Date(data$dateRep, format="%d/%m/%Y")),]
data <- data[order(data$countriesAndTerritories),]
data <- data[data$countriesAndTerritories!='China' & data$countriesAndTerritories!='Kosovo' & 
               data$countriesAndTerritories!='Cases_on_an_international_conveyance_Japan',]
data <- na.omit(data)
data$dateRep <- as.character(data$dateRep)
data$countriesAndTerritories <- factor(data$countriesAndTerritories)
US <- data.frame(dateRep = data[data$geoId=='US',]$dateRep)
US$dateRep <- as.character(US$dateRep)

gc();rm(list = ls())
#setwd("/Users//Users/qidiwang1/Desktop/plot_codes/")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/helper.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/lha_theme.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/plot_functions.R")
merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
plot_data <- data.frame()
for (a in levels(merge$Date)){
  certain_date <- merge[merge$Date==a,]
  asia_confirm <- sum(certain_date[certain_date$region=="Asia",]$confirm)/sum(certain_date$confirm)
  asia_death <- sum(certain_date[certain_date$region=="Asia",]$death)/sum(certain_date$death)
  africa_confirm <- sum(certain_date[certain_date$region=="Africa",]$confirm)/sum(certain_date$confirm)
  africa_death <- sum(certain_date[certain_date$region=="Africa",]$death)/sum(certain_date$death)
  europe_confirm <- sum(certain_date[certain_date$region=="Europe",]$confirm)/sum(certain_date$confirm)
  europe_death <- sum(certain_date[certain_date$region=="Europe",]$death)/sum(certain_date$death)
  oceania_confirm <- sum(certain_date[certain_date$region=="Oceania",]$confirm)/sum(certain_date$confirm)
  oceania_death <- sum(certain_date[certain_date$region=="Oceania",]$death)/sum(certain_date$death)
  northamerica_confirm <- sum(certain_date[certain_date$sub.region=="Northern America",]$confirm)/sum(certain_date$confirm)
  northamerica_death <- sum(certain_date[certain_date$sub.region=="Northern America",]$death)/sum(certain_date$death)
  latin_confirm <- sum(certain_date[certain_date$sub.region=="Latin America and the Caribbean",]$confirm)/sum(certain_date$confirm)
  latin_death <- sum(certain_date[certain_date$sub.region=="Latin America and the Caribbean",]$death)/sum(certain_date$death)
  plot_data <- rbind(plot_data, data.frame(Date = as.Date(a,'%m/%d/%y'),asia_confirm=asia_confirm,africa_confirm=africa_confirm,europe_confirm=europe_confirm,
             oceania_confirm=oceania_confirm,northamerica_confirm=northamerica_confirm,latin_confirm=latin_confirm,asia_death=asia_death,
             africa_death=africa_death,europe_death=europe_death,oceania_death=oceania_death,northamerica_death=northamerica_death,
             latin_death=latin_death))
}
plot_data$Date <- as.Date(plot_data$Date)
plot_data <- plot_data[order(plot_data$Date),]
write.csv(plot_data,'/Users/qidiwang1/Desktop/plot_data.csv')

is_chn = F
ggplot()+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_confirm+plot_data$africa_confirm+plot_data$oceania_confirm+plot_data$latin_confirm+plot_data$europe_confirm+plot_data$northamerica_confirm,fill='Northern America'))+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_confirm+plot_data$africa_confirm+plot_data$oceania_confirm+plot_data$latin_confirm+plot_data$europe_confirm,fill='Europe'))+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_confirm+plot_data$africa_confirm+plot_data$oceania_confirm+plot_data$latin_confirm,fill='Latin America'))+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_confirm+plot_data$africa_confirm+plot_data$oceania_confirm,fill='Oceania'))+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_confirm+plot_data$africa_confirm,fill='Africa'))+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_confirm,fill='Asia'))+
  xlab('Date')+
  ylab('Percentage')+
  ggtitle('Confirm')+
  lha_theme(is_chn)+
  scale_fill_manual(legend.title = element_blank())

ggplot()+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_death+plot_data$africa_death+plot_data$oceania_death+plot_data$latin_death+plot_data$europe_death+plot_data$northamerica_death,fill='Northern America'))+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_death+plot_data$africa_death+plot_data$oceania_death+plot_data$latin_death+plot_data$europe_death,fill='Europe'))+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_death+plot_data$africa_death+plot_data$oceania_death+plot_data$latin_death,fill='Latin America'))+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_death+plot_data$africa_death+plot_data$oceania_death,fill='Oceania'))+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_death+plot_data$africa_death,fill='Africa'))+
  geom_area(aes(x=plot_data$Date,y=plot_data$asia_death,fill='Asia'))+
  xlab('Date')+
  ylab('Percentage')+
  ggtitle('Death')+
  lha_theme(is_chn)+
  scale_fill_manual(legend.title = element_blank())

merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
rt <- data.frame()
levels(merge$sub.region)
for (a in levels(merge$Date)){
  certain_date <- merge[merge$Date==a,]
  aus_zea <- sum(certain_date[certain_date$sub.region=="Australia and New Zealand",]$New.cases)
  central_asia <- sum(certain_date[certain_date$sub.region=="Central Asia",]$New.cases)
  eastern_asia <- sum(certain_date[certain_date$sub.region=="Eastern Asia",]$New.cases)
  east_europe <- sum(sum(certain_date[certain_date$sub.region=="Eastern Europe",]$New.cases))
  latin <- sum(certain_date[certain_date$sub.region=="Latin America and the Caribbean",]$New.cases)
  melanesia <- sum(certain_date[certain_date$sub.region=="Melanesia",]$New.cases)
  micronesia <- sum(certain_date[certain_date$sub.region=="Micronesia",]$New.cases)
  northafrica <- sum(certain_date[certain_date$sub.region=="Northern Africa",]$New.cases)
  northamerica <- sum(certain_date[certain_date$sub.region=="Northern America",]$New.cases)
  northeurope <- sum(certain_date[certain_date$sub.region=="Northern Europe",]$New.cases)
  polynesia <- sum(certain_date[certain_date$sub.region=="Polynesia",]$New.cases)
  se_asia <- sum(certain_date[certain_date$sub.region=="South-eastern Asia",]$New.cases)
  south_asia <- sum(certain_date[certain_date$sub.region=="Southern Asia",]$New.cases)
  south_europe <- sum(certain_date[certain_date$sub.region=="Southern Europe",]$New.cases)
  sub_africa <- sum(certain_date[certain_date$sub.region=="Sub-Saharan Africa",]$New.cases)
  west_asia <- sum(certain_date[certain_date$sub.region=="Western Asia",]$New.cases)
  west_europe <- sum(certain_date[certain_date$sub.region=="Western Europe",]$New.cases)
 
  rt <- rbind(rt, data.frame(Date = as.Date(a,'%m/%d/%y'),Australia_and_New_Zealand = aus_zea, Central_Asia = central_asia, Eastern_Asia = eastern_asia,
                             Eastern_Europe = east_europe, Latin = latin, Melanesia = melanesia, Micronesia = micronesia,
                             Northern_Africa = northafrica, Northern_America = northamerica, Northern_Europe = northeurope,
                             Polynesia = polynesia, South_eastern_Asia = se_asia, Southern_Asia = south_asia, Southern_Europe = south_europe,
                             Sub_Saharan_Africa = sub_africa, Western_Asia = west_asia, Western_Europe = west_europe))


}
View(rt)
write.csv(rt,'/Users/qidiwang1/Desktop/rt_data.csv')

merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
death <- data.frame()

for (a in levels(merge$Date)){
  certain_date <- merge[merge$Date==a,]
  aus_zea <- sum(certain_date[certain_date$sub.region=="Australia and New Zealand",]$New.cases)
  central_asia <- sum(certain_date[certain_date$sub.region=="Central Asia",]$New.cases)
  eastern_asia <- sum(certain_date[certain_date$sub.region=="Eastern Asia",]$New.cases)
  east_europe <- sum(certain_date[certain_date$sub.region=="Eastern Europe",]$New.cases)
  latin <- sum(certain_date[certain_date$sub.region=="Latin America and the Caribbean",]$New.cases)
  melanesia <- sum(certain_date[certain_date$sub.region=="Melanesia",]$New.cases)
  micronesia <- sum(certain_date[certain_date$sub.region=="Micronesia",]$New.cases)
  northafrica <- sum(certain_date[certain_date$sub.region=="Northern Africa",]$New.cases)
  northamerica <- sum(certain_date[certain_date$sub.region=="Northern America",]$New.cases)
  northeurope <- sum(certain_date[certain_date$sub.region=="Northern Europe",]$New.cases)
  polynesia <- sum(certain_date[certain_date$sub.region=="Polynesia",]$New.cases)
  se_asia <- sum(certain_date[certain_date$sub.region=="South-eastern Asia",]$New.cases)
  south_asia <- sum(certain_date[certain_date$sub.region=="Southern Asia",]$New.cases)
  south_europe <- sum(certain_date[certain_date$sub.region=="Southern Europe",]$New.cases)
  sub_africa <- sum(certain_date[certain_date$sub.region=="Sub-Saharan Africa",]$New.cases)
  west_asia <- sum(certain_date[certain_date$sub.region=="Western Asia",]$New.cases)
  west_europe <- sum(certain_date[certain_date$sub.region=="Western Europe",]$New.cases)
  
  death <- rbind(death, data.frame(Date = as.Date(a,'%m/%d/%y'),Australia_and_New_Zealand = aus_zea, Central_Asia = central_asia, Eastern_Asia = eastern_asia,
                             Eastern_Europe = east_europe, Latin = latin, Melanesia = melanesia, Micronesia = micronesia,
                             Northern_Africa = northafrica, Northern_America = northamerica, Northern_Europe = northeurope,
                             Polynesia = polynesia, South_eastern_Asia = se_asia, Southern_Asia = south_asia, Southern_Europe = south_europe,
                             Sub_Saharan_Africa = sub_africa, Western_Asia = west_asia, Western_Europe = west_europe))
  
  
}

death$Date <- as.Date(death$Date)


newdeath <- data.frame(Date = death$Date, East_Asia = death$Eastern_Asia, Second_Waveth = death$Northern_Africa+death$Western_Asia,
                       Third_Wave = death$Northern_Europe+death$Southern_Europe+death$Western_Europe, Forth_Wave = death$Australia_and_New_Zealand+
                         death$Melanesia+death$Micronesia, Fifth_Wave = death$Northern_America, Sixth_Wave = death$Latin,
                       Seventh_Wave = death$Central_Asia+death$South_eastern_Asia+death$Southern_Asia, Eighth_Wave = death$Sub_Saharan_Africa,
                       Ninth_Wave = death$Eastern_Europe)

write.csv(newdeath,'/Users/qidiwang1/Desktop/newconfirm.csv')
ggplot()+
  geom_area(aes(x=death$Date,y=death$Western_Europe+death$Northern_America,fill='Northern America'))+
  geom_area(aes(x=death$Date,y=death$Western_Europe,fill='Western Europe'))+
  geom_area(aes(x=death$Date,y=-(death$Australia_and_New_Zealand+death$Central_Asia+death$Eastern_Asia+death$Eastern_Europe+
                  death$Latin+death$Melanesia+death$Micronesia+death$Northern_Africa+death$Northern_Europe+death$Polynesia+
                  death$South_eastern_Asia+death$Southern_Asia+death$Southern_Europe+death$Sub_Saharan_Africa+death$Western_Asia), fill = 'Rest of World'))+
  geom_area(aes(x=death$Date,y=-death$South_eastern_Asia-death$Sub_Saharan_Africa,fill='Sub Saharan Africa'))+
  geom_area(aes(x=death$Date,y=-death$South_eastern_Asia,fill='South Esatern Asia'))+
  theme(axis.text.y = element_blank(), axis.title = element_blank(),legend.title = element_blank())+
  xlim(as.Date('2020-03-01'),as.Date('2020-05-09'))
  
 


