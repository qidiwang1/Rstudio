library(dplyr)
library(readxl)
data <- read_excel('/Users/qidiwang1/Desktop/疫情数据库/cov.xlsx')
data <- data[,c(1:11)]

#data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/cov.csv')
data <- data[order(as.Date(data$dateRep, format="%d/%m/%Y")),]
data <- data[order(data$countriesAndTerritories),]
data <- data[data$countriesAndTerritories!='China' & data$countriesAndTerritories!='Kosovo' & 
               data$countriesAndTerritories!='Cases_on_an_international_conveyance_Japan',]
data <- na.omit(data)
data$dateRep <- as.character(data$dateRep)
data$countriesAndTerritories <- factor(data$countriesAndTerritories)
US <- data.frame(dateRep = data[data$geoId=='US',]$dateRep)
US$dateRep <- as.character(US$dateRep)

#把数据补全为从12月31号开始每天的数据，并填充缺失的new cases与new death为0
aaa <- data.frame()
for (a in levels(data$countriesAndTerritories)){
  country <- data[data$countriesAndTerritories==a,]
  bb <- US%>%
    left_join(country, by = 'dateRep')
  for (i in c(1:nrow(bb))) {
    if (is.na(bb$countriesAndTerritories[i])==TRUE) 
      bb$countriesAndTerritories[i] <- as.character(a)
    if (is.na(bb$geoId[i])==TRUE)
      bb$geoId[i] <- as.character(country$geoId[which(is.na(country$geoId)==FALSE)][1])
    if (is.na(bb$popData2019[i]==TRUE))
      bb$popData2019[i] <- as.numeric(as.character(country$popData2019[which(is.na(country$popData2019)==FALSE)][1]))
    
  }
  aaa <- rbind(aaa,bb)
}

data <- aaa%>%
  dplyr::group_by(countriesAndTerritories)%>%
  dplyr::mutate(cases = tidyr::replace_na(cases, 0),
                deaths = tidyr::replace_na(deaths, 0))

processed_data <- data%>%
  dplyr::select(dateRep, countriesAndTerritories, geoId, popData2019, cases, deaths)%>%
  dplyr::group_by(countriesAndTerritories)%>%
  dplyr::mutate(confirm = cumsum(cases), death = cumsum(deaths))%>%
  dplyr::rename(Date = dateRep, Country = countriesAndTerritories, alpha.2 = geoId, 
                Population = popData2019, New.cases = cases, New.death = deaths)%>%
  dplyr::select(Date, Country, confirm, death, alpha.2, Population, New.cases, New.death)


processed_data$alpha.2 <- as.character(processed_data$alpha.2)
JDate <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/JDate.csv')
JDate <- na.omit(JDate)
JDate$Date <- as.Date(JDate$Date,'%m/%d/%y')
processed_data$Date <- as.Date(processed_data$Date)

merge <- processed_data%>%
  left_join(JDate,by=c('Date'))
