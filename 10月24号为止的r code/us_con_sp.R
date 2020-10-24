employment <- read.csv('https://raw.githubusercontent.com/Opportunitylab/EconomicTracker/master/data/Low%20Inc%20Emp%20All%20Businesses%20-%20State%20-%20Daily.csv')
con_sp <- read.csv('https://raw.githubusercontent.com/Opportunitylab/EconomicTracker/master/data/Affinity%20-%20State%20-%20Daily.csv')
lowinv_rev <- read.csv('https://raw.githubusercontent.com/Opportunitylab/EconomicTracker/master/data/Womply%20Revenue%20-%20State%20-%20Daily.csv')
geo <- read.csv('https://raw.githubusercontent.com/Opportunitylab/EconomicTracker/master/data/GeoIDs%20-%20State.csv')
#confirm <- read.csv('/Users/qidiwang1/Desktop/us confirm.csv')

data$area_name <-as.character(data$area_name)
data$index_dt <- as.numeric(as.character(data$index_dt))

#Consumer Spending
con_sp <- con_sp%>%
  left_join(geo, by = 'statefips')
con_sp$date <- paste(con_sp$year,con_sp$month,con_sp$day,sep = "/")
con_sp$index_dt <- as.numeric(as.character(as.Date(con_sp$date,'%Y/%m/%d'),'%Y%m%d'))
names(con_sp)[15] <- 'area_name'
con_sp$area_name <- as.character(con_sp$area_name)

data2 <- data%>%
  left_join(con_sp, by = c('index_dt', 'area_name'))

#Employment
employment <- employment%>%
  left_join(geo, by = 'statefips')

employment$date <- paste(employment$year,employment$month,employment$day,sep = "/")
employment$index_dt <- as.numeric(as.character(as.Date(employment$date,'%Y/%m/%d'),'%Y%m%d'))
names(employment)[14] <- 'area_name'
employment$area_name <- as.character(employment$area_name)
data3 <- data2%>%
  left_join(employment, by = c('index_dt', 'area_name'))

#Small Business Revenue
lowinv_rev <- lowinv_rev%>%
  left_join(geo, by = 'statefips')
lowinv_rev$date <- paste(lowinv_rev$year,lowinv_rev$month,lowinv_rev$day,sep = "/")
lowinv_rev$index_dt <- as.numeric(as.character(as.Date(lowinv_rev$date,'%Y/%m/%d'),'%Y%m%d'))
names(lowinv_rev)[13] <- 'area_name'
lowinv_rev$area_name <- as.character(lowinv_rev$area_name)
data4 <- data3%>%
  left_join(lowinv_rev, by = c('index_dt', 'area_name'))

write_xlsx(data4,'/Users/qidiwang1/Desktop/us_data.xlsx')




data2$area_name <- factor(data2$area_name)
library(ggplot2)
for (a in levels(data2$area_name)){
  country <- data2[data2$area_name==a,]
  p <- ggplot()+
    geom_path(aes(x=country$smooth_confirm_doubling_days,y=country$spend_all))+
    xlab('Doubling days')+
    ylab('Consumer Spending')+
    ggtitle(as.character(a))
  ggsave(p,filename = paste('/Users/qidiwang1/Desktop/con_sp/',as.character(a),'.png'))
}

write_xlsx(data2,'/Users/qidiwang1/Desktop/con_sp.xlsx')

