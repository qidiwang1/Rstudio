economic <- read_excel('/Users/qidiwang1/Desktop/gdp&mobility (5).xlsx',sheet = 'Sheet7')
merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
merge$iso3_name <- as.character(merge$alpha.3)
merge$index_dt <- as.numeric(as.character(as.Date(merge$Date),'%Y%m%d'))
economic$index_dt <- as.numeric(economic$index_dt)
data <- economic%>%
  left_join(merge[,c('iso3_name','IMF.A.E','index_dt','Date')],by = c('iso3_name','index_dt'))

gdp <- read_excel('/Users/qidiwang1/Desktop/gdp_share.xlsx',sheet = 'Sheet1')
data <- data%>%
  left_join(gdp, by = 'iso3_name')

data <- data[data$iso3_name!='CHN'&data$iso3_name!='TWN'&data$iso3_name!='TZA',]
data <- data[data$index_dt>=20200315,]
advanced_gdp <- sum(data$`2018GDP`[data$index_dt==20200501&data$IMF.A.E=='Advanced'])
emerging <- sum(data$`2018GDP`[data$index_dt==20200501&data$IMF.A.E!='Advanced'])
data$gdp_share <- as.double(NA)

data$gdp_share[data$IMF.A.E=='Advanced'] <- data$`2018GDP`[data$IMF.A.E=='Advanced']/advanced_gdp
data$gdp_share[data$IMF.A.E!='Advanced'] <- data$`2018GDP`[data$IMF.A.E!='Advanced']/emerging
data$eco_after <- data$gdp_relative*data$gdp_share

economic_data <- data.frame()
for (a in levels(factor(data$Date))){
  certain_date <- data[data$Date==a,]
  advanced <- certain_date[certain_date$IMF.A.E=='Advanced',]
  emerging <- certain_date[certain_date$IMF.A.E!='Advanced',]
  advanced_data <- sum(advanced$eco_after)
  emerging_data <- sum(emerging$eco_after)
  economic_data <- rbind(economic_data,data.frame(Date = as.character(a),
             advanced = advanced_data,
             others = emerging_data))
}
write_xlsx(economic_data,'/Users/qidiwang1/Desktop/economic_data.xlsx')
