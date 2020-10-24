economic <- read_excel('/Users/qidiwang1/Desktop/gdp&mobility (5).xlsx',sheet = 'Sheet7')
stage <- read_excel('/Users/qidiwang1/Desktop/stage.xlsx')
stage$iso3_name <- as.character(stage$iso3_name)

data <- economic%>%
  left_join(stage,by = 'iso3_name')

gdp <- read_excel('/Users/qidiwang1/Desktop/gdp_share.xlsx',sheet = 'Sheet1')
data <- data%>%
  left_join(gdp, by = 'iso3_name')

data <- data[data$iso3_name!='CHN'&data$iso3_name!='TWN'&data$iso3_name!='TZA',]
data <- data[data$index_dt>=20200315,]
recover <- sum(data$`2018GDP`[data$index_dt==20200501&data$Stage=='Stage4'])
trough <- sum(data$`2018GDP`[data$index_dt==20200501&data$Stage!='Stage4'&data$Stage!='Stage1'])
data$gdp_share <- as.double(NA)

data$gdp_share[data$Stage=='Stage4'] <- data$`2018GDP`[data$Stage=='Stage4']/recover
data$gdp_share[data$Stage!='Stage4'&data$Stage!='Stage1'] <- data$`2018GDP`[data$Stage!='Stage4'&data$Stage!='Stage1']/trough
data$eco_after <- data$gdp_relative*data$gdp_share
data$Date <- as.Date(data$index_dt,'%Y%m%d')
economic_data <- data.frame()
for (a in levels(factor(data$Date))){
  certain_date <- data[data$Date==a,]
  recover <- certain_date[certain_date$Stage=='Stage4',]
  trough <- certain_date[certain_date$Stage!='Stage4'&certain_date$Stage!='Stage1',]
  recover_data <- sum(recover$eco_after)
  trough_data <- sum(trough$eco_after)
  economic_data <- rbind(economic_data,data.frame(Date = as.character(a),
                                                  recover = recover_data,
                                                  trough = trough_data))
}
write_xlsx(economic_data,'/Users/qidiwang1/Desktop/stage_economic_data.xlsx')
