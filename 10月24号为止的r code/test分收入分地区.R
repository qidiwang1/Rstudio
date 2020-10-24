data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
data <- data[!is.na(data$Total_tests),]
data <- data[data$Income!='',]

Test_data <- data.frame()
data$Date <- factor(data$Date)
for (a in levels(data$Date)){
  certain_date <- data[data$Date==a,]
  today <- data.frame(Date = as.character(a),
                      High_income = sum(certain_date$Total_tests[certain_date$Income == 'High income'])/sum(certain_date$Population[certain_date$Income == 'High income'])*1000000,
                      Low_income = sum(certain_date$Total_tests[certain_date$Income == 'Low income'])/sum(certain_date$Population[certain_date$Income == 'Low income'])*1000000,
                      Lower_Middle_income = sum(certain_date$Total_tests[certain_date$Income == 'Lower-Middle income'])/sum(certain_date$Population[certain_date$Income == 'Lower-Middle income'])*1000000,
                      Upper_Middle_income = sum(certain_date$Total_tests[certain_date$Income == 'Upper-Middle income'])/sum(certain_date$Population[certain_date$Income == 'Upper-Middle income'])*1000000)
  Test_data <- rbind(Test_data, today)
}
write_xlsx(Test_data,'/Users/qidiwang1/Desktop/Test_data.xlsx')


data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
data <- data[!is.na(data$Total_tests),]
data <- data[data$Income!='',]
data$sub.region <- data$Sub_region
data$Date <- factor(data$Date)
Region_data <- data.frame()
for (a in levels(data$Date)){
  certain_date <- data[data$Date==a,]
  Eastern_Asia <- sum(certain_date[certain_date$sub.region=="Eastern Asia",]$Total_tests)/sum(certain_date[certain_date$sub.region=="Eastern Asia",]$Population)*1000000
  MENA <- sum(certain_date[certain_date$sub.region=="Northern Africa"|certain_date$sub.region=="Western Asia",]$Total_tests)/sum(certain_date[certain_date$sub.region=="Northern Africa"|certain_date$sub.region=="Western Asia",]$Population)*1000000
  Western_Europe <- sum(certain_date[certain_date$sub.region=="Western Europe"|certain_date$sub.region=="Southern Europe"|certain_date$sub.region=="Northern Europe",]$Total_tests)/sum(certain_date[certain_date$sub.region=="Western Europe"|certain_date$sub.region=="Southern Europe"|certain_date$sub.region=="Northern Europe",]$Population)*1000000
  Oceania <- sum(certain_date[certain_date$sub.region=="Australia and New Zealand"|certain_date$sub.region=="Melanesia"|certain_date$sub.region=="Micronesia",]$Total_tests)/sum(certain_date[certain_date$sub.region=="Australia and New Zealand"|certain_date$sub.region=="Melanesia"|certain_date$sub.region=="Micronesia",]$Population)*1000000
  North_America <- sum(certain_date[certain_date$sub.region=="Northern America",]$Total_tests)/sum(certain_date[certain_date$sub.region=="Northern America",]$Population)*1000000
  Latin <- sum(certain_date[certain_date$sub.region=="Latin America and the Caribbean",]$Total_tests)/sum(certain_date[certain_date$sub.region=="Latin America and the Caribbean",]$Population)*1000000
  Rest_of_Asia <- sum(certain_date[certain_date$sub.region=="South-eastern Asia"|certain_date$sub.region=="Southern Asia"|certain_date$sub.region=="Central Asia",]$Total_tests)/sum(certain_date[certain_date$sub.region=="South-eastern Asia"|certain_date$sub.region=="Southern Asia"|certain_date$sub.region=="Central Asia",]$Population)*1000000
  Sub_Saharan <- sum(certain_date[certain_date$sub.region=="Sub-Saharan Africa",]$Total_tests)/sum(certain_date[certain_date$sub.region=="Sub-Saharan Africa",]$Population)*1000000
  Eastern_Europe <- sum(certain_date[certain_date$sub.region=="Eastern Europe",]$Total_tests)/sum(certain_date[certain_date$sub.region=="Eastern Europe",]$Population)*1000000
  Region_data <- rbind(Region_data,cbind(Date = as.character(a),Eastern_Asia=Eastern_Asia, MENA=MENA, Western_Europe=Western_Europe, Oceania=Oceania,
                                         North_America=North_America, Latin=Latin, Rest_of_Asia=Rest_of_Asia, Sub_Saharan= Sub_Saharan,
                                         Eastern_Europe=Eastern_Europe))
}

write_xlsx(Region_data,'/Users/qidiwang1/Desktop/Region_data.xlsx')

