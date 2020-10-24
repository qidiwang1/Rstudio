merge <- read_excel('/Users/qidiwang1/Desktop/PET_DATA_20201019.xlsx')
merge <- merge[merge$index_dt>=20200215&merge$index_dt<=20201016,]
gdp <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp$relative.csv')
merge <- merge[merge$iso3_name%in%levels(factor(gdp$iso3_name)),]
merge$gdp_relative[merge$gdp_relative>0.9886] <- 0.9886

data <- data.frame()
for (a in levels(factor(merge$Country))){
  country <- merge[merge$Country==a,]
  for (i in c(1:nrow(country))){
    if (is.na(country$gdp_relative[i]))
      if (i==1){
        country$gdp_relative[i] <- 0.9886
      } else{
        country$gdp_relative[i] <- country$gdp_relative[i-1]
      }
  }
  data <- rbind(data,country)
}
data$Fatality_rate[is.na(data$Fatality_rate)] <- 0
data$Recovery_rate[is.na(data$Recovery_rate)] <- 0
data$death_ratio[is.na(data$death_ratio)] <- 0
data <- data%>%
  group_by(iso3_name)%>%
  mutate(newcase_7d=rollmean(New_case,7,fill=NA,align='right'),
         newdeath_7d=rollmean(New_death,7,fill=NA,align='right'),
         newrecover_7d=rollmean(New_recover,7,fill=NA,align='right'))

analysis_data <- data[,c(3,25,5:7,12:14,19,62,63,65:66,73:75)]



cluster_result <- data.frame(iso3_name = levels(factor(analysis_data$iso3_name)))



for (i in c(3:ncol(analysis_data))){
  cluster_data <- analysis_data[,c(1,2,i)]
  long_table <- cluster_data%>%
                      dcast(index_dt~iso3_name)
  long_table <- na.omit(long_table)
  long_table <- t(long_table)
  long_table <- long_table[-1,]
  aaa <- KMedoids(long_table, k=5, "euclidean")
  bbb <- data.frame(iso3_name = rownames(long_table),
                    value = aaa)
  names(bbb)[2]<-names(cluster_data)[3]
  cluster_result <- cluster_result%>%
    left_join(bbb,by='iso3_name')
}

test_data <- data[,c(3,25,15,8)]
test_data <- na.omit(test_data)

for (i in c(3:ncol(test_data))){
  cluster_data <- test_data[,c(1,2,i)]
  long_table <- cluster_data%>%
    dcast(index_dt~iso3_name)
  long_table <- na.omit(long_table)
  long_table <- t(long_table)
  long_table <- long_table[-1,]
  aaa <- KMedoids(long_table, k=5, "euclidean")
  bbb <- data.frame(iso3_name = rownames(long_table),
                    value = aaa)
  names(bbb)[2]<-names(cluster_data)[3]
  cluster_result <- cluster_result%>%
    left_join(bbb,by='iso3_name')
}
write_xlsx(cluster_result,'/Users/qidiwang1/Desktop/time_series_cluster_reslut.xlsx')













