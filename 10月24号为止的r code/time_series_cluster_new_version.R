require(reshape2)
require(dplyr)
require(cluster)
require(writexl)
require(readxl)
require(zoo)
merge <- read_excel('/Users/qidiwang1/Desktop/PET_DATA_20201019.xlsx')
gdp <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp$relative.csv')
ICL <- read.csv('/Users/qidiwang1/Desktop/ICL.csv')

#将大表里的数据限制为有mobility数据的国家
merge <- merge[merge$iso3_name%in%levels(factor(gdp$iso3_name)),]

#将日期限制为有mobility数据及ICL数据的日期
merge <- merge[merge$index_dt>=20200215&merge$index_dt<=20201012,]

#将mobility大于1的数据调为1
merge$gdp_relative[merge$gdp_relative>0.9886] <- 0.9886


#补economic activity数据，原则为刚开始为NA则补0.9886，后来有数据则补为上一个数据的值
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

#将几个ratio的NA值补为0，因为这种情况发生在刚开始疫情没有到来的情况
data$Fatality_rate[is.na(data$Fatality_rate)] <- 0
data$Recovery_rate[is.na(data$Recovery_rate)] <- 0
data$death_ratio[is.na(data$death_ratio)] <- 0

#处理ICL数据，删除没有ICL数据的行数
ICL <- ICL[!is.na(ICL$ICL),]
ICL <- ICL[ICL$iso3_name%in%levels(factor(gdp$iso3_name)),]

#算出ICL预测的total cases
ICL <- ICL%>%
  group_by(Entity)%>%
  mutate(ICL_total=cumsum(ICL))

#将ICL数据与官方数据限制为大于100例
ICL <- ICL[ICL$ICL_total>=100,]
data <- data[data$Total_cases>=100,]

#调整时间格式方便merge
data$Date <- as.Date(data$Date)
ICL$Date <- as.Date(ICL$Date,'%m/%d/%y')
data <- data%>%
  left_join(ICL,by=c('iso3_name','Date'))


#算出每个国家大于100例到10月12号的天数
filter_100 <- data%>%
  group_by(Country)%>%
  mutate(days = length(Total_cases))

#筛选出天数大于180的国家
filter_100 <- filter_100[filter_100$days>=180,]

#这里有一个bug，不得不这么写，这一步是把每一个国家超过180天的部分删除
data <- filter_100[filter_100$Country=='Vietnam',]
data <- data[c(1:180),]
for (a in levels(factor(filter_100$Country))){
  country <- filter_100[filter_100$Country==a,]
  country <- country[c(1:180),]
  data <- rbind(data,country)
}
data <- data[-c(1:180),]




#算出7天平滑的新增数据
data <- data%>%
  group_by(iso3_name)%>%
  mutate(newcase_7d=rollmean(New_case,7,fill=NA,align='right'),
         newdeath_7d=rollmean(New_death,7,fill=NA,align='right'),
         newrecover_7d=rollmean(New_recover,7,fill=NA,align='right'))

#选出第一个需要做cluster的分区，原则为这个分区的数没有缺失数据的国家。
#比如test数据有些国家没有，所以test的数据不再这个分区里
analysis_data <- data[,c(3,25,5:7,12:14,19,62,63,65:66,77:79)]



#算出第一个分区的cluster并融合为一张表格
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

#选出第二个有缺失数据的分区，包括icl数据和test数据
test_data <- data[,c(3,25,15,8,74,75)]
test_data <- na.omit(test_data)

#算出第二个分区的cluster并融合为一张表格
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
















