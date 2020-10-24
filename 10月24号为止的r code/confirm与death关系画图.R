merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
merge$index_dt <- as.numeric(as.character(as.Date(merge$Date),'%Y%m%d'))

data <- data.frame()
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  lasting <- country$JDATE[country$C7_Restrictions.on.internal.movement==2][1]-country$JDATE[country$confirm>0][1]
  country$lasting <- lasting
  data <- rbind(data,country)
}

data <- data[data$index_dt==20200820,]

data$fatality <- data$death/data$confirm
#ggplot()+
#  geom_point(aes(x=log(data$Total_cases,10),y=data$fatality))+
#  ylim(0,0.1)
data$log_cases <- log(data$confirm,10)
data$log_death <- log(data$death,10)
#data$plot <-0
#data$plot[c(100:187)] <- 7
#data$per_025 <- data$plot-log(400,10)
#data$per_05 <- data$plot-log(200,10)
#data$per_40 <- data$plot-log(2.5,10)
#data$per_20 <- data$plot-log(5,10)
#data$per_1 <- data$plot-log(100,10)
#data$per_2 <- data$plot-log(50,10)
#data$per_5 <- data$plot-log(20,10)
#data$per_10 <- data$plot-log(10,10)
#write.csv(data,'/Users/qidiwang1/Desktop/fatality_rate.csv')
#data <- data[data$log_death!='-Inf',]
life <- read.csv('/Users/qidiwang1/Desktop/slum/life-expectancy.csv')
life <- life[life$Year==2019,]
life$iso3_name <- as.character(life$Code)
data$iso3_name <- as.character(data$alpha.3)
data <- data%>%
  left_join(life,by='iso3_name')
gdp <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp_world.csv')
data$iso3_name <- as.character(data$iso3_name) 
gdp$iso3_name <- as.character(gdp$iso3_name)

eco <- data%>%
  left_join(gdp,by = 'iso3_name')

aging_ratio <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/aging_ratio.csv')
aging_ratio$iso3_name <- as.character(aging_ratio$Country.Code)
data <- eco%>%
  left_join(aging_ratio,by='iso3_name')

write_xlsx(data,'/Users/qidiwang1/Desktop/data_exploration.xlsx')



ggplot()+
  geom_point(aes(x=data$aging_ratio,y=log(data$fatality,10),color = data$region))
  

geom_text(aes(x=data$aging_ratio,y=data$fatality,label = data$Country))



linear <- lm(log_death~log_cases,data=data)
summary(linear)

linear2 <- lm(Total_death~Total_cases,data=data)
summary(linear2)
pred <- predict(linear)
data$pred <- pred

levels(data$sub.region)
data <- data[data$region=='Asia'|data$region=='Europe',]
ggplot()+
  geom_point(aes(x=data$log_cases,y=data$log_death,color = data$region,size = data$aging_ratio))+
  #geom_text(aes(x=data$log_cases,y=data$log_death,label = data$Country))+
  


  geom_line(aes(x=data$plot,y=data$per_05))+

  geom_line(aes(x=data$plot,y=data$per_2))+

  geom_line(aes(x=data$plot,y=data$per_10))
  
write.csv(data,'/Users/qidiwang1/Desktop/fatality_rate.csv')

ggplot()+
  geom_point(aes(x=data$log_cases,y=data$fatality,color = data$region))
  geom_text(aes(x=data$aging_ratio,y=data$fatality,color = data$region,label=data$Country))+


  ylim(0,0.15)
  xlim(0,500000)+
  ylim(0,0.15)
  geom_line(aes(x=data$log_cases,y=data$pred))
  #geom_text(aes(x=data$log_cases,y=data$log_death,label = data$Country))

write.csv(data,'/Users/qidiwang1/Desktop/fatality_rate.csv')


cor(data$Total_death,data$Total_cases)
gdp <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp_world.csv')
data$iso3_name <- as.character(data$iso3_name) 
gdp$iso3_name <- as.character(gdp$iso3_name)

eco <- gdp%>%
  left_join(data,by = 'iso3_name')
ggplot()+
  geom_point(aes(x=log(eco$X2019/eco$Population,10),y=log(eco$Total_tests/eco$Population*1000,10),color = eco$Region))+
  geom_text(aes(x=log(eco$X2019/eco$Population,10),y=log(eco$Total_tests/eco$Population*1000,10),label = eco$Country))
