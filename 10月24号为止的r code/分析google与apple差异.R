library(dplyr)
#读取Google数据，选择国家层面的数据，并计算Raw Google Index
google <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/google.csv')
google <- google[google$sub_region_1=='&'google$metro_area=='',]
google <- google[google$country_region!='Namibia',]
google$country_region <- factor(google$country_region)
google$index_1 <- rowMeans(google[,c('grocery_and_pharmacy_percent_change_from_baseline','retail_and_recreation_percent_change_from_baseline')],na.rm = TRUE)/100+1
google$index_2 <- (100+google$workplaces_percent_change_from_baseline)/100
google$google_index <- rowMeans(google[,c('index_1','index_2')],na.rm = TRUE)
google <- google[google$country_region!='Guinea-Bissau'&google$country_region!='Liechtenstein'&google$country_region!='Réunion',]

#读取Apple数据，清理Apple数据为标准化表格
apple <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/apple.csv')

apple <- apple[apple$geo_type=='country/region',]
apple$region <- factor(apple$region)
apple_index <- data.frame()
for (a in levels(apple$region)){
  country <- apple[apple$region==a,]
  
  driving = data.frame(t(country[country$transportation_type=='driving',][,c(7:ncol(country))]))
  if (ncol(driving)==0){
    driving = data.frame(driving = rep(NA,(ncol(country)-6)))
  } else {driving = data.frame(driving = driving[,1])}
  
  
  walking = data.frame(t(country[country$transportation_type=='walking',][,c(7:ncol(country))]))
  if (ncol(walking)==0){
    walking = data.frame(walking = rep(NA,(ncol(country)-6)))
  } else {walking = data.frame(walking = data.frame(walking[,1]))}
  
  transit = data.frame(t(country[country$transportation_type=='transit',][,c(7:ncol(country))]))
  if (ncol(transit)==0){
    transit = data.frame(transit = rep(NA,(ncol(country)-6)))
  } else {transit = data.frame(transit = data.frame(transit = transit[,1]))}
  
  
  Date = seq(as.Date('2020-01-13'),(as.Date('2020-01-13')+ncol(apple)-7),'days')
  
  apple_data <- data.frame(Date = Date, country_region = as.character(a), driving = driving, walking = walking, transit = transit)
  apple_index <- rbind(apple_index,apple_data)
  
}

#计算Raw Apple Index
apple_index$apple_average <- rowMeans(apple_index[,c(4:5)],na.rm = TRUE)/100
google$Date <- as.Date(google$date, '%m/%d/%y')
apple_index$country_region <- as.character(apple_index$country_region)
google$country_region <- as.character(google$country_region)

#Merge Apple与Google数据，这时候有Apple数据的国家日期会比有Google数据的国家多几天
mobility <- merge(x = google, y = apple_index, by = c('country_region', 'Date'), all = TRUE)
mobility$country_region <- factor(mobility$country_region)
mobility <- mobility[mobility$Date%in%seq(as.Date(google$date[1],'%m/%d/%y'),(as.Date('2020-01-13')+ncol(apple)-5),'days'),]
mobility$google_index[mobility$country_region=='Albania'] <- mobility$apple_average[mobility$country_region=='Albania']
mobility$google_index[mobility$country_region=='Iceland'] <- mobility$apple_average[mobility$country_region=='Iceland']

#计算Raw Google Index与Raw Apple Index的7天平滑数据
index_form <- data.frame()
for (a in levels(mobility$country_region)){
  country <- mobility[mobility$country_region==a,]
  if (sum(is.na(country$google_index)==FALSE)>10){
    bbb <- data.frame()
    bbb2 <- data.frame()
    for (i in c(1:6)){
      bbb <- rbind(bbb, cbind(Date = as.character(country$Date[i]), Country = as.character(country$country_region[i]),
                              iso2_name = as.character(country$country_region_code[i]), google_index = country$google_index[i],
                              apple_index = country$apple_average[i],google_avg = as.double(NA),apple_avg = as.double(NA),google_grocery = as.double(NA),
                              google_retail = as.double(NA),google_workplace = as.double(NA),google_residential = as.double(NA),
                              apple_walking = as.double(NA),apple_driving = as.double(NA),apple_transit = as.double(NA)))
      
    } 
    for (i in c(7:nrow(country))){ 
      bbb2 <- rbind(bbb2, cbind(Date = as.character(country$Date[i]), Country = as.character(country$country_region[i]),
                                iso2_name = as.character(country$country_region_code[i]), google_index = country$google_index[i],
                                apple_index = country$apple_average[i],google_avg = mean(country$google_index[(i-6):i],na.rm=TRUE), 
                                apple_avg = mean(country$apple_average[(i-6):i],na.rm=TRUE),google_grocery = mean(country$grocery_and_pharmacy_percent_change_from_baseline[(i-6):i],na.rm=TRUE),
                                google_retail = mean(country$retail_and_recreation_percent_change_from_baseline[(i-6):i],na.rm=TRUE),google_workplace = mean(country$workplaces_percent_change_from_baseline[(i-6):i],na.rm=TRUE),
                                google_residential = mean(country$residential_percent_change_from_baseline[(i-6):i],na.rm=TRUE),
                                apple_walking = mean(country$walking...1.[(i-6):i],na.rm=TRUE),apple_driving = mean(country$driving[(i-6):i],na.rm=TRUE),apple_transit = mean(country$transit[(i-6):i],na.rm=TRUE)))
    }
  } else {
    print(paste(as.character(a),'cannot be analyzed'))
  }
  index_form <- rbind(index_form,rbind(bbb,bbb2))
}

index_form$iso2_name <- as.character(index_form$iso2_name)
index_form$iso2_name[index_form$Country=='Albania'] <- 'AL'
index_form$iso2_name[index_form$Country=='Iceland'] <- 'IS'

for (i in c(1:nrow(index_form))){
  if (is.na(index_form$iso2_name[i]))
    index_form$iso2_name[i] <- index_form$iso2_name[i-1]
  if (index_form$google_index[i]=='NaN'|is.na(index_form$google_index[i]))
    index_form$google_avg[i] <- as.double(NA)
}

library(anchors)
index_form <- replace.value(index_form, 'iso2_name', from='GB', to='UK', verbose = FALSE)
index_form <- replace.value(index_form, 'iso2_name', from='GR', to='EL', verbose = FALSE)
index_form <- replace.value(index_form, 'apple_avg', from='NaN', to=NA, verbose = FALSE)
index_form$index_dt <- as.numeric(as.character(as.Date(index_form$Date),'%Y%m%d'))

merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')

merge$iso2_name <- as.character(merge$iso2_name)
index_form$iso2_name <- as.character(index_form$iso2_name)

data <- index_form%>%
  left_join(merge,by = c('iso2_name','index_dt'))
data$Total_tests <- tidyr::replace_na(data$Total_tests, 0)
for (i in c(4:13)){
  data[,i] <- as.numeric(as.character(data[,i]))
}
US <- data[data$iso2_name=='US',]
library(ggplot2)
data$Country.x <- factor(data$Country.x)
View(data)
for (a in levels(data$Country.x)) {
  country <- data[data$Country.x==a,]
  p <- ggplot()+
    geom_line(aes(x=country$index_dt,y=(100+country$google_grocery)/100,color = 'grocery'))+
    geom_line(aes(x=country$index_dt,y=(100+country$google_retail)/100,color = 'retail'))+
    geom_line(aes(x=country$index_dt,y=(100+country$google_workplace)/100,color = 'workplace'))+
    geom_line(aes(x=country$index_dt,y=(100+country$google_residential)/100,color = 'residential'))+
    geom_line(aes(x=country$index_dt,y=country$google_avg,color = 'google_avg'))+
    geom_line(aes(x=country$index_dt,y=country$apple_avg,color = 'apple_avg'))+
    ggtitle(as.character(a))
  ggsave(p, filename = paste('/Users/qidiwang1/Desktop/g&a/',as.character(a),'.png'))
}

for (a in levels(data$Country.x)) {
  country <- data[data$Country.x==a,]
  p <- ggplot()+
    geom_line(aes(x=country$index_dt,y=(100+country$google_workplace)/(100+country$google_grocery)/100,color = 'work/grocery'))+
    geom_line(aes(x=country$index_dt,y=(100+country$google_workplace)/(100+country$google_retail)/100,color = 'work/retail'))+
    ggtitle(as.character(a))
  ggsave(p, filename = paste('/Users/qidiwang1/Desktop/google_work_consumption/',as.character(a),'.png'))
}



ggplot()+
  geom_line(aes(x=US$index_dt,y=(100+US$google_grocery)/100,color = 'grocery'))+
  geom_line(aes(x=US$index_dt,y=(100+US$google_retail)/100,color = 'retail'))+
  geom_line(aes(x=US$index_dt,y=(100+US$google_workplace)/100,color = 'workplace'))+
  geom_line(aes(x=US$index_dt,y=US$google_avg,color = 'google_avg'))+
  geom_line(aes(x=US$index_dt,y=US$apple_avg,color = 'apple_avg'))


library(data.table)
library(mltools)
countries <- one_hot(as.data.table(data$Country.x))
data <- cbind(data,countries)
data <- data[,c(2,6,11,19,20,22,26,34,38,39:ncol(data))]
data <- data[data$Country.x!='United_Kingdom'&data$Country.x!='Greece',]
train <- data[data$index_dt<=20200620,]
test <- data[data$index_dt>20200620,]
data$Country.x <- factor(data$Country.x)

#data$Country.x
library(randomForest)
rf <- randomForest(google_avg*100 ~ . - Country.x-index_dt,data = train)

pred <- predict(rf, newdata = test)

test$pred <- pred
test$Country.x <- factor(test$Country.x)
library(ggplot2)
for (a in levels(test$Country.x)){
  country <- test[test$Country.x==a,]
  p <- ggplot()+
    geom_line(aes(x=country$index_dt,y=country$google_avg*100,color='google'))+
    geom_line(aes(x=country$index_dt,y=country$apple_all,color='apple_w_t'))+
    geom_line(aes(x=country$index_dt,y=country$pred, color = 'pred'))+
    xlab('Date')+
    ylab('Index')+
    ggtitle(as.character(a))
  ggsave(p, filename = paste('/Users/qidiwang1/Desktop/test4/',as.character(a),'.png'))
}



library(modelr)
data.frame(
  R2 = rsquare(rf, data = test),
  RMSE = rmse(rf, data = test),
  MAE = mae(rf, data = test)
)

data <- data[,c(2,6,7,8,9,10,11)]
names(data)
for (a in levels(data$Country.x)){
  country <- data[data$Country.x==a,]
  p <- ggplot()+
    geom_line(aes(x=country$index_dt,y=country$google_avg*100,color='google'))+
    geom_line(aes(x=country$index_dt,y=country$apple_walking,color='apple walking'))+
    geom_line(aes(x=country$index_dt,y=country$apple_driving,color='apple driving'))+
    geom_line(aes(x=country$index_dt,y=country$apple_transit,color='apple transit'))+
    xlab('Date')+
    ylab('Index')+
    ggtitle(as.character(a))
  ggsave(p, filename = paste('/Users/qidiwang1/Desktop/test4/',as.character(a),'.png'))
}

data$Country.x <- factor(data$Country.x)

View(data)

summary(lm(google_7d~index_dt + apple_7d + Total_death + Total_recover + Total_cases + Total_tests +
             New_death + New_recover + New_case + smooth_confirm_doubling_days + Daily_Economics_Policy_Score +
             Covid_Wave + Population + Development + Income + Sub_region,data = train))
summary(lm(google_7d~index_dt + apple_7d + Total_death + Total_cases + Total_tests +
             New_death + New_recover + New_case + smooth_confirm_doubling_days + Daily_Economics_Policy_Score +
             Covid_Wave + Population + Development + Income + Sub_region,data = train))
summary(lm(google_7d~index_dt + apple_7d + Total_cases + Total_tests +
             New_death + New_case + smooth_confirm_doubling_days + Daily_Economics_Policy_Score +
             Covid_Wave + Population + Development + Income + Sub_region,data = train))
summary(lm(google_avg~index_dt + apple_all + Total_tests +
             New_death + New_case +  Daily_Economics_Policy_Score +
             Covid_Wave + Population + Development + Income + Sub_region,data = train))

summary(lm(google_avg*100 ~ .-Country.x,data = train))








randomForest()