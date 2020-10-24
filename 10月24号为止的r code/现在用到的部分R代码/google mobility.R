#读取Google数据，选择国家层面的数据，并计算Raw Google Index
google <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/google.csv')
google <- google[google$sub_region_1=='',]
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
                                alpha.2 = as.character(country$country_region_code[i]), google_index = country$google_index[i],
                                apple_index = country$apple_average[i],google_avg = as.double(NA),apple_avg = as.double(NA)))
        
      } 
    for (i in c(7:nrow(country))){ 
        bbb2 <- rbind(bbb2, cbind(Date = as.character(country$Date[i]), Country = as.character(country$country_region[i]),
                                alpha.2 = as.character(country$country_region_code[i]), google_index = country$google_index[i],
                                apple_index = country$apple_average[i],google_avg = mean(country$google_index[(i-6):i],na.rm=TRUE), 
                                apple_avg = mean(country$apple_average[(i-6):i],na.rm=TRUE)))
      }
  } else {
      print(paste(as.character(a),'cannot be analyzed'))
    }
    index_form <- rbind(index_form,rbind(bbb,bbb2))
}

index_form$alpha.2 <- as.character(index_form$alpha.2)
index_form$alpha.2[index_form$Country=='Albania'] <- 'AL'
index_form$alpha.2[index_form$Country=='Iceland'] <- 'IS'

for (i in c(1:nrow(index_form))){
  if (is.na(index_form$alpha.2[i]))
    index_form$alpha.2[i] <- index_form$alpha.2[i-1]
  if (index_form$google_index[i]=='NaN'|is.na(index_form$google_index[i]))
    index_form$google_avg[i] <- as.double(NA)
}

library(anchors)
index_form <- replace.value(index_form, 'alpha.2', from='GB', to='UK', verbose = FALSE)
index_form <- replace.value(index_form, 'alpha.2', from='GR', to='EL', verbose = FALSE)
merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')

index_form$alpha.2 <- as.character(index_form$alpha.2)
merge$alpha.2 <- as.character(merge$alpha.2)
index_form$Date <- as.Date(index_form$Date)
merge$Date <- as.Date(merge$Date,'%m/%d/%y')
data_all <- index_form%>%
  left_join(merge, by = c('alpha.2','Date'))
data_all$index_dt <- as.character(data_all$Date,'%Y%m%d')
data_all$index_dt <- as.numeric(data_all$index_dt)
data_all$Total.tests[is.na(data_all$Total.tests)] <- 0
data_all <- replace.value(data_all, 'apple_avg', from='NaN', to=NA, verbose = FALSE)
data_all <- replace.value(data_all, 'google_avg', from='NaN', to=NA, verbose = FALSE)

data <- data_all[data_all$Country.x!='Albania'&data_all$Country.x!='Iceland',]
aaa <- data[is.na(data$google_avg)==FALSE & is.na(data$apple_avg)==FALSE,]
aaa$alpha.2 <- factor(aaa$alpha.2)
data <- data[data$alpha.2%in%levels(aaa$alpha.2),]
data$alpha.2 <- factor(data$alpha.2)

library(data.table)
library(mltools)
countries <- one_hot(as.data.table(data$alpha.2))
data <- cbind(data,countries)
#data$alpha.2 <- factor(data$alpha.2)


bbb <- data[is.na(data$google_avg)==FALSE & is.na(data$apple_avg)==FALSE,]

a1 <- bbb[,c('google_avg','Total.tests','New.death','New.cases','alpha.2','sub.region','Daily_Economics_Policy_Score','apple_avg')]
a2 <- bbb[,c(90:ncol(bbb))]
train <- cbind(a1,a2)

train$google_avg <- as.numeric(as.character(train$google_avg))
train$apple_avg <- as.numeric(as.character(train$apple_avg))
train$New.death <- as.numeric(as.character(train$New.death))
train$New.cases <- as.numeric(as.character(train$New.cases))
library(randomForest)
rf <- randomForest(google_avg ~ . - alpha.2,data = train)

data$google_avg <- as.numeric(as.character(data$google_avg))
data$apple_avg <- as.numeric(as.character(data$apple_avg))
data$New.death <- as.numeric(as.character(data$New.death))
data$New.cases <- as.numeric(as.character(data$New.cases))
pred <- predict(rf,newdata = data)
data$pred <- pred
for (i in c(1:nrow(data))){
  if (is.na(data$google_avg[i]))
    data$google_avg[i] <- data$pred[i]
}
#View(data[,c('Date','index_dt','Country.x','alpha.2','alpha.3','google_avg','pred')])
mobility <- data[,c('Date','index_dt','Country.x','alpha.2','alpha.3','google_avg')]
mobility$Country.x <- factor(mobility$Country.x)
rest <- data_all[!(data_all$Country.x%in%mobility$Country.x),c('Date','index_dt','Country.x','alpha.2','alpha.3','google_avg')]

mobility <- rbind(mobility,rest)
mobility$Country.x <- factor(mobility$Country.x)
mobility$google_avg[is.na(mobility$google_avg)==FALSE & mobility$google_avg>1] <- 1

library(writexl)
write_xlsx(mobility,'/Users/qidiwang1/Desktop/mobility.xlsx')





































'''
levels(mobility$Country.x)

View(data[,c('Date','index_dt','Country.x','alpha.2','alpha.3','google_avg','apple_avg','pred')])

index_form$google_avg <- as.numeric(as.character(index_form$google_avg))
index_form$apple_avg <- as.numeric(as.character(index_form$apple_avg))
index_form$apple_index <- as.numeric(as.character(index_form$apple_index))
index_form$google_index <- as.numeric(as.character(index_form$google_index))
baseline_date <- seq((google[nrow(google),]$Date-6),google[nrow(google),]$Date,'days')
index_form$Date <- as.Date(index_form$Date)

View(google)
#计算用Apple数据拟合的Raw Google Index并填入Raw Google Index那一列中
for (i in c(1:nrow(index_form))){
  if (is.na(index_form$google_index[i])==TRUE) {
    country <- index_form[index_form$Country==index_form$Country[i],]
    google_baseline = mean(country[country$Date%in%baseline_date,]$google_avg)
    apple_baseline = mean(country[country$Date%in%baseline_date,]$apple_avg)
    index_form$google_index[i] <- index_form$apple_index[i]/apple_baseline*google_baseline
  } else {index_form$google_index[i] <- index_form$google_index[i]}
   
}

#填入部分NA Value
for (i in c(1:nrow(index_form))){
  if (is.na(index_form$alpha.2[i])==TRUE)
    index_form$alpha.2[i] <-  index_form$alpha.2[i-1]
}

#对Raw Google Index做7天平滑
Mobility_index <- data.frame()
for (a in levels(index_form$Country)){
  country <- index_form[index_form$Country==a,]
  bbb <- data.frame()
  for (i in c(7:nrow(country))){
    bbb <- rbind(bbb, cbind(Date = as.character(country$Date[i]),
                            alpha.2 = as.character(country$alpha.2[i]),
                            Mobility_Index = mean(country$google_index[(i-6):i])))
      
    }
  Mobility_index <- rbind(Mobility_index,bbb)
}

Mobility_index$alpha.2 <- as.character(Mobility_index$alpha.2)

#转换英国的alpha.2 code为 ‘UK’， 否则融合不进去，有些地方用'UK'表示，有些地方用 ‘GB’，这里统一用 ‘UK’
library(anchors)
Mobility_index <- replace.value(Mobility_index, 'alpha.2', from='GB', to='UK', verbose = FALSE)
Mobility_index <- replace.value(Mobility_index, 'alpha.2', from='GR', to='EL', verbose = FALSE)


#add_data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/Mobility_Index.csv')

#add_data <- add_data[,-1]
#add_data$Date <- as.Date(add_data$Date)

#step_1 <- add_data[add_data$Date<=(google[nrow(google),]$Date-6),]
#step_2 <- Mobility_index[Mobility_index$Date>(google[nrow(google),]$Date-6),]
#mobility <- rbind(step_1,step_2)
#mobility <- mobility[order(mobility$alpha.2),]




write.csv(Mobility_index,'/Users/qidiwang1/Desktop/疫情数据库/Mobility_Index.csv')
'''
