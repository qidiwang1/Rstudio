  library(dplyr)
  #读取欧盟数据并删除中国，珍珠号与Kosovo的数据
  data <- read.csv('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv') 

  #library(readxl)
  #data <- read_excel('/Users/qidiwang1/Desktop/疫情数据库/cov.xlsx')
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
  
  data2 <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/Country List.csv', stringsAsFactors = FALSE)

  data2$alpha.2 <- as.character(data2$alpha.2)
  merge <- processed_data %>%
    left_join(data2,by='alpha.2')
  


  merge <- data.frame(merge)

  china <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/中国疫情每日更新.csv', stringsAsFactors = FALSE)
  #china$confirm <- as.character(china$confirm)
  #china$death <- as.character(china$death)
  #china$Population <- as.character(china$Population)
  #china$New.cases <- as.character(china$New.cases)
  #china$New.death <- as.character(china$New.death)
  #china$Date <- as.Date(china$Date,'%d/%m/%Y')
#processed_data$Date <- as.Date(processed_data$Date)
#china$Date <- as.Date(china$Date,'%d/%m/%Y')
  processed_data <- rbind(merge,china)
  processed_data <- processed_data[!is.na(processed_data$Date),]
#processed_data <- processed_data[!is.na(processed_data$Date),]

  processed_data$Date <- as.Date(processed_data$Date,'%d/%m/%Y')
  processed_data <- processed_data[!is.na(processed_data$Date),]
  john <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
  
  new_zealand <- john[john$Country.Region=='New Zealand',c(5:ncol(john))]
  nzl_data <- data.frame(Date = seq(as.Date('2020-01-22'),as.Date('2020-01-22')+ncol(new_zealand)-1,'days'),
                         confirm = colSums(new_zealand))

  nzl_fill <- data.frame(Date = seq(max(processed_data$Date)-(max(processed_data$Date)-max(nzl_data$Date)),max(processed_data$Date),'days'),
             confirm = nzl_data$confirm[nzl_data$Date== max(nzl_data$Date)])
  if (max(nzl_data$Date)==max(processed_data$Date)){
    nzl_data <- rbind(nzl_data,nzl_fill)
  } else {
    nzl_data <- nzl_data
  }

  nzl_data$cases <- c(nzl_data$confirm[1],diff(nzl_data$confirm))
  processed_data$confirm[processed_data$Country=='New_Zealand'&processed_data$Date>=as.Date('2020-01-22')] <- nzl_data$confirm
  processed_data$New.cases[processed_data$Country=='New_Zealand'&processed_data$Date>=as.Date('2020-01-22')] <- nzl_data$cases

  #new_case <- c()

  #for (i in c(1:length(new_zealand)){
  #  if (i == 1){
  #    new_case <- c(new_case,new_zealand[1])
  #  } else {
  #    new_case <- c(new_case,(new_zealand[i]-new_zealand[i-1]))
  #  }
  #}

  #processed_data$confirm[processed_data$Country=='New_Zealand' & processed_data$Date %in% seq(as.Date('2020-01-22'),as.Date('2020-07-12'),by='days')] <- c(colSums(new_zealand[5:ncol(new_zealand)]),1544)
  #View(processed_data)
  
  
  
  #读取牛津大学政策数据
  policy <- read.csv('https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv')
  #policy <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/policy.csv')

  policy <- policy[policy$RegionName==''&policy$RegionCode=='',]
  policy$Date <- as.character(policy$Date)
  policy$Date <- as.Date(policy$Date,'%Y%m%d')
  
  processed_data$alpha.3 <- as.character(processed_data$alpha.3)
  policy$alpha.3 <- as.character(policy$CountryCode)
  
  merge <- processed_data %>%
    left_join(policy,by=c('alpha.3','Date'))
  
  merge$Country <- factor(merge$Country)
  merge$Country <- as.character(merge$Country)
  merge$Country <- factor(merge$Country)
  
  data <- data.frame()
  

  #填充缺失的policy数据
  for (a in levels(merge$Country)){
    country <- merge[merge$Country==a,]
  
  
    for (i in c(2:nrow(country))){
      for (j in c("C1_School.closing","C1_Flag","C2_Workplace.closing","C2_Flag","C3_Cancel.public.events" ,
                  "C3_Flag","C4_Restrictions.on.gatherings","C4_Flag","C5_Close.public.transport","C5_Flag",
                  "C6_Stay.at.home.requirements","C6_Flag","C7_Restrictions.on.internal.movement","C7_Flag",
                  "C8_International.travel.controls")){
        if (is.na(country[1,j])==TRUE)
          country[1,j] <- 0
        if (is.na(country[i,j])==TRUE)
          country[i,j] <- country[(i-1),j]
      }
    }
    data <- rbind(data,country)
  }

  #请将recover数据处理一下，处理为recover模板里的样式
  #添加Hubei，Hong Kong，China_Exhubei
  #China的total recover请排除Hong Kong和Macau的数据
  #计算国家层面的数据并删除多余的省级层面的数据
  #确保名字与recover模板里的一样来merge表格
library(anchors)
download_data <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv')
#download_data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/recover.csv')
download_data <- download_data[,-c(3,4)]
download_data$Country.Region <- as.character(download_data$Country.Region)
download_data$Country.Region = gsub(' ','_',download_data$Country.Region)
download_data <- download_data[download_data$Country.Region!='Diamond_Princess',]
download_data <- download_data[download_data$Province.State!='Macau',]
France <- download_data[download_data$Country.Region=='France'&download_data$Province.State=='',]
Denmark <- download_data[download_data$Country.Region=='Denmark'&download_data$Province.State=='',]
UK <- download_data[download_data$Country.Region=='United_Kingdom'&download_data$Province.State=='',]
Netherlands <- download_data[download_data$Country.Region=='Netherlands'&download_data$Province.State=='',]
HK <- download_data[download_data$Province.State=='Hong Kong',]
HK <- replace.value(HK, 'Country.Region', from='China', to='Hong Kong', verbose = FALSE)
Hubei <- download_data[download_data$Province.State=='Hubei',]
Hubei <- replace.value(Hubei, 'Country.Region', from='China', to='Hubei', verbose = FALSE)
Australia <- colSums(download_data[download_data$Country.Region=='Australia',c(3:ncol(download_data))])
China <- colSums(download_data[download_data$Country.Region=='China'&download_data$Province.State!='Hong Kong',c(3:ncol(download_data))])
China_Exhubei <- colSums(download_data[download_data$Country.Region=='China'&download_data$Province.State!='Hong Kong'&download_data$Province.State!='Hubei',c(3:ncol(download_data))])
download_data <- replace.value(download_data,'Country.Region', from = 'Brunei', to = 'Brunei_Darussalam', verbose = FALSE)
download_data <- replace.value(download_data,'Country.Region', from = 'Cabo_Verde', to = 'Cape_Verde', verbose = FALSE)
download_data <- replace.value(download_data,'Country.Region', from = 'Congo_(Brazzaville)', to = 'Congo', verbose = FALSE)
download_data <- replace.value(download_data,'Country.Region', from = 'Congo_(Kinshasa)', to = 'Democratic_Republic_of_the_Congo', verbose = FALSE)
download_data <- replace.value(download_data,'Country.Region', from = "Cote_d'Ivoire", to = 'Cote_dIvoire', verbose = FALSE)
download_data <- replace.value(download_data,'Country.Region', from = "Guinea-Bissau", to = 'Guinea_Bissau', verbose = FALSE)
download_data <- replace.value(download_data,'Country.Region', from = "Korea,_South", to = 'South_Korea', verbose = FALSE)
download_data <- replace.value(download_data,'Country.Region', from = "Taiwan*", to = 'Taiwan', verbose = FALSE)
download_data <- replace.value(download_data,'Country.Region', from = "Tanzania", to = 'United_Republic_of_Tanzania', verbose = FALSE)
download_data <- replace.value(download_data,'Country.Region', from = "Timor-Leste", to = 'Timor_Leste', verbose = FALSE)
download_data <- replace.value(download_data,'Country.Region', from = "US", to = 'United_States_of_America', verbose = FALSE)
download_data <- download_data[download_data$Country.Region!='Australia'&download_data$Country.Region!='China'&
                                 download_data$Country.Region!= 'France'&download_data$Country.Region!= 'Denmark'&
                                 download_data$Country.Region!= 'United_Kingdom' & download_data$Country.Region!= 'Netherlands',]
download_data <- rbind(download_data,France,Denmark,UK,Netherlands,HK,Hubei)
recover <- download_data[,-1]

time <- seq(as.Date("2020-01-22"), as.Date("2020-01-22")+ncol(recover)-2, "days")

China_form <- data.frame(Date = time,
                         Country = 'China',
                         Recover = China)
China_Exhubei_form <- data.frame(Date = time,
                                 Country = 'China_Exhubei',
                                 Recover = China_Exhubei)
Australia_form <- data.frame(Date = time,
                             Country = 'Australia',
                             Recover = Australia)
#recover <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/recover.csv')
doc <- data.frame()
for (i in c(1:nrow(recover))){
  first_merge <- data.frame(Date = time,
                            Country = as.character(recover$Country.Region[i]),
                            Recover = colSums(recover[i,c(2:ncol(recover))]))
  doc <- rbind(doc, first_merge)
  
}

doc <- rbind(doc, China_form, China_Exhubei_form,Australia_form)

revised <- data.frame()
for (a in levels(doc$Country)){
  country <- doc[doc$Country==a,]
  for (i in c(2:nrow(country))){
    if (is.na(country$Recover[i])){
      country$Recover[i] <- country$Recover[i-1]
    } else if (country$Recover[i]<country$Recover[i-1]){
      country$Recover[i] <- country$Recover[i-1]
    } else {
      country$Recover[i] <- country$Recover[i]
    }
  }
  revised <- rbind(revised,country)
}


#计算新增recover
newrecover <- data.frame()
for (a in levels(revised$Country)){
  country <- revised[revised$Country==a,]
  new_recover <- c()
  for (i in c(1:nrow(country))){
    if (i == 1){
      new_recover <- c(new_recover,country$Recover[i])
    } else {
      new_recover <- c(new_recover, (country$Recover[i]-country$Recover[i-1]))
    }
  }
  country$New_Recover <- new_recover
  newrecover <- rbind(newrecover,country)
  
}

doc <- newrecover

doc$Date <- as.Date(doc$Date)

doc$Country <- as.character(doc$Country)
data$Country <- as.character(data$Country)
#processed_data$Country <- as.character(processed_data$Country)
merge <- data %>%
  left_join(doc,by=c('Country','Date'))
merge$Country <- factor(merge$Country)
#把Total Recover与New Recover的缺失值填上0
data <- data.frame()
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  for (i in c(1:nrow(country))){
    if (is.na(country$Recover[i])==TRUE)
      if (i == 1) {
        country$Recover[i] <- 0
      } else {
        country$Recover[i] <- country$Recover[i-1]
      }
    if (is.na(country$New_Recover[i])==TRUE)
      country$New_Recover[i] <- 0
  }
  data <- rbind(data,country)
}

#write.csv(data,'/Users/qidiwang1/Desktop/test_merge.csv')
#放入一个从12月31号开始数的JDate

JDate <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/JDate.csv')
JDate <- na.omit(JDate)
JDate$Date <- as.Date(JDate$Date,'%m/%d/%y')


merge <- data%>%
  left_join(JDate,by=c('Date'))

merge <- cbind(JDATE=merge[,ncol(merge)],merge[,1:(ncol(merge)-1)])
#write.csv(merge,'/Users/qidiwang1/Desktop/merge.csv')
merge$alpha.3<-as.character(merge$alpha.3)

#merge test的数据
#merge <- read.csv('/Users/qidiwang1/Desktop/test_merge.csv')
#merge$Date <- as.Date(merge$Date,'%m/%d/%y')
#merge$alpha.3 <- as.character(merge$alpha.3)

test <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')
test$alpha.3 <- as.character(test$iso_code)
test$Date <- as.Date(test$date)
test$Total.tests <- test$total_tests
data <- merge%>%
  left_join(test[,c('alpha.3','Date','Total.tests')], by = c('Date', 'alpha.3'))
data2 <- data
data2$Country <- factor(data2$Country)

data <- data.frame()
for (a in levels(data2$Country)){
  country <- data2[data2$Country==a,]
  for (i in c(1:nrow(country))){
    if (is.na(country$Total.tests[i]))
      if (i == 1){
        country$Total.tests[i] <- 0
      } else {
        country$Total.tests[i] <- country$Total.tests[i-1]
      }
  }
  if (sum(country$Total.tests)==0)
    country$Total.tests <- NA
  data <- rbind(data,country)
}



#data <- data[data$Date=='2020-05-20',]
#write.csv(data,'/Users/qidiwang1/Desktop/test_merge2.csv')
#data <- read.csv('/Users/qidiwang1/Desktop/test_merge.csv')
#merge RT的数据
data$Country <- factor(data$Country)
data$New.cases <- as.numeric(as.character(data$New.cases))

merge <- data.frame()
for (a in levels(data$Country)){
  country <- data[data$Country==a,]
  first <- c()
  second <- c()
  for (i in c(1:6)){
    first <- c(first,0)
  }
  for (i in c(7:nrow(country))){
    second <- c(second,mean(country$New.cases[(i-6):i]))
  }
  full <- c(first,second)
  country$smooth_7_day <- full
  merge <- rbind(merge,country)
}

data <- data.frame()
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  first <- c()
  second <- c()
  for (i in c(1:(nrow(country)-7))){
    rt <- (country$smooth_7_day[i+1]*0.044395165+country$smooth_7_day[i+2]*0.135112483+
      country$smooth_7_day[i+3]*0.192959607+country$smooth_7_day[i+4]*0.202165107+
      country$smooth_7_day[i+5]*0.17874358+country$smooth_7_day[i+6]*0.141947598+
      country$smooth_7_day[i+7]*0.104676459)/country$smooth_7_day[i]
    first <- c(first,rt)

    
  }
  for (i in c((nrow(country)-6):nrow(country))){
    rt2 <- 0
    second <- c(second,rt2)
  }
  combine <- c(first,second)
  country$RT <- combine
  data <- rbind(data,country)
}

for (i in c(1:nrow(data))){
  if (data$RT[i]=='NaN'|data$RT[i]=='Inf'|data$RT[i]<=0)
    data$RT[i] <- NA
}
for (i in c(1:nrow(data))){
  if (is.na(data$RT[i])==FALSE)
    if (data$RT[i]>6)
      data$RT[i] <- 6.0
}

#write.csv(data,'/Users/qidiwang1/Desktop/test_merge.csv')

#merge mobility的数据
mobility <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/Mobility_Index.csv')
mobility$Date <- as.Date(mobility$Date)
mobility$alpha.2 <- as.character(mobility$alpha.2)


merge <- data%>%
  left_join(mobility, by = c('Date', 'alpha.2'))


write.csv(merge,'/Users/qidiwang1/Desktop/疫情数据库/merge.csv')








