merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
google <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/google.csv')  
get_mobility_data <- function(data){
  data$index_1 <- rowMeans(data[,c('grocery_and_pharmacy_percent_change_from_baseline','retail_and_recreation_percent_change_from_baseline')],na.rm = TRUE)/100+1
  data$index_2 <- (100+data$workplaces_percent_change_from_baseline)/100
  data$google_index <- rowMeans(data[,c('index_1','index_2')],na.rm = TRUE)
  
  data$country_region <- factor(data$country_region)
  google_mobility <- data.frame()
  for (a in levels(data$country_region)){
    state <- data[data$country_region==a,]
    google_avg <- c()
    for (i in c(1:6)){
      google_avg <- c(google_avg,as.double(NA))
    }
    for (i in c(7:nrow(state))){
      google_avg <- c(google_avg,mean(state$google_index[(i-6):i],na.rm = TRUE))
    }
    state$google_avg <- google_avg
    google_mobility <- rbind(google_mobility,state)
  }
  google_mobility$date <- as.Date(google_mobility$date)
  return(data.frame(index_dt = as.numeric(as.character(google_mobility$date,'%Y%m%d')),
                    State = google_mobility$country_region,
                    iso2_name = google_mobility$country_region_code,
                    Mobility = google_mobility$google_avg,
                    Economic_Activity = 0.3886*google_mobility$google_avg+0.61))
  
}  

google_mm <- google[google$sub_region_1==''& !is.na(google$country_region_code) & google$sub_region_2==''&google$metro_area=='',]

mobility <- get_mobility_data(google_mm)

merge <- merge%>%
  left_join(mobility,by=c('index_dt','iso2_name'))

merge <- merge%>%
  group_by(Country)%>%
  mutate(newcase_7d = rollmean(New_case,7,fill=NA,align='right'),
         newdeath_7d = rollmean(New_death,7,fill=NA,align='right'))
merge$case_per_million <- merge$Total_cases/merge$Population*1000000
merge$death_per_million <- merge$Total_death/merge$Population*1000000


merge <- merge%>%
  group_by(Country)%>%
  mutate(max_case = max(newcase_7d[index_dt>=20200301&index_dt<=20200630]))

countries <- c('United_Kingdom','Russia','Norway','Sweden','United_States_of_Americas',
               'Austria','Belgium','Bulgaria','Croatia','Czechia','Cyprus','Denmark','Estonia',
               'Finland','France','Germany','Greece','Hungary','Ireland','Italy','Latvia','Lithuania',
               'Luxembourg','Malta','Netherlands','Poland','Portugal','Romania','Slovakia','Slovenia',
               'Spain','Switzerland','India','Brazil','Japan','Bosnia_and_Herzegovina','Montenegro',
               'Serbia','North_Macedonia','Ukraine','Albania','Belarus')
list_country <- merge[merge$Country%in%countries,]
estimate <- read.csv('/Users/qidiwang1/Desktop/0925宏观周报/estimate_data.csv')
estimate$ICL_estimate[is.na(estimate$ICL_estimate)] <- 0
estimate <- estimate%>%
  group_by(iso3_name)%>%
  mutate(estimate_total = cumsum(ICL_estimate))

estimate$Date <- as.Date(estimate$Date,'%m/%d/%y')

list_country$Date <- as.Date(list_country$Date)
list_country <- list_country%>%
  left_join(estimate,by=c('iso3_name','Date'))
list_country <- list_country[list_country$index_dt==20200925,]


write_xlsx(list_country,'/Users/qidiwang1/Desktop/list_country2.xlsx')

library(scales)
library(ggpubr)
get_hotspot_data <- function(data){
  max_case <- max(data$newcase_7d[data$index_dt>=20200301&data$index_dt<=20200630])
  data$ratio <- data$newcase_7d/max_case
  if (sum(is.na(data$Total_tests))>0){
    p1 <- ggplot()+
      geom_line(aes(x=as.Date(data$Date),y=data$Total_tests))+
      ylab('没测试数据')
  } else {
    p1 <- ggplot()+
      geom_line(aes(x=as.Date(data$Date[data$Total_tests>0]),y=data$Total_cases[data$Total_tests>0]/data$Total_tests[data$Total_tests>0]))+
      xlab('')+
      ylab('测试阳性率')+
      scale_x_date(date_breaks = "1 month", date_labels =  "%m")+
      theme(axis.title.y = element_text(family="FZLanTingHeiS-R-GB"))
  }

  p2 <- ggplot()+
    geom_line(aes(x=as.Date(data$Date),y=data$newcase_7d))+
    xlab('')+
    ylab('新增病例')+
    scale_x_date(date_breaks = "1 month", date_labels =  "%m")+
    theme(axis.title.y = element_text(family="FZLanTingHeiS-R-GB"))
  p3 <- ggplot()+
    geom_line(aes(x=as.Date(data$Date),y=data$newdeath_7d))+
    xlab('')+
    ylab('新增死亡')+
    scale_x_date(date_breaks = "1 month", date_labels =  "%m")+
    theme(axis.title.y = element_text(family="FZLanTingHeiS-R-GB"))
  p4 <- ggplot()+
    geom_line(aes(x=as.Date(data$Date),y=data$ratio))+
    xlab('')+
    ylab('与春季最高值对比比例')+
    scale_x_date(date_breaks = "1 month", date_labels =  "%m")+
    theme(axis.title.y = element_text(family="FZLanTingHeiS-R-GB"))
  p5 <- ggplot()+
    geom_line(aes(x=as.Date(data$Date),y=data$newcase_7d/data$Population*1000000))+
    xlab('')+
    ylab('每百万人新增病例')+
    scale_x_date(date_breaks = "1 month", date_labels =  "%m")+
    theme(axis.title.y = element_text(family="FZLanTingHeiS-R-GB"))
  p6 <- ggplot()+
    geom_line(aes(x=as.Date(data$Date),y=data$newdeath_7d/data$Population*1000000))+
    xlab('')+
    ylab('每百万人新增死亡')+
    scale_x_date(date_breaks = "1 month", date_labels =  "%m")+
    theme(axis.title.y = element_text(family="FZLanTingHeiS-R-GB"))
  p7 <- ggplot()+
    geom_line(aes(x=as.Date(data$Date),y=data$Total_tests/data$Population*1000000))+
    xlab('')+
    ylab('每百万人测试率')+
    scale_x_date(date_breaks = "1 month", date_labels =  "%m")+
    theme(axis.title.y = element_text(family="FZLanTingHeiS-R-GB"))+
    ylim(0,350000)
  p8 <- ggplot()+
    geom_line(aes(x=as.Date(data$Date),y=data$Economic_Activity))+
    xlab('')+
    ylab('日度经济活跃度')+
    scale_x_date(date_breaks = "1 month", date_labels =  "%m")+
    theme(axis.title.y = element_text(family="FZLanTingHeiS-R-GB"))
  ppp <- ggarrange(p1,p7,p2,p3,p4,p8,ncol=2,nrow=3)
  ggsave(ppp,filename = paste('/Users/qidiwang1/Desktop/东欧疫情/',as.character(data$Country[1]),'.png'))
}



East_Europe <- merge[merge$Sub_region=='Eastern Europe',]

for (a in levels(factor(East_Europe$Country))){
  country <- East_Europe[East_Europe$Country==a,]
  get_hotspot_data(data = country)
}


USA <- merge[merge$iso3_name=='USA',]
get_hotspot_data(USA)
India <- merge[merge$Country=='India',]
get_hotspot_data(India)




