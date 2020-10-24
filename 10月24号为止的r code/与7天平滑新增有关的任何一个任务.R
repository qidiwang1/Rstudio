library(writexl)
data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')

TOP20 <- read_excel('/Users/qidiwang1/Desktop/TOP20.xlsx')
TOP20$id_13 <- factor(TOP20$id_13)

#data <- data[data$alpha.3%in% levels(factor(TOP20$id_13)),]

data$Country <- factor(data$Country)
merge <- data.frame()
for (a in levels(data$Country)){
  country <- data[data$Country==a,]
  newcase_7day <- c()
  for (i in c(1:6)){
    newcase_7day <- c(newcase_7day,as.double(NA))
  }
  for (i in c(7:nrow(country))){
    newcase_7day <- c(newcase_7day,mean(country$New.cases[(i-6):i]))
  }
  country$newcase_7day <- newcase_7day
  merge <- rbind(merge,country)
}
data <- data.frame()
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  ratio <- c()
  for (i in c(1:nrow(country))){
    ratio <- c(ratio,country$newcase_7day[i]/max(country$newcase_7day,na.rm = TRUE))
  }
  country$ratio <- ratio
  data <- rbind(data,country)
  
}

advance <- data.frame()
for (a in levels(factor(data$Date))){
  certain_date <- data[data$Date==a,]
  advanced <- sum(certain_date$newcase_7day[certain_date$IMF.A.E=='Advanced'])
  Emerging <- sum(certain_date$newcase_7day[certain_date$IMF.A.E!='Advanced'&certain_date$Country!='China'])
  advance <- rbind(advance,data.frame(Date = as.character(a),
                                      advanced = advanced,
                                      others = Emerging))
}
write_xlsx(advance,'/Users/qidiwang1/Desktop/advanced.xlsx')

regions <- data.frame()
for (a in levels(factor(data$Date))){
  certain_date <- data[data$Date==a,]
  europe <- sum(certain_date$newcase_7day[certain_date$region=='Europe'])
  africa <- sum(certain_date$newcase_7day[certain_date$region=='Africa'])
  oceania <- sum(certain_date$newcase_7day[certain_date$region=='Oceania'])
  latin <- sum(certain_date$newcase_7day[certain_date$sub.region=='Latin America and the Caribbean'])
  northA <- sum(certain_date$newcase_7day[certain_date$sub.region=='Northern America'])
  sAsia <- sum(certain_date$newcase_7day[certain_date$sub.region=='Southern Asia'])
  rAsia <- sum(certain_date$newcase_7day[certain_date$region=='Asia'&certain_date$sub.region!='Southern Asia'])
  regions <- rbind(regions,data.frame(Date = as.character(a),
             Europe = europe,
             Africa = africa,
             Oceania = oceania,
             Latin = latin,
             North_America = northA,
             South_Asia = sAsia,
             Rest_of_Asia = rAsia))
}
regions$Date <- as.Date(regions$Date)
ggplot()+
  geom_line(aes(x=regions$Date,y=regions$Europe,color = 'Europe'))+
  geom_line(aes(x=regions$Date,y=regions$Africa, color = 'Africa'))+
  geom_line(aes(x=regions$Date,y=regions$South_Asia, color = 'South_Asia'))+
  geom_line(aes(x=regions$Date,y=regions$Rest_of_Asia, color = 'Rest_of_Asia'))+
  geom_line(aes(x=regions$Date, y =regions$Oceania, color = 'Oceania'))+
  geom_line(aes(x=regions$Date,y=regions$North_America, color = 'North_America'))+
  geom_line(aes(x=regions$Date,y=regions$Latin, color = 'Latin America'))+
  xlab('Date')+
  ylab('New confirmed cases')
write_xlsx(regions,'/Users/qidiwang1/Desktop/regions.xlsx')

today <- data[data$Date=='2020-08-16',]
ggplot()+
  geom_point(aes(x=log(today$death,10),y=log(today$confirm,10)))

countries <- data[data$Country%in%c('Germany','United_Kingdom','Spain','France','Norway','Switherland'),]
ggplot()+
  geom_line(aes(x=as.Date(countries$Date),y=countries$newcase_7day,color=countries$Country))



data$ratio[data$ratio=='NaN']<- as.double(NA)
data$Date <- as.Date(data$Date)
for (a in levels(data$Country)){
  country <- data[data$Country==a,]
  p <- ggplot()+
    geom_line(aes(x=country$Date,y=country$ratio))+
    ggtitle(as.character(a))
  ggsave(p, filename = paste('/Users/qidiwang1/Desktop/aaa/',as.character(a),'.png'))
}

View(country)
whole_condition <- merge%>%
  group_by(factor(Date))%>%
  mutate(whole = sum(newcase_7day)/sum(Population)*1000000)

whole_condition <- whole_condition[whole_condition$Country=='China',c('Date','whole')]
merge$newcase_per_million <- merge$newcase_7day/merge$Population*1000000
whole_condition$Date <- as.Date(whole_condition$Date)
merge$Date <- as.Date(merge$Date)

ggplot()+
  geom_line(aes(x=whole_condition$Date,y=whole_condition$whole))+
  geom_line(aes(x=merge$Date,y=merge$newcase_per_million,color = merge$Country))
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  p <- ggplot()+
    geom_line(aes(x=country$Date,y=country$newcase_per_million))+
    xlab('Date')+
    ylab('New cases per million people')+
    ggtitle(as.character(a))
  ggsave(p, filename = paste('/Users/qidiwang1/Desktop/newcase_7day/',as.character(a),'.png'))
}

high_little_decrease <- merge[merge$Country%in%c('Brazil','South_Africa','United_States_of_America','Saudi_Arabia','Mexico','Russia'),]
a1 <- ggplot()+
  geom_line(aes(x=whole_condition$Date,y=whole_condition$whole),linetype = "dashed",color = 'grey')+
  geom_line(aes(x=high_little_decrease$Date,y=high_little_decrease$newcase_per_million,color = factor(high_little_decrease$Country)))+
  xlab('')+
  ylab('New cases per million people')+
  ylim(0,200)

low_little_decrease <- merge[merge$Country%in%c('Bangladesh','Egypt','Pakistan'),]
a2 <- ggplot()+
  geom_line(aes(x=whole_condition$Date,y=whole_condition$whole),linetype = "dashed",color = 'grey')+
  geom_line(aes(x=low_little_decrease$Date,y=low_little_decrease$newcase_per_million,color = factor(low_little_decrease$Country)))+
  xlab('')+
  ylab('New cases per million people')+
  ylim(0,200)

recovery <- merge[merge$Country%in%c('China','Canada','South_Koera','Italy','Germany','Thailand','United_Kingdom'),]
a3 <- ggplot()+
  geom_line(aes(x=whole_condition$Date,y=whole_condition$whole),linetype = "dashed",color = 'grey')+
  geom_line(aes(x=recovery$Date,y=recovery$newcase_per_million,color = factor(recovery$Country)))+
  xlab('')+
  ylab('New cases per million people')+
  ylim(0,200)

increasing <- merge[merge$Country%in%c('France','India','Indonesia','Japan','Philippines','Spain','Turkey'),]
a4 <- ggplot()+
  geom_line(aes(x=whole_condition$Date,y=whole_condition$whole),linetype = "dashed",color = 'grey')+
  geom_line(aes(x=increasing$Date,y=increasing$newcase_per_million,color = factor(increasing$Country)))+
  xlab('')+
  ylab('New cases per million people')+
  ylim(0,200)
library(ggpubr)
figure <- ggarrange(a1, a2, a3,a4,
                    labels = c("A", "B", "C","D"),
                    ncol = 2, nrow = 2)
figure
