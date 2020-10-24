merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
mobility <- read.csv('/Users/qidiwang1/Desktop/gdp&mobility.csv')
#merge$New.cases <- as.numeric(as.character(merge$New.cases))
#merge$New.death <- as.numeric(as.character(merge$New.death))
#merge$New_Recover <- as.numeric(as.character(merge$New_Recover))
haha <- data.frame()
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  cases_7 <- c()
  deaths_7 <-c()
  recover_7 <- c()
  for (i in c(1:3)){
    cases_7 <- c(cases_7,0)
    deaths_7 <- c(deaths_7,0)
    recover_7 <- c(recover_7,0)
  }
  for (i in c(4:(nrow(country)-3))){
    cases_7 <- c(cases_7,mean(country$New.cases[(i-3):(i+3)]))
    deaths_7 <- c(deaths_7, mean(country$New.death[(i-3):(i+3)]))
    recover_7 <- c(recover_7, mean(country$New_Recover[(i-3):(i+3)]))
  }
  for (i in c((nrow(country)-2):nrow(country))){
    if (i == nrow(country)-2) {
      cases_7 <- c(cases_7,mean(country$New.cases[(i-3):nrow(country)]))
      deaths_7 <- c(deaths_7,mean(country$New.death[(i-3):nrow(country)]))
      recover_7 <- c(recover_7,mean(country$New_Recover[(i-3):nrow(country)]))
    } else if (i == nrow(country)-1){
      cases_7 <- c(cases_7,mean(country$New.cases[(i-3):nrow(country)]))
      deaths_7 <- c(deaths_7,mean(country$New.death[(i-3):nrow(country)]))
      recover_7 <- c(recover_7,mean(country$New_Recover[(i-3):nrow(country)]))
    } else if (i == nrow(country)){
      cases_7 <- c(cases_7,mean(country$New.cases[(i-3):nrow(country)]))
      deaths_7 <- c(deaths_7,mean(country$New.death[(i-3):nrow(country)]))
      recover_7 <- c(recover_7,mean(country$New_Recover[(i-3):nrow(country)]))
    }
  }
  country$cases_7 <- cases_7
  country$deaths_7 <- deaths_7
  country$recover_7 <- recover_7
  haha <- rbind(haha,country)
}

haha$JDATE <- as.numeric(as.character(haha$JDATE))
haha$cases_7 <- as.numeric(as.character(haha$cases_7))
haha$recover_7 <- as.numeric(as.character(haha$recover_7))
data2 <- haha[haha$alpha.2%in%levels(mobility$iso2_name),]
data2 <- data2[data2$recover_7>=data2$cases_7,]

#data2 <- data2[data2$JDATE!=143,]
#data2 <- data2[data2$JDATE!=144,]
#data2 <- data2[data2$JDATE!=145,]

View(data2[data2$confirm>=150,c('Country','Date','confirm','smooth_confirm_doubling_days','RT','cases_7','recover_7')])
#View(haha)
#View(haha[haha$recover_7>haha$cases_7,])
for (a in levels(haha$Country)){
  country <- haha[haha$Country==a,]
  p <- ggplot()+
    geom_line(aes(x=country$JDATE,y=country$cases_7,color = 'cases'))+
    geom_line(aes(x=country$JDATE,y=country$recover_7, color = 'recover'))+
    ggtitle(as.character(a))
  ggsave(p, filename = paste('/Users/qidiwang1/Desktop/compare/',as.character(a),'.png'))
}

