library(EpiEstim)
merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
merge <- merge[merge$confirm>=50,]
merge <- merge[merge$New.cases>=0,]
merge$Country.x <- factor(merge$Country.x)

final <- data.frame()
for (a in levels(merge$Country.x)){
  country <- merge[merge$Country.x==a,]
  if (nrow(country)<10){
    print(paste(as.character(a),'cannot be analyzed!'))
  } else {
    Rt <- estimate_R(incid=country$New.cases, method = "parametric_si",
                    config = make_config(list(t_start = seq(2, (nrow(country)-6)),t_end = seq(8, nrow(country)),mean_si = 5, std_si = 7)))
    rttt <- cbind(Date = country$Date[c(2:(nrow(country)-6))], Country = country$Country.x[c(2:(nrow(country)-6))], confirm = country$confirm[c(2:(nrow(country)-6))],  
                 newcase = country$New.cases[c(2:(nrow(country)-6))], RT = Rt$R)
  }
  final <- rbind(final, rttt)
}
View(final)

final$newcase<-as.numeric(as.character(final$newcase))
final <- final[final$newcase>0,]

final$Date <- as.Date(final$Date)
library(ggplot2)
final$Country<-factor(final$Country)
for (a in levels(final$Country)){
  country <- final[final$Country==a,]
  p <- ggplot()+
    geom_line(aes(x=country$Date,y=country$`RT.Mean(R)`))+
    ggtitle(as.character(a))
  ggsave(p,filename = paste('/Users/qidiwang1/Desktop/RT/',as.character(a),'.png'))
}

write.csv(final,'/Users/qidiwang1/Desktop/rt.csv')

merge[which(merge$New.cases<0),]
sum(data$New.cases<0)

country$Date
Rt$R
country <- merge[merge$Country.x=='Spain',]
Rt <- estimate_R(incid=country$confirm, method = "parametric_si",
                 config = make_config(list(t_start = seq(2, (nrow(country)-6)),t_end = seq(8, nrow(country)),mean_si = 4, std_si = 1.5)))
nrow(Rt$R)
nrow(country)
