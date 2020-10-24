library(smoother)
merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')

a<- merge[merge$Country == 'Germany',]
x <- a$JDATE
y <- a$New.cases
z <- smth(y,method = 'gaussian')




plot(d1)
ggplot()+
  geom_point(aes(x=x,y=y))+
  geom_line(aes(x=x,y=z))+

  ggtitle(a$Country[1])















lockdown <- data.frame()
for (a in levels(merge$Country)) {
  country <- merge[merge$Country==a,]
  country <- country[which(country$S6_Restrictions.on.internal.movement>1)[1],]
  lockdown <- rbind(lockdown,cbind(as.character(country$Country),as.character(country$Date), 
                                   country$S6_Restrictions.on.internal.movement,country$S6_IsGeneral))
}
write.csv(lockdown,'/Users/qidiwang1/Desktop/lockdown.csv')
which(merge$S6_Restrictions.on.internal.movement==2)
merge[merge$Country=='United_States_of_America',][1,]
[merge[merge$Country=='United_States_of_America',]$S6_Restrictions.on.internal.movement==2,]
View(lockdown)
