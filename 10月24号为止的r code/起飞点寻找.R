merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
gdp <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp$relative.csv')
merge <- merge[merge$alpha.3%in%levels(gdp$iso3_name),]
merge$Country<-factor(merge$Country)
library(smoother)
data <- data.frame()
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  smooth1 <- smth(country$New.cases,method = 'gaussian')
  smooth2 <- smth(country$New.death, method = 'gaussian')
  data <- rbind(data,cbind(Date=as.character(country$Date),iso3_name = as.character(country$alpha.3),Country=as.character(country$Country),confirm=as.numeric(as.character(country$confirm)),
                death=as.numeric(as.character(country$death)),New.cases=as.numeric(as.character(country$New.cases)),
                New.death=as.numeric(as.character(country$New.death)),smooth1,smooth2))
  
}

data$confirm <-as.numeric(as.character(data$confirm))
data$death <- as.numeric(as.character(data$death))
data$New.cases <- as.numeric(as.character(data$New.cases))
data$New.death <- as.numeric(as.character(data$New.death))
data$smooth1 <- as.numeric(as.character(data$smooth1))
data$smooth2 <- as.numeric(as.character(data$smooth2))
data <- data[data$confirm>5,]


data$Country<-factor(data$Country)
one_stick <- data.frame()
for (a in levels(data$Country)){
  country <- data[data$Country==a,]
  #country <- na.omit(country)
  if (max(country$confirm)<1000){
    print(paste(a,'is not analyzable'))
  } else{
    analysis <- country[country$confirm<=1000,]

    x <- c(1:nrow(analysis))
    #x2 <- c(1:nrow(analysis2))

    linear <- piecewise.linear(x=x,y=analysis$confirm,middle=1,CI = FALSE)

    #linear2 <- piecewise.linear(x=x2,y=analysis2$confirm,middle=1,CI=FALSE)
  


    #print(paste('The take off point of',a,':',analysis[round(linear$change.point),]$JDATE))
    #p <- ggplot()+
   #   geom_line(aes(x=country$JDATE,y=country$confirm))+
      #geom_line(aes(x=analysis$JDATE,y=predict(linear,x)))+
      #geom_line(aes(x=country$JDATE,y=country$smooth1))+
     # geom_vline(xintercept = analysis[round(linear$change.point),]$JDATE, size = 0.8, color = "#990000", linetype = "dashed")+
      #geom_vline(xintercept = analysis2[round(linear2$change.point),]$JDATE, size = 0.8, color = "green", linetype = "dashed")+
      #geom_vline(xintercept = analysis[which(analysis$smooth1==max(analysis$smooth1)),]$JDATE,size = 0.8, color = "grey", linetype = "dashed")+
     # ggtitle(a)
    #ggsave(p,filename = paste('/Users/qidiwang1/Desktop/graph2/',a,'.png'))
    #one_stick <- rbind(one_stick, cbind(Country = as.character(a), iso3_name = as.character(analysis$iso3_name), DATE_take_off = as.character(analysis[round(linear$change.point),]$Date)))
  }
  one_stick <- rbind(one_stick, cbind(Country = as.character(a), iso3_name = as.character(country$iso3_name[1]), DATE_take_off = as.character(analysis[round(linear$change.point),]$Date)))
}

merge$C7_Restrictions.on.internal.movement <- as.numeric(as.character(merge$C7_Restrictions.on.internal.movement))


lockdown_date <- data.frame()
for (a in levels(merge$Country)){
  country <- merge[merge$Country==a,]
  ddd <- head(country[country$C7_Restrictions.on.internal.movement==2,],1)
  if (nrow(ddd)==0){
    print(paste('OMG!',a,'will be terminated!'))
  }else{
    lockdown_date <- rbind(lockdown_date,cbind(iso3_name = as.character(ddd$alpha.3),lockdown_date=as.character(ddd$Date)))
  }

} 

one_stick$iso3_name <- as.character(one_stick$iso3_name)
lockdown_date$iso3_name <- as.character(lockdown_date$iso3_name)

data <- one_stick%>%
  left_join(lockdown_date,by='iso3_name')
View(data)
old_data <- read.csv('/Users/qidiwang1/Desktop/data.csv')


old_data$iso3_name <- as.character(old_data$iso3_name)

data2 <- old_data%>%
  left_join(data, by = 'iso3_name')
View(data2)
library(writexl)
write_xlsx(data2,'/Users/qidiwang1/Desktop/GAP.xlsx')



for (a in levels(data$Country.x)){
  country <- data[data$Country.x==a,]
  country <- na.omit(country)
  analysis <- country[country$JDATE<country[which(country$smooth1==max(country$smooth1)),]$JDATE,]
  analysis2 <- country[country$JDATE<country[which(country$smooth2==max(country$smooth2)),]$JDATE,]
  if (nrow(analysis)<10|nrow(analysis2)<10){
    print(paste(a,'cannot be analyzed'))
  } else {
    
    
    x <- c(1:nrow(analysis))
    x2 <- c(1:nrow(analysis2))
    
    linear <- piecewise.linear(x=x,y=analysis$smooth1,middle=1,CI = FALSE)
    linear2 <- piecewise.linear(x=x2,y=analysis2$smooth2,middle=1,CI=FALSE)
    print(paste('The take off point of',a,':',analysis[round(linear$change.point),]$JDATE))
    p <- ggplot()+
      geom_line(aes(x=country$JDATE,y=country$confirm))+
      geom_vline(xintercept = analysis[round(linear$change.point),]$JDATE, size = 0.8, color = "#990000", linetype = "dashed")+
      geom_vline(xintercept = analysis[round(linear2$change.point),]$JDATE, size = 0.8, color = "green", linetype = "dashed")+
      geom_vline(xintercept = analysis[which(analysis$smooth1==max(analysis$smooth1)),]$JDATE,size = 0.8, color = "grey", linetype = "dashed")+
      ggtitle(a)
    ggsave(p,filename = paste('/Users/qidiwang1/Desktop/graph/',a,'2.png'))
  }
}

library(segmented)
xx <- 1:100
zz <- runif(100)
yy <- 2 + 1.5*pmax(xx - 35, 0) - 1.5*pmax(xx - 70, 0) + 15*pmax(zz - .5, 0) + 
  rnorm(100,0,2)
dati <- data.frame(x = xx, y = yy, z = zz)

out.lm <- lm(analysis$smooth1 ~ c(1:nrow(analysis)))
o <- segmented(out.lm, seg.Z = ~x, psi = list(x = c(1,nrow(analysis)),
               control = seg.control(display = FALSE)
))
dat2 = data.frame(x = xx, y = broken.line(o)$fit)

library(ggplot2)
ggplot(dati, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat2, color = 'blue')
