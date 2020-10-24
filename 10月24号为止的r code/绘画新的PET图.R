merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/数据大表.csv')
gdp <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/gdp$relative.csv')
ICL <- read.csv('/Users/qidiwang1/Desktop/ICL.csv')
merge <- merge%>%
  group_by(Country)%>%
  mutate(newcase_7d = rollmean(New_case,7,fill=NA,align='right'),
         newdeath_7d = rollmean(New_death,7,fill=NA,align='right'))
merge$Date <- as.Date(merge$Date)
ICL$Date <- as.Date(ICL$Date,'%m/%d/%y')
data <- merge%>%
  left_join(gdp,by=c('iso3_name','index_dt'))%>%
  left_join(ICL,by=c('iso3_name','Date'))
data <- data[!is.na(data$gdp_relative),]
write_xlsx(data,'/Users/qidiwang1/Desktop/data.xlsx')



setwd('/Users/qidiwang1/Desktop/官方统计死亡vs经济活跃度')
getwd()

#data <- data[!is.na(data$ICL),]
for (a in levels(factor(data$Country))){
  country <- data[data$Country==a,]
  x=as.Date(country$Date)
  y1 = country$newdeath_7d
  y2=country$gdp_relative
  png(paste(as.character(a),'.png'))
  par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
  plot(x, y1, pch = 16, col = 2,ylim = c(-max(y1),max(y1)),ylab='New death',xlab='Date',main=as.character(a))              # Create first plot
  par(new = TRUE)                             # Add new plot
  plot(x, y2, pch = 17, col = 3,              # Create second plot without axes
       axes = FALSE, xlab = "", ylab = "",ylim = c(0.7,1.4))
  axis(side = 4, at = c(0.7,0.8,0.9,1,1.1,1.2,1.3,1.4))      # Add second axis
  mtext("Economic Activity", side = 4, line = 3) 
  dev.off()
}
png('France.png')
par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(x, y1, pch = 16, col = 2,ylim = c(-max(y1),max(y1)),ylab='New cases',xlab='Date',main='France')              # Create first plot
par(new = TRUE)                             # Add new plot
plot(x, y2, pch = 17, col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "",ylim = c(0.7,1.4))
axis(side = 4, at = c(0.7,0.8,0.9,1,1.1,1.2,1.3,1.4))      # Add second axis
mtext("Economic Activity", side = 4, line = 3) 
dev.off()
