gdp <- read.csv('/Users/qidiwang1/Desktop/gdp.csv')
names(gdp)[3] <- 'area_name'
gdp$index_dt <- as.numeric(as.character(gdp$index_dt))
gdp$area_name <- as.character(gdp$area_name)
merge <- data4%>%
  left_join(gdp, by = c('index_dt','area_name'))

merge$area_name <- factor(merge$area_name)

cor(merge$gdp_relative,merge$spend_all,use = "na.or.complete")

recover_day <- read.csv('/Users/qidiwang1/Desktop/p_day.csv')
recover_day$P.day <- as.numeric(as.character(as.Date(recover_day$P.day,'%Y/%m/%d'),'%Y%m%d'))  
names(recover_day)[1] <- 'area_name'
merge$area_name <- as.character(merge$area_name)
recover_day$area_name <- as.character(recover_day$area_name)
merge <- merge%>%
  left_join(recover_day, by = 'area_name')
merge$area_name <- factor(merge$area_name)

library("gridExtra")
library('ggpubr')

for (a in levels(merge$area_name)){
  country <- merge[merge$area_name==a,]
  p1 <- ggplot()+
    geom_path(aes(x=country$confirm_doubling_days.x, y = country$spend_all))+
    geom_vline(xintercept = country$confirm_doubling_days.x[which(country$index_dt==country$P.day[1])],size = 0.8, color = "#990000", linetype = "dashed")+
    xlab('Doubling Days')+
    ylab('Consumer Spending')+
    ggtitle(paste(as.character(a),'Total Cases=',as.character(max(country$total_cases))))
  p2 <- ggplot()+
    geom_path(aes(x=country$confirm_doubling_days.x, y = country$gdp_relative))+
    geom_vline(xintercept = country$confirm_doubling_days.x[which(country$index_dt==country$P.day[1])],size = 0.8, color = "#990000", linetype = "dashed")+
    xlab('Doubling Days')+
    ylab('Economic Loss')+
    ggtitle(as.character(a))
  plot_graph <- ggarrange(p1,p2,ncol = 2, nrow = 1)
  ggsave(plot_graph, filename = paste('/Users/qidiwang1/Desktop/mg&cs/',as.character(a),'.png'))
}

write_xlsx(merge,'/Users/qidiwang1/Desktop/merge.xlsx')
