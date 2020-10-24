data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')
gap <- read.csv('/Users/qidiwang1/Desktop/GAP.csv')

blog <- data.frame()

for (a in levels(data$Country.x)){
  country <- data[data$Country.x==a,]
  blog <- rbind(blog,cbind(country[1,],maxscore=max(country$Daily_Policy)))
}

blog$Country.x <- as.character(blog$Country.x)
gap$Country.x <- as.character(gap$Country.x)


merge <- gap%>%
  left_join(blog, by = 'Country.x')
write.csv(merge,'/Users/qidiwang1/Desktop/blog.csv')

gc();rm(list = ls())
#setwd("/Users//Users/qidiwang1/Desktop/plot_codes/")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/helper.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/lha_theme.R")
source("/Users/qidiwang1/Desktop/plot_codes/Codes/functions/plot_functions.R")
blog <- read.csv('/Users/qidiwang1/Desktop/blog.csv')

blog <- blog[c(1:55),]
blog$GAP <- as.numeric(as.character(blog$GAP))
blog$epedemic <- as.numeric(as.character(blog$epedemic))
linear <- lm(blog$epedemic~blog$GAP)
a <- predict(linear,data=blog$GAP)

blog$Income <- as.character(blog$Income)

is_chn = F
ggplot()+
  geom_point(aes(x=blog$GAP,y=blog$epedemic,color=blog$Income,size = blog$tests.per.million))+
  geom_text(aes(x=blog$GAP,y=blog$epedemic,label=blog$Country.x,color=blog$Income),hjust=-0.1,vjust=-0.1)+
  geom_line(aes(x=blog$GAP,y=a),linetype='dashed')+
  xlab('Gap Between Lockdown Date and Takeoff Date')+
  ylab('Gap Between Max Death Date and Takeoff Date')+
  scale_color_manual(values=c('burlywood','coral4','darkslategray'),
                     limits=c('High income','Upper-Middle income','Lower-Middle income'))+
  lha_theme(is_chn) +
  labs(color = '', size = 'Tests per Million')
  


