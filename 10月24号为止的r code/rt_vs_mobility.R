merge <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/merge.csv')

rt_vs_mobility <- data.frame()
for (a in levels(merge$Date)){
  certain_date <- merge[merge$Date==a,]
  mobility <- mean(certain_date$Mobility_Index,na.rm=TRUE)
  rt <- mean(certain_date$RT, na.rm=TRUE)
  rt_vs_mobility <- rbind(rt_vs_mobility, cbind(Date = as.character(a), rt = rt, mobility = mobility))
  
}

rt_vs_mobility$rt <- as.numeric(as.character(rt_vs_mobility$rt))
rt_vs_mobility$mobility <- as.numeric(as.character(rt_vs_mobility$mobility))
for (i in c(1:nrow(rt_vs_mobility))){
  if (is.na(rt_vs_mobility$rt[i])==FALSE)
    if (rt_vs_mobility$rt[i]>6)
      rt_vs_mobility$rt[i] <- 6
  
}

write.csv(rt_vs_mobility,'/Users/qidiwang1/Desktop/疫情数据库/rt_vs_mobility.csv')
library(ggplot2)
ggplot()+
  geom_point(aes(x=rt_vs_mobility$mobility,y=rt_vs_mobility$rt))
