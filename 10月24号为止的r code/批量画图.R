a <- read.csv('/Users/qidiwang1/Desktop/Book25.csv')



a <- a[a$Confirmed >=100,]

a <- na.omit(a)

bbbb <- data.frame()
for (i in levels(a[,2])){
  b3 <- data.frame()
  b4 <- data.frame()
  for (j in c(1:nrow(a))) {
    if (a[j,2]==i)
      b3 <- rbind(b3,a[j,])
  }
  if (nrow(b3)>0)
    for (z in 1:nrow(b3)){
      b4 <- rbind(b4,cbind(z,b3[z,]))
    }
  bbbb <- rbind(bbbb,b4)
}

View(bbbb)

coulmn_num <- 14

#write.csv(bbbb,'/Users/qidiwang1/Desktop/C.csv')
bbbb[,1] <- as.character(bbbb[,1])
bbbb[,1] <- as.numeric(bbbb[,1])
bbbb[,coulmn_num] <- as.numeric(as.character(bbbb[,coulmn_num]))
write.csv(bbbb,'/Users/qidiwang1/Desktop/cc.csv')
d <- read.csv('/Users/qidiwang1/Desktop/cc.csv')
d <- d[,-1]
View(d)


p <- ggplot()+
  geom_line(aes(x=d[,1],y=d[,coulmn_num],group=d[,3]),color = 'azure4')+
  theme(legend.position = 'none')+
  #scale_y_continuous(limits = c(0, 0.2))+
  xlab('Date Starting from 100 Confirmed Cases')+
  ylab('Death Growth Rate')

for (i in levels(d[,3])){
  b <- data.frame()
  for (j in c(1:nrow(d))) {
    if (d[j, 3] == i)
      b <- rbind(b,d[j,])
  }
  plot <- p + geom_line(aes(x=b[,1],y=b[,coulmn_num]),color = 'red', size = 2) +
    ggtitle(b[1,3])
  ggsave(plot,filename = paste('/Users/qidiwang1/Desktop/Log New Death/',b[1,3],'.png'))
}

