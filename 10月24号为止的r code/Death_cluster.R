a <- read.csv('/Users/qidiwang1/Desktop/阿里/国际疫情发展/ncovdata.csv')

View(a)

a <- a[a$Deaths>=5,]



bbbb <- data.frame()
for (i in levels(a[,2])){
  b3 <- data.frame()
  for (j in c(1:nrow(a))) {
    if (a[j,2]==i)
      b3 <- rbind(b3,a[j,])
  }
  
  if (nrow(b3)>=14)
    bbbb <- rbind(bbbb,b3[c(1:14),])
}


write.csv(bbbb,'/Users/qidiwang1/Desktop/a.csv')
bbbb <- read.csv('/Users/qidiwang1/Desktop/a.csv')
View(bbbb)
aaaa <- data.frame(c(1:14))
for (i in levels(bbbb[,3])){
  b3 <- data.frame()
  for (j in c(1:nrow(bbbb))) {
    if (bbbb[j,3]==i)
      b3 <- rbind(b3,bbbb[j,6])
  }
  aaaa <- cbind(aaaa,b3)
}

View(aaaa)

col_name <- data.frame(as.character(c(1:14)))
for (i in levels(bbbb[,3])){
  b3 <- data.frame()
  for (j in c(1:nrow(bbbb))) {
    if (bbbb[j,3]==i)
      b3 <- rbind(b3,as.character(bbbb[j,3]))
  }
  col_name <- cbind(col_name,b3)
}
View(col_name)

for (i in c(1:ncol(col_name))){
  names(aaaa)[i] <- substr(names(col_name)[i],3,nchar(names(col_name)[i])-1)
}

View(aaaa)

cluster_data <- t(aaaa[,-1])

#for (i in c(1:ncol(cluster_data))){
#  names(cluster_data)[i] <- as.character(i)
#}

View(cluster_data)
cluster_data.pca <- prcomp(log(cluster_data), center =TRUE, scale = TRUE)
summary(cluster_data.pca)
res.pca$ind


res.pca <- PCA(log(cluster_data), ncp = 2, graph =TRUE)
res.hcpc <- HCPC(res.pca, graph = FALSE) #You may choose the number of clusters by graoh = 'TRUE' or 'FALSE'
library(factoextra)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

res.hcpc$desc.axes$quanti


####Graph compare
View(aaaa[,-1])
plot_data <- cbind(aaaa[,-1],c(1:21))

all_country_data <- data.frame()
for (i in levels(bbbb[,3])){
  
  b3 <- data.frame()
  b4 <- data.frame()
  for (j in c(1:nrow(bbbb))) {
    if (bbbb[j,3]==i)
      b3 <- rbind(b3,bbbb[j,])
  }
  
  for (z in 1:nrow(b3)){
    b4 <- rbind(b4,cbind(z,b3[z,]))
  }
  all_country_data <- rbind(all_country_data,b4)
}

write.csv(all_country_data,'/Users/qidiwang1/Desktop/b.csv')
all_country_data <- read.csv('/Users/qidiwang1/Desktop/b.csv')
View(all_country_data)
ggplot()+
  geom_line(aes(x=all_country_data[,2],y=all_country_data[,6],group=d[,5]),color = 'azure4')+
  theme(legend.position = 'none')
#scale_y_continuous(limits = c(0, 0.2))+
xlab('Date Starting from 50 Confirmed Cases')+
  ylab('Total Confirmed')

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










