data <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/聚类分析.csv')
library(smoother)
library(dplyr)
cluster_data <- data.frame()
for (a in levels(data$Country.x)) {
  country <- data[data$Country.x==a,]
  smooth_cases <- smth(country$New.cases,mehtod = 'gaussian')
  smooth_death <- smth(country$New.death,method = 'gaussian')
  cluster_data <- rbind(cluster_data,cbind(country,smooth_cases,smooth_death))
}
write.csv(cluster_data, '/Users/qidiwang1/Desktop/cluster_data.csv')


cluster_data <- read.csv('/Users/qidiwang1/Desktop/cluster_data.csv')
cluster_data$Date <- as.Date(cluster_data$Date,'%m/%d/%y')
cluster_data <- na.omit(cluster_data)

cluster_data <- cluster_data[cluster_data$confirm>50,]

#reverse_data <- data.frame(Date=cluster_data[cluster_data$Country.x=='china',]$Date)


cluster_data$Country.x <- factor(cluster_data$Country.x)

reverse_data <- data.frame(c(1:20))
for (a in levels(cluster_data$Country.x)){
  country <- data.frame(cluster_data[cluster_data$Country.x==a,12])
  names(country) <- a
  if (nrow(country) >=20)
    bbb <- country[c((nrow(country)-19):nrow(country)),]
  
    reverse_data <- cbind(reverse_data,bbb)
    names(reverse_data)[ncol(reverse_data)] <- a

}



cluster_data <- t(reverse_data[,-1])


#for (i in c(1:ncol(cluster_data))){
#  names(cluster_data)[i] <- as.character(i)
#}


cluster_data.pca <- prcomp(cluster_data, center =TRUE, scale = TRUE)
summary(cluster_data.pca)


library(FactoMineR)
res.pca <- PCA(cluster_data, ncp = 5, graph =TRUE)
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










