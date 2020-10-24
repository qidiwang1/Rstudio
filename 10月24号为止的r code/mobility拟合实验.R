graph <- read.csv('/Users/qidiwang1/Desktop/g&a.csv')
library(ggplot2)
for (a in levels(graph$iso3_name)){
  country <- graph[graph$iso3_name==a,]
  p <- ggplot()+
    geom_line(aes(x=country$index_dt,y=country$google_7d,color='google'))+
    geom_line(aes(x=country$index_dt,y=country$apple_7d,color='apple'))+
    xlab('Date')+
    ylab('Index')+
    ggtitle(as.character(a))
  ggsave(p, filename = paste('/Users/qidiwang1/Desktop/google & apple/',as.character(a),'.png'))
}


bel <- graph[graph$iso3_name=='BEL',]

library(xgboost)
input <- as.matrix(bel[,c('index_dt','apple_7d')])
output <- as.matrix(bel$google_7d)
bst <- xgboost(data = input, label = output, max.depth = 2,
               eta = 1, nthread = 2, nround = 2, objective = "reg:logistic")
pred <- predict(bst, input)
bel$pred <- pred
ggplot()+
  geom_line(aes(x=bel$index_dt,y=bel$google_7d,color = 'google'))+
  geom_line(aes(x=bel$index_dt,y=bel$apple_7d,color = 'apple'))+
  geom_line(aes(x=bel$index_dt,y=bel$pred, color = 'pred'))

library(randomForest)  
rf <- randomForest(google_7d~apple_7d+index_dt, data = bel)
pred <- predict(rf)
bel$pred <- pred
ggplot()+
  geom_line(aes(x=bel$index_dt,y=bel$google_7d,color = 'google'))+
  geom_line(aes(x=bel$index_dt,y=bel$apple_7d,color = 'apple'))+
  geom_line(aes(x=bel$index_dt,y=bel$pred, color = 'pred'))


for (a in levels(graph$iso3_name)){
  country <- graph[graph$iso3_name==a,]
  rf <- randomForest(google_7d~apple_7d+index_dt, data = country)
  pred <- predict(rf)
  country$pred <- pred
  p <- ggplot()+
    geom_line(aes(x=country$index_dt,y=country$google_7d,color='google'))+
    geom_line(aes(x=country$index_dt,y=country$apple_7d,color='apple'))+
    geom_line(aes(x=country$index_dt,y=country$pred, color = 'pred'))+
    xlab('Date')+
    ylab('Index')+
    ggtitle(as.character(a))
  ggsave(p, filename = paste('/Users/qidiwang1/Desktop/google & apple/',as.character(a),'.png'))
}
