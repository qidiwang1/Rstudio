recover <- read.csv('/Users/qidiwang1/Desktop/疫情数据库/recover.csv')



doc <- data.frame()
for (i in c(1:nrow(recover))) {
  first_merge <- data.frame()
  for (j in 1:(ncol(recover)-1)){
    time <- as.Date('2020-01-21')+j
    b <- cbind(as.character(time),as.character(recover[i,1]), recover[i,(j+1)])
    first_merge <- rbind(first_merge,b)
  }
  doc <- rbind(doc,first_merge)
}

View(doc)
write.csv(doc,'/Users/qidiwang1/Desktop/a.csv')
