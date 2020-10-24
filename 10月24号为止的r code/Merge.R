merge <- read.csv('/Users/qidiwang1/Desktop/Merge/Confirmed.csv')
View(merge)
names(merge)
for (i in 1:ncol(merge)) {
  names(merge)[i]<- 'a'
}


x <- data.frame()
for (i in 4:ncol(merge)){
  a <- merge[,c(1,2,3,i)]
  x <- rbind(x,a)
}
x
write.csv(x, '/Users/qidiwang1/Desktop/Merge/C(adjust).csv')

