data1 <- read.csv('/Users/qidiwang1/Desktop/阿里/迁入与迁出/省份城市大全.csv')
data2 <- read.csv('/Users/qidiwang1/Desktop/阿里/迁入与迁出/迁出20200303.csv')
data1 <- data1[,1:4]
View(data1)
View(data2)


province <- c()
for (i in data2$city) {
  for (j in data1$area_name) 
    if (j==i) province <- c(province, data1[which(grepl(j,data1$area_name)),2])
}


city <-c()
for (i in levels(data2$city)) {
  if (sum(i==levels(data1$area_name))==1) city <- c(city,i)
}


levels(province)

data2$prov <- province

con<-file('/Users/qidiwang1/Desktop/阿里/迁入与迁出/final.csv',encoding="UTF-8")
write.csv(...,file=con,...)


write.csv(data2,'/Users/qidiwang1/Desktop/阿里/迁入与迁出/final.csv', fileEncoding = 'UTF-8')
library(xlsx)
write.xlsx(data2, '/Users/qidiwang1/Desktop/阿里/迁入与迁出/final2.xlsx')

write.table(data2, '/Users/qidiwang1/Desktop/阿里/迁入与迁出/final2.csv')


province <- c(province, data1[which(grepl(j,data1$area_name)),2])
print(province)


data3 <- read.csv('/Users/qidiwang1/Desktop/阿里/迁入与迁出/ffinal.csv')
View(data3)

num1 <- c()
num2 <- c()

levels(factor(data3$date))
levels(factor(data3$prov))
factor(data3$date)

date <- c()
for (i in levels(factor(data3$date))) {
  for (j in data3$date)
    if (i==j) print(j)
}

num <- c()
if (data3$date == 20190112 & data3$prov == '云南省') {
  num <- c(num, data3[which(grepl(j,data3$area_name)),3])

}

which(grepl(20190112,data3$date)) = which(grepl('云南省',data3$prov))

for (i in levels(data3$prov)) {
  for (a in data3$prov
    for (j in levels(factor(data3$date)))
      for (z in data3$date)
        if (i == a & j == z)
}
