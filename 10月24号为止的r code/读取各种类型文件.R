library(rJava)
library(xlsx)
my_data <- data.table(file = '/Users/qidiwang1/Desktop/YADONG/household debt.xls')
library(readxl)
my_data <- read_excel('/Users/qidiwang1/Desktop/YADONG/household debt.xls')
View(my_data)
typeof(my_data$time)
typeof(my_data$y)

my_data <- read.xlsx('/Users/qidiwang1/Desktop/YADONG/household debt.xls',1)
View(my_data)
typeof(my_data$time)
typeof(my_data$y)

#1、读取xlsx中所有的sheet表格
#如果像vector一样定义List？？——list()函数来主动定义，用data.list[[i]]来赋值
data.list<-list()
for (i in 1:2){
  data.list[[i]]=read.xlsx("C1.xlsx",i)
}
————————————————
版权声明：本文为CSDN博主「悟乙己」的原创文章，遵循 CC 4.0 BY-SA 版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/sinat_26917383/article/details/51100736

#3、利用List批量读出操作
#难点：如果构造输出表格的名称——paste来构造名称
flie=list()
xlsxflie=paste(1:2,".xlsx",sep="")

for(i in 1:2){
  flie[[i]]=paste("C:/Users/long/Desktop/",xlsxflie[i],sep="")
  write.xlsx(data.list2[[i]],file)
}
————————————————
版权声明：本文为CSDN博主「悟乙己」的原创文章，遵循 CC 4.0 BY-SA 版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/sinat_26917383/article/details/51100736

#csv
filenames <- list.files('/Users/qidiwang1/Desktop/Google Trend/', pattern = '*.csv$', full.names = TRUE)
files=list()
for (i in 1:3){
  files[[i]]=read.csv(filenames[[i]],skip = 2)
}

#SECOND WAY
my_data <- lapply(filenames, read.csv, skip = 2)
View(my_data)

for (i in c(2:5)){
  
}

filenames

#excel
dir('/Users/qidiwang1/Desktop/Google Trend',pattern = "csv")
list.files('C:\\Users\\long\\Desktop\\',pattern = "txt$")

