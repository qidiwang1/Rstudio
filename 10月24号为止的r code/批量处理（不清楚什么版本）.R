filenames <- list.files('/Users/qidiwang1/Desktop/Google Trend', pattern = '*.csv$', full.names = TRUE)


my_data <- lapply(filenames, read.csv, skip = 2)

for (a in c(1:length(my_data))){
  if (nrow(my_data[[a]])==0)
    my_data[[a]] <- NULL
}


View(my_data)

  
for (a in c(1:length(my_data))){
  if (nrow(my_data[[a]])>0)
    for (i in c(1:nrow(my_data[[a]]))){
      for (j in c(1:ncol(my_data[[a]]))){
        if (my_data[[a]][i,j] == '<1') my_data[[a]][i,j] <- NA
      }
    }

}

x <- data.frame()
y <- c()
for (a in c(1:length(my_data))){
  my_data[[a]][,2] <- as.numeric(as.character(my_data[[a]][,2]))
  my_data[[a]][,3] <- as.numeric(as.character(my_data[[a]][,3]))
  my_data[[a]][,4] <- as.numeric(as.character(my_data[[a]][,4]))
  my_data[[a]][,5] <- as.numeric(as.character(my_data[[a]][,5])) 
  my_data[[a]][,6] <- as.numeric(as.character(my_data[[a]][,6]))
  y <- c(y,names(my_data[[a]])[2])
  data1 <- my_data[[a]][c(1:91),]

  a1 <- sum(data1[,2,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a2 <- sum(data1[,3,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a3 <- sum(data1[,4,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a4 <- sum(data1[,5,drop = FALSE],na.rm = TRUE)/nrow(data1)
  a5 <- sum(data1[,6,drop = FALSE],na.rm = TRUE)/nrow(data1)
  if (a1 > 0 & a2 > 0) Baseline <- a2/a1
  else Baseline <- 0
  
  data2 <- my_data[[a]][c(91:176),]
  add <- data.frame('y'= 2)

  for (i in c(1:85)) {
    R <- data2[i,3]/data2[i,2]
    R_Real <- R-Baseline
    add <- rbind(add, R_Real)
      }
  x <- cbind(x,add)
}

my_data[[1]][,2] <- as.numeric(as.character(my_data[[a]][,2]))
my_data[[1]][,3] <- as.numeric(as.character(my_data[[a]][,3]))
my_data[[1]][,4] <- as.numeric(as.character(my_data[[a]][,4]))
my_data[[1]][,5] <- as.numeric(as.character(my_data[[a]][,5])) 
my_data[[1]][,6] <- as.numeric(as.character(my_data[[a]][,6]))
data1 <- my_data[[1]][c(1:91),]
a1 <- sum(data1[,2,drop = FALSE],na.rm = TRUE)/nrow(data1)
a2 <- sum(data1[,3,drop = FALSE],na.rm = TRUE)/nrow(data1)
a3 <- sum(data1[,4,drop = FALSE],na.rm = TRUE)/nrow(data1)
a4 <- sum(data1[,5,drop = FALSE],na.rm = TRUE)/nrow(data1)
a5 <- sum(data1[,6,drop = FALSE],na.rm = TRUE)/nrow(data1)
if (a1 > 0 & a2 > 0) 
  Baseline <- a2/a1 
else
  Baseline <- 0

data2 <- my_data[[1]][c(91:176),]
b1 <- data.frame('y'= 2)

for (i in c(1:85)) {
  R <- data2[i,3]/data2[i,2]
  R_Real <- R-Baseline
  b1 <- rbind(new, R_Real)
}
print(b1)
  

a1 <- sum(data1[,2,drop = FALSE],na.rm = TRUE)/nrow(data1)







View(x)

write.csv(add,'/Users/qidiwang1/Desktop/Google Trend/Italy.csv')

a <- names(my_data[[1]])[2]
data.frame('a'=2)

x$names(my_data[[1]])[2] <- 2 
data.frame(paste(names(my_data[[1]])[2],'y') = numeric)
paste(names(my_data[[1]])[2],'y')

data.frame('y' = 2)
nrow(my_data[[1]])

my_data[[1]][,2] <- as.numeric(as.character(my_data[[a]][,2]))
my_data[[1]][,3] <- as.numeric(as.character(my_data[[a]][,3]))
my_data[[1]][,4] <- as.numeric(as.character(my_data[[a]][,4]))
my_data[[1]][,5] <- as.numeric(as.character(my_data[[a]][,5])) 
my_data[[1]][,6] <- as.numeric(as.character(my_data[[a]][,6]))
sum(c(my_data[[1]][1,3],my_data[[1]][1,4],my_data[[1]][1,5],my_data[[1]][1,6]),na.rm=TRUE)/my_data[[1]][1,2]
my_data[[1]]

write.csv(x,'/Users/qidiwang1/Desktop/add.csv')


View(x)



