filenames <- list.files('/Users/qidiwang1/Desktop/阿里/国际疫情发展/Google Trend', pattern = '*.csv$', full.names = TRUE)


my_data <- lapply(filenames, read.csv, skip = 2)


for (a in c(1:length(my_data))) {
  if (nrow(my_data[[a]]) == 0)
    my_data[[a]]<-NULL
}
my_data[[59]]<-NULL
my_data[[124]]<-NULL
my_data[[145]]<-NULL
my_data[[123]]<-NULL
my_data[[143]]<-NULL
my_data[[142]]<-NULL

data <- my_data
View(data)

for (a in c(1:length(data))){
  if (nrow(data[[a]])>0) 
    for (i in c(1:nrow(data[[a]]))){
      for (j in c(1:ncol(data[[a]]))){
        if (data[[a]][i,j] == '<1') data[[a]][i,j] <- NA
      }
      
    }
}



for (a in c(1:length(data))){
  data[[a]][,2] <- as.character(data[[a]][,2])
  data[[a]][,3] <- as.character(data[[a]][,3])
  data[[a]][,4] <- as.character(data[[a]][,4])
  data[[a]][,5] <- as.character(data[[a]][,5]) 
  data[[a]][,6] <- as.character(data[[a]][,6])
}
for (a in c(1:length(data))){
  data[[a]][,2] <- as.numeric(data[[a]][,2])
  data[[a]][,3] <- as.numeric(data[[a]][,3])
  data[[a]][,4] <- as.numeric(data[[a]][,4])
  data[[a]][,5] <- as.numeric(data[[a]][,5]) 
  data[[a]][,6] <- as.numeric(data[[a]][,6])
}



flu <- list()
for (a in c(1:length(data))) {
  data[[a]][,2] <- ifelse(is.na(data[[a]][,2]),0,data[[a]][,2])
  data[[a]][,3] <- ifelse(is.na(data[[a]][,3]),0,data[[a]][,3])
  data[[a]][,4] <- ifelse(is.na(data[[a]][,4]),0,data[[a]][,4])
  data[[a]][,5] <- ifelse(is.na(data[[a]][,5]),0,data[[a]][,5])
  data[[a]][,6] <- ifelse(is.na(data[[a]][,6]),0,data[[a]][,6])
  
  data1 <- data[[a]][c(1:91),]
  a1 <- sum(data1[,2])/nrow(data1)
  a2 <- sum(data1[,3])/nrow(data1)
  a3 <- sum(data1[,4])/nrow(data1)
  a4 <- sum(data1[,5])/nrow(data1)
  a5 <- sum(data1[,6])/nrow(data1)
  a6 <- sum(a2,a3,a5)
  if (a1 >0 & a6 >0) {Baseline <- a6/a1
  } else {Baseline <- 0}
  
  let_go <- data[[a]][c(92:nrow(data[[a]])),]
  let_go[,2] <- as.character(let_go[,2])
  let_go[,3] <- as.character(let_go[,3])
  let_go[,4] <- as.character(let_go[,4])
  let_go[,5] <- as.character(let_go[,5]) 
  let_go[,6] <- as.character(let_go[,6])
  let_go[,2] <- as.numeric(let_go[,2])
  let_go[,3] <- as.numeric(let_go[,3])
  let_go[,4] <- as.numeric(let_go[,4])
  let_go[,5] <- as.numeric(let_go[,5]) 
  let_go[,6] <- as.numeric(let_go[,6])
  data2 <- c()
  for (i in 1:(nrow(let_go))){
    if (let_go[i,2] >0 & sum(let_go[i,3],let_go[i,4],let_go[i,6])>0{
      R <- sum(let_go[i,3],let_go[i,4],let_go[i,6])/let_go[i,2] >0
    } else{R <- 0}
    R_Real <- R-Baseline
    data2 <- c(data2,R_Real)
  }
  flu <- append(flu, list(data2))
}
length(flu)
      

library(TSclust)
library(dtw)
library(rJava)
library(dtwclust)


proxy::pr_DB$set_entry(FUN = diss.ACF, names = c("ACFD"),
                       loop = TRUE, type = "metric", distance = TRUE,
                       description = "Autocorrelation-based distance")
# Normalized DTW
ndtw <- function(x, y, ...) {
  dtw(x, y, ...,
      step.pattern = asymmetric,
      distance.only = TRUE)$normalizedDistance
}
# Register the distance with proxy
proxy::pr_DB$set_entry(FUN = ndtw, names = c("nDTW"),
                       loop = TRUE, type = "metric", distance = TRUE,
                       description = "Normalized, asymmetric DTW")
# Partitional clustering
#tsclust(flu, k = 2L,
#        distance = "nDTW", seed = 838)
# Reinterpolate to same length
flu <- reinterpolate(flu, new.length = max(lengths(flu)))

# Calculate the DTW distances between all elements
system.time(D1 <- proxy::dist(flu[1L:5L], flu[6L:100L],
                              method = "dtw_basic",
                              window.size = 20L))
# Nearest neighbors
NN1 <- apply(D1, 1L, which.min)
# Calculate the distance matrix with dtw_lb
system.time(D2 <- dtw_lb(flu[1L:5L], flu[6L:100L],
                         window.size=20L))
# Nearest neighbors
NN2 <- apply(D2, 1L, which.min)
# Same results?
all(NN1 == NN2)

# Exclude a series as an example
database <- flu[-100L]
classify_series <- function(query) {
  d <- dtw_lb(database, query, window.size = 18L, nn.margin = 2L)
  # Return label of nearest neighbor
  CharTrajLabels[which.min(d)]
}
# 100-th series is a Z character
classify_series(flu[50L])  

hc_sbd <- tsclust(flu, type = "h", k = 20L,
                  preproc = zscore, seed = 899,
                  distance = "sbd", centroid = shape_extraction,
                  control = hierarchical_control(method = "average"))
# By default, the dendrogram is plotted in hierarchical clustering
plot(hc_sbd)  

# The series and the obtained prototypes can be plotted too
plot(hc_sbd, type = "sc")  

# Focusing on the first cluster
plot(hc_sbd, type = "series", clus = 1L)
plot(hc_sbd, type = "centroids", clus = 1L)  

library(cluster)
tsclust(flu, type = "h", k = 4L,
        distance = "dtw_basic",
        control = hierarchical_control(method = diana),
        args = tsclust_args(dist = list(window.size = 18L)))

# Reinterpolate to same length
flu <- reinterpolate(flu, new.length = max(lengths(flu)))
length(flu)
# z-normalization
flu <- zscore(flu)
'''pc_dtw <- tsclust(flu, k = 4L,
                  distance = "dtw_basic", centroid = "dba",
                  trace = TRUE, seed = 8,
                  norm = "L2", window.size = 20L,
                  args = tsclust_args(cent = list(trace = TRUE)))
plot(pc_dtw)
#pc_dtwlb <- tsclust(flu, k = 4L,
#                    distance = "dtw_lb", centroid = "dba",
                    trace = TRUE, seed = 8,
                    norm = "L2", window.size = 20L,
                    control = partitional_control(pam.precompute = FALSE),
                    args = tsclust_args(cent = list(trace = TRUE)))
#plot(pc_dtwlb)
#pc_ks <- tsclust(flu, k = 4L,
                 distance = "sbd", centroid = "shape",
                 seed = 8, trace = TRUE)
#plot(pc_ks)
'''
pc_tp <- tsclust(flu, k = 5L, type = "t",
                 seed = 8, trace = TRUE,
                 control = tadpole_control(dc = 1.5,
                                           window.size = 20L))

plot(pc_tp)
b <- c()
for (a in 1:length(data)){
  b <- c(b,substr(names(data[[a]])[2],13,nchar(names(data[[a]])[2])-1))
}
b
result <- data.frame('country'=b,'cluster'=pc_tp@cluster)
result
a1 <- c()
a2 <- c()
a3 <- c()
a4 <- c()
a5 <- c()
for (i in 1:229) {
  if (result[i,2] == 1)
    a1 <- c(a1,as.character(result[i,1]))
  else if (result[i,2]==2)
    a2 <- c(a2,as.character(result[i,1]))
  else if (result[i,2]==3)
    a3 <- c(a3,as.character(result[i,1]))
  else if (result[i,2]==4)
    a4 <- c(a4, as.character(result[i,1]))
  else if (result[i,2]==5)
    a5 <- c(a5, as.character(result[i,1]))
}
a1
a2
a3
a4
a5

write.csv(data.frame('a1'=a1),'/Users/qidiwang1/Desktop/a1.csv')
write.csv(data.frame('a2'=a2),'/Users/qidiwang1/Desktop/a2.csv')
write.csv(data.frame('a3'=a3),'/Users/qidiwang1/Desktop/a3.csv')
write.csv(data.frame('a4'=a4),'/Users/qidiwang1/Desktop/a4.csv')
write.csv(data.frame('a5'=a5),'/Users/qidiwang1/Desktop/a5.csv')




