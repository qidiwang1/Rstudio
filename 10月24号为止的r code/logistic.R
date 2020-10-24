data <- read.csv('/Users/qidiwang1/Desktop/阿里/province_nCoV_20200210.csv')
data_china <- data[data$Partner.Name == 'China', c(3,6,7)]
data_china




library(ggplot2)
library(caret)
library(propagate)
compare_logistics <- function(x, y, k1, a1, tm1) {
  
  logistic <- nls(y ~ ( k1 / (1 + exp( (log(81)/-a1) * (x - tm1)))),   start = c(k1=k1,a1=a1,tm1=tm1))
  newx  <- seq(1,100,length=100)
  pred_y <- predict(logistic, newdata = data.frame(x=newx))
  prednls <- predictNLS(logistic, newdata = data.frame(x=newx))
  result1 <- summary(logistic)
  return(list((ggplot() +
                 geom_point(aes(x = x, y = y), color = 'red') +
                 geom_line(aes(x = pred_x$Day, y = pred_y), color = 'blue') +
                 geom_line(aes(x = newx, y = prednls$summary[,6]), color = 'green') +
                 ggtitle('People vs Days') +
                 xlab('Days') +
                 ylab('People')),result1,AIC(logistic)))
  
}



for (i in 2:ncol(data)) {
  tryCatch({
    y <- data[,i]
    result <- compare_logistics(x,y,100,5,20)
    print(result)
  }, error = function(e){cat("ERROR:", conditionMessage(e),"\n")})
}


#DB
library(ifultools)
decibel(pred_y,type = 1)

#Bilogistic and trilogistic
compare_bilogistics <- function(x, y, k1, a1, tm1, k2, a2, tm2) {
  bilogistic <- nls(y ~  (( k1 / (1 + exp( (log(81)/-a1) * (x - tm1)))) +
                            ( k2 / (1 + exp( (log(81)/-a2) * (x - tm2))))),
                    start=c(a1=a1,k1=k1,tm1=tm1,a2=a2,k2=k2,tm2=tm2))
  result <- data.frame('bilogistic_AIC' = AIC(bilogistic))  
  return(result)
}
compare_bilogistics(x,y,1300,5,15,1300,5,21)

compare_trilogistics <- function(x, y, k1, a1, tm1, k2, a2, tm2, k3, a3, tm3) {
  trilogistic <- nls(y ~  (( k1 / (1 + exp( (log(81)/-a1) * (x - tm1)))) +
                             ( k2 / (1 + exp( (log(81)/-a2) * (x - tm2)))) +
                             (k3 / (1 + exp( (log(81)/-a3) * (x - tm3))))),
                     start=c(a1=a1,k1=k1,tm1=tm1,a2=a2,k2=k2,tm2=tm2,a3=a3,k3=k3,tm3=tm3))
  result <- data.frame('trilogistic_AIC' = AIC(trilogistic))  
  return(result)
}




#Monte-Carlo
library(propagate)
y <- data$Hubei
x <- data$Day

newx  <- seq(1,100,length=100)

logistic <- nls(y ~ ( k1 / (1 + exp( (log(81)/-a1) * (x - tm1)))),   start = c(k1=100,a1=5,tm1=20))

prednls <- predictNLS(logistic, newdata = data.frame(x=newx))
summary(prednls$summary[,1])
lines(newx,prednls$summary[,6])

newx  <- seq(1,100,length=100)
pred_y <- predict(logistic)

data1 <- factor(data[1:14,2])
data1
data2 <- factor(pred_y[1:14])
data2

sensitivity(data1,data2)

library(ifultools)
decibel(pred_y,type = 1)












compare_bilogistics <- function(x, y, k1, a1, tm1, k2, a2, tm2) {
bilogistic <- nls(y ~  (( k1 / (1 + exp( (log(81)/-a1) * (x - tm1)))) +
                          ( k2 / (1 + exp( (log(81)/-a2) * (x - tm2))))),
                  start=c(a1=a1,k1=k1,tm1=tm1,a2=a2,k2=k2,tm2=tm2))
result <- data.frame('bilogistic_AIC' = AIC(bilogistic))  
return(result)
}
compare_bilogistics(x,y,1300,5,15,1300,5,21)

compare_trilogistics <- function(x, y, k1, a1, tm1, k2, a2, tm2, k3, a3, tm3) {
trilogistic <- nls(y ~  (( k1 / (1 + exp( (log(81)/-a1) * (x - tm1)))) +
                           ( k2 / (1 + exp( (log(81)/-a2) * (x - tm2)))) +
                           (k3 / (1 + exp( (log(81)/-a3) * (x - tm3))))),
                   start=c(a1=a1,k1=k1,tm1=tm1,a2=a2,k2=k2,tm2=tm2,a3=a3,k3=k3,tm3=tm3))
result <- data.frame('trilogistic_AIC' = AIC(trilogistic))  
return(result)
}
#compare_trilogistics(x,y,900,4,10,1600,4,15,1300,4,20)
#compare_trilogistics(x,y,941,4.06,15.7,1638,3.97,22.7,1265,2.3,36.9)
compare_trilogistics(x,y,1568.49,4.5,12.1568,49,4.5,20,1568.49,4.5,32)


summary(logistic)
#plot(data$days,data$height)
newx  <- seq(1,50,length=50)
pred_x <- data.frame('days' = newx)

pred_y <- predict(logistic,newdata=newx)
pred_y
library(ggplot2)
ggplot() +
  geom_point(aes(x = data$days, y = data$height),
             color = 'red') +
  geom_line(aes(x = pred_x$days, y = pred_y),
            color = 'blue') +
  ggtitle('People vs Days') +
  xlab('Days') +
  ylab('People')

AIC(logistic)



