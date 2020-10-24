data <- read.csv('province_nCoV_20200210.csv')

library(ggplot2)
library(caret)
library(propagate)
compare_logistics <- function(x, y, k1, a1, tm1) {
  
  logistic <- nls(y ~ ( k1 / (1 + exp( (log(81)/-a1) * (x - tm1)))),   start = c(k1=k1,a1=a1,tm1=tm1))
  newx  <- seq(1,100,length=100)
  pred_y <- predict(logistic, newdata = data.frame(x=newx))
  #prednls <- predictNLS(logistic, newdata = data.frame(x=newx))
  result1 <- summary(logistic)
  return(AIC(logistic))
  
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

