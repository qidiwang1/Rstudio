v <- c()
s <- list(id = '', name = '')
which(v1 %% 2 == 0)
seq(from =0, to = 5, by = 0.5)
any(v <1)
all(v >0)
m <- matrix(c(1,2,3,4,5,6),nrow = 3, ncol = 2)
dim(m)
rowSums()
d< -sapply(data, function(e,m)(e-m), mean(data))
library(deSolve)
model <- function(time,stocks, auxs) {
  with(as.list(c(stocks,auxs)), {
    fRecruits <- sCustomers * aGrowthFraction
    fLosses <- sCustomers * aDeclineFraction
    dc_dt <- fRecruits - fLosses
    return(list(c(dc_dt),
                Recruits = fRecruits, Losses = fLosses,
                GF = aGrowthFraction, DF = aDeclineFraction))
  }
}

o <- data.frame(ode(y=stocks, time = simtime, func = model, parms = auxs, method = 'euler'))

library(ggplot2)
ggplot() +
  geom_line(data = o, aes(time, 0$Customers), color = 'blue') +
  geom_point(data = o, aes(time, 0$Customers), color = 'blue') +
  scale_y_continuous(labels = comma) +
  ylab('Customers') +
  xlab('Year')
  
  
  
  
  
  
  