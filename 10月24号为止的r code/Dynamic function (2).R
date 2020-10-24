START <- 0; FINISH <- 100; STEP <- 1
simtime <- seq(START, FINISH, by = STEP)
stocks <- c(sSuseptible = 60340000, sInfected = 2, sRecovered = 0)
auxs <- c(aTotalPopulation = 60340002, aEffective.Contact.Rate = 2.5, aDelay = 3)
model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)), 
       {
       aBeta <- aEffective.Contact.Rate / aTotalPopulation
       aLambda <- aBeta * sInfected
       fIR <- sSuseptible * aLambda
       fRR <- sInfected / aDelay
       dS_dt <- -fIR
       dI_dt <- fIR - fRR
       dR_dt <- fRR
       
       return (list(c(dS_dt, dI_dt, dR_dt),
               IR = fIR, RR = fRR, Beta = aBeta, Lamda = aLambda,
               CE = aEffective.Contact.Rate))
  
       })
}
library(deSolve)
o <- data.frame(ode(y = stocks, times = simtime, func = model, parms = auxs, method = 'euler'))
o


library(ggplot2)
ggplot() +
  geom_line(data = o, aes(time, o$sSuseptible,color = 'Suseptible')) +
  geom_line(data = o, aes(time, o$sInfected, color = 'Infected')) +
  geom_line(data = o, aes(time, o$sRecovered,color = 'Recovered')) +
  ylab('Population') +
  xlab('Days')

