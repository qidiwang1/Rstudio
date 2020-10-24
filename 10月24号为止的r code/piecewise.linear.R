data <- read.csv('/Users/qidiwang1/Desktop/阿里/消费额/消费额恢复.csv')
View(data)
data <- data[c(1:17),]

library(SiZer)
data$recover <- c(1:17)

ncol(data)-1

for (i in 2:32) {
  x <- data$recover
  y <- data[,i]
  linear <- piecewise.linear(x=x, y=y, middle = 1, CI = FALSE)
  plot <- ggplot() +
    geom_point(aes(x = data$recover, y = data[,i]),
               color = 'red') +
    geom_line(aes(x = data$recover, y = predict(linear,data$recover)),
              color = 'blue') +
    ggtitle(i) +
    xlab('Days') +
    ylab('consumer')
  print(plot)
}

