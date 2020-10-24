data <- read.csv('/Users/qidiwang1/Desktop/阿里/2018-2010/火力发电量.csv')
View(data)
data <- data[c(1:110),c(1:12)]

linear_accumulation <- lm(火力发电量累计增长...~火力发电累计. + Month, data = data)
summary(linear_accumulation)
pred <- predict(linear_accumulation)
data$pred <- pred

linear <- lm(工业增加值累计增长...~pred, data = data)
summary(linear)
pred2 <- predict(linear)
ggplot() +
  geom_line(aes(x = c(1:33), y = data$工业增加值累计增长...), color = 'red') +
  geom_line(aes(x = c(1:33), y = pred2), color = 'green') 



library(ggplot2)
ggplot() +
  geom_line(aes(x = c(1:33), y = data$火力发电量累计增长...), color = 'red') +
  geom_line(aes(x = c(1:33), y = pred), color = 'green') +
  geom_line(aes(x = c(1:33), y = data$火力发电累计.), color = 'yellow') 

testdata <- read.csv('/Users/qidiwang1/Desktop/阿里/2018-2010/Book1.csv')
predict(linear_accumulation, newdata = testdata)
testdata$pred <- predict(linear_accumulation, newdata = testdata)
predict(linear, newdata = testdata)
View(testdata)




cor(pred,data$工业增加值累计增长...)
cor(pred,data$火力发电量累计增长...)
cor(data$火力发电量累计增长...,data$火力发电累计.)
cor(data$火力发电量累计增长...,data$工业增加值累计增长...)
[1] 0.8350074