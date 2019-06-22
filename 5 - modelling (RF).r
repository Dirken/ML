library(data.table)
library(randomForest)
load('data/train.rds')
load('data/val.rds')
load('data/test.rds')

mu <- colMeans(train.data[, .(x0, y0, x1, y1, distance)])
sigma <- apply(train.data[, .(x0, y0, x1, y1, distance)], 2, sd)

train.data <- data.table(
  scale(train.data[, .(x0, y0, x1, y1, distance)], mu, sigma),
  duration=train.data$duration)
val.data <- data.table(
  scale(val.data[, .(x0, y0, x1, y1, distance)], mu, sigma),
  duration=val.data$duration)
test.data <- data.table(
  scale(test.data[, .(x0, y0, x1, y1, distance)], mu, sigma),
  duration=test.data$duration)
rm(mu, sigma); gc()


set.seed(7777)
rf <- randomForest(duration ~ ., data=train.data, ntree=1000, mtry=5, replace=FALSE, sampsize=10000)

train.pred <- predict(rf, train.data)
train.e <- train.pred - train.data$duration
train.mae <- mean(abs(train.e))
# 3.77

val.pred <- predict(rf, val.data)
val.e <- val.pred - val.data$duration
val.mae <- mean(abs(val.e))
# 3.82

test.pred <- predict(rf, test.data)
test.e <- test.pred - test.data$duration
test.mae <- mean(abs(test.e))
# 3.81
