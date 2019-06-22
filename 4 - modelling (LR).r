library(data.table)
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

# Linear Regression
model <- lm(duration ~ ., data=train.data)

train.pred <- predict(model, train.data)
train.e <- train.pred - train.data$duration
train.mae <- mean(abs(train.e))
# 4.56

val.pred <- predict(model, val.data)
val.e <- val.pred - val.data$duration
val.mae <- mean(abs(val.e))
# 4.55

test.pred <- predict(model, test.data)
test.e <- test.pred - test.data$duration
test.mae <- mean(abs(test.e))
# 4.54

# No overfit at all!
# In fact, the model is probably underfitting the problem
# Therefore, no regularization is tried
