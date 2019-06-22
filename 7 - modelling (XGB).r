library(data.table)
library(xgboost)
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

train.mat <- data.matrix(train.data[, .(x0, y0, x1, y1, distance)])
train.mat <- xgb.DMatrix(data=train.mat, label=train.data$duration)
val.mat <- data.matrix(val.data[, .(x0, y0, x1, y1, distance)])
val.mat <- xgb.DMatrix(data=val.mat, label=val.data$duration)
test.mat <- data.matrix(test.data[, .(x0, y0, x1, y1, distance)])
test.mat <- xgb.DMatrix(data=test.mat, label=test.data$duration)


eta.range <- (1:10)/10
depth.range <- (1:10)*2
grid <- expand.grid(eta=eta.range, depth=depth.range)

grid$mae <- apply(grid, 1, function(r) {
  set.seed(7777)
  model <- xgboost(data=train.mat,
                   max.depth=r[2], eta=r[1],
                   nthread=8, nrounds=10)
  val.pred <- predict(model, val.mat)
  val.e <- val.pred - val.data$duration
  mean(abs(val.e))
})

col <- c(1:8, 'purple', 'orange')
plot(NULL,
     xlim=c(0, 1), ylim=c(3, 6),
     main='XGBoost tuning',
     xlab='eta',
     ylab='Mean Absolute Error (MAE)')
step <- length(eta.range)
for (i in 1:length(depth.range)) {
  r <- grid[(step*(i-1)+1):(step*i),]
  points(r$eta, r$mae, 'b', col=col[i])
}
legend(x='topright', legend=paste('Max Depth:', depth.range), fill=col)

ix <- which.min(grid$mae)
eta <- grid$eta[ix]
depth <- grid$depth[ix]

set.seed(7777)
model <- xgboost(data=train.mat,
                 max.depth=depth, eta=eta,
                 nthread=8, nrounds=10)

train.pred <- predict(model, train.mat)
train.e <- train.pred - train.data$duration
train.mae <- mean(abs(train.e))
# 3.33

val.pred <- predict(model, val.mat)
val.e <- val.pred - val.data$duration
val.mae <- mean(abs(val.e))
# 3.69

test.pred <- predict(model, test.mat)
test.e <- test.pred - test.data$duration
test.mae <- mean(abs(test.e))
# 3.69
