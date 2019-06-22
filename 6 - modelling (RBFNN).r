library(data.table)
load('data/train.rds')
load('data/test.rds')
load('data/val.rds')


g.range <- (1:10)/10

PHI <- function (x, m, g) {
  Z <- pracma::distmat(as.matrix(x), as.matrix(m))
  as.data.table(exp(-g*Z^2))
}

set.seed(7777)
M <- 500
m <- as.data.table(kmeans(train.data[, .(x0, y0, x1, y1)], M, iter.max=100)$centers)
m$distance <- apply(m, 1, function(x) geosphere::distm(x[1:2], x[3:4]))
m$distance <- round(m$distance)/1000

mu <- colMeans(train.data[, .(x0, y0, x1, y1, distance)])
sigma <- apply(train.data[, .(x0, y0, x1, y1, distance)], 2, sd)

m <- as.data.table(scale(m, mu, sigma))

train.data <- data.table(
  scale(train.data[, .(x0, y0, x1, y1, distance)], mu, sigma),
  duration=train.data$duration)
val.data <- data.table(
  scale(val.data[, .(x0, y0, x1, y1, distance)], mu, sigma),
  duration=val.data$duration)
test.data <- data.table(
  scale(test.data[, .(x0, y0, x1, y1, distance)], mu, sigma),
  duration=test.data$duration)
rm(mu, sigma, M); gc()

memory.limit(1.5e10)

val.mae <- sapply(g.range, function(g) {
  cat('Training with value g =', g, '...\n')
  P.train <- PHI(train.data[, .(x0, y0, x1, y1, distance)], m, g)
  P.train[, duration := train.data$duration]
  cat('P.train built.\n')
  model <- lm(duration ~ ., P.train)
  cat('Model trained.\n')
  rm(P.train); gc()
  P.val <- PHI(val.data[, .(x0, y0, x1, y1, distance)], m, g)
  cat('P.val built.\n')
  pred <- predict(model, P.val)
  rm(model, P.val); gc()
  mae <- mean(abs(pred - val.data$duration))
  cat('Current mae:', mae, '\n')
  mae
})

plot(g.range, val.mae, 'b',
     main='RBFNN tuning',
     xlab='Decay rate',
     ylab='Mean Absolute Error (MAE)')

ix <- which.min(val.mae)
g <- g.range[ix]
# Optimal g: 0.04


P.train <- PHI(train.data[, .(x0, y0, x1, y1, distance)], m, g)
P.train[, duration := train.data$duration]
model <- lm(duration ~ ., P.train)
pred <- predict(model, P.train)
mae.train <- mean(abs(pred - P.train$duration))
rm(P.train, pred); gc()
# 3.82

P.val <- PHI(val.data[, .(x0, y0, x1, y1, distance)], m, g)
pred <- predict(model, P.val)
mae.val <- mean(abs(pred - val.data$duration))
rm(P.val, pred); gc()
# 3.80

P.test <- PHI(test.data[, .(x0, y0, x1, y1, distance)], m, g)
pred <- predict(model, P.test)
mae.test <- mean(abs(pred - test.data$duration))
rm(P.test, pred); gc()
# 3.80
