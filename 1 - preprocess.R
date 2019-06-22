library(data.table)

# Load train data
train <- fread('data/train.csv')

##### Pre-processing + Feature Selection / Extraction

# 1) Keep only geographical information
train$id <- NULL
train$vendor_id <- NULL
train$pickup_datetime <- NULL
train$passenger_count <- NULL
train$store_and_fwd_flag <- NULL

# 2) "dropoff_datetime" and "trip_duration" 
#   are the variables to predict. Since we
#   will focus on "trip_duration" as a target,
#   we will remove "dropoff_datetime". Also,
#   round duration to minutes.
train$dropoff_datetime <- NULL
train$trip_duration <- round(train$trip_duration/60)

# 3) Outlier coordinades should be removed
# We can consider removing arround 1000 points (0.06%)

X <- cbind(train$pickup_longitude, train$pickup_latitude)
M <- colMeans(X)
dist <- apply(X, 1, function(x) crossprod(x - M))
out.500.pick <- sort(dist, decreasing=T, index.return=T)$ix[1:500]

X <- cbind(train$dropoff_longitude, train$dropoff_latitude)
M <- colMeans(X)
dist <- apply(X, 1, function(x) crossprod(x - M))
out.500.drop <- sort(dist, decreasing=T, index.return=T)$ix[1:500]

out.500 <- union(out.500.pick, out.500.drop)
length(out.500) # Finally 837 points

train <- train[-out.500,]

rm(X, dist, M, out.500, out.500.drop, out.500.pick)

# 6) There are also outliers in the duration

100*sum(train$trip_duration > 60*2)/nrow(train)
# 0.1% has more than 2h trip!
100*sum(train$trip_duration > 60)/nrow(train)
# 0.8% has more than 1h trip

train <- train[train$trip_duration <= 60,]


# Re-arrange and re-name variables of the dataset
names(train) <- c('x0', 'y0', 'x1', 'y1', 'duration')

cl <- parallel::makeCluster(parallel::detectCores())
train$distance <- parallel::parApply(cl, train, 1, function(x) geosphere::distm(x[1:2], x[3:4]))
parallel::stopCluster(cl)
rm(cl)
train$distance <- round(train$distance)/1000

train <- train[,c('x0', # pickup longitude
                'y0', # pickup latitude
                'x1', # dropoff longitude
                'y1', # dropoff latitude
                'distance', # distance between (x0, y0) and (x1, y1)
                'duration' # target variable
)]

# NOTE: Data comes without standarization
#   for further visualization.
# If required, it should be done by the
#   learning algoritms

set.seed(1234)
s <- sample(nrow(train), 0.4*nrow(train))
train.data <- train[s,]
test.data <- train[-s,]
s <- sample(nrow(test.data), 0.5*nrow(test.data))
val.data <- test.data[s,]
test.data <- test.data[-s,]

save(train.data, file='data/train.rds')
save(test.data, file='data/test.rds')
save(val.data, file='data/val.rds')
