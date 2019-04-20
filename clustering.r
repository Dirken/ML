load('processed.rds')

# Clustering by coordinates

colors <- c('red', 'green', 'blue',
            'purple', 'orange', 'brown',
            'yellow', 'black', 'grey', 'pink',
            'darkred', 'darkgreen', 'darkblue')

xy0 <- data[,c('x0', 'y0')]
xy1 <- data[,c('x1', 'y1')]

k <- 10

cl0 <- kmeans(xy0, k)
plot(xy0, col=colors[cl0$cluster],
     xlim=c(-74.2, -73.7), ylim=c(40.5, 41),
     xlab='x', ylab='y',
     main='K-means clustering (pickup)')
legend('topright',
       legend=paste('cluster', 1:k),
       fill=colors[1:k])

plot(factor(cl0$cluster), data$duration,
     xlab='cluster', ylab='duration',
     main='Duration by cluster (pickup)')


cl1 <- kmeans(xy1, k)
plot(xy1, col=colors[cl1$cluster],
     xlim=c(-74.2, -73.7), ylim=c(40.5, 41),
     xlab='x', ylab='y',
     main='K-means clustering (dropoff)')
legend('topright',
       legend=paste('cluster', 1:k),
       fill=colors[1:k])

plot(factor(cl1$cluster), data$duration,
     xlab='cluster', ylab='duration',
     main='Duration by cluster (dropoff)')

########################### PICKUP HIERARCHICAL ###########################
set.seed(1234)
km <- kmeans(xy0, 10000)
save(km, file='km0.cl')

# load('km0.cl')
plot(km$centers,
     xlim=c(-74.2, -73.7), ylim=c(40.5, 41),
     xlab='x', ylab='y',
     main='Sample of data (pickup)')

ss <- km$centers

h1 <- hclust(dist(ss, method='euclidean'))
plot(h1)
cl <- cutree(h1, h=0.15)
table(cl)
xycl <- list(ss[cl==3,])
xycl <- c(xycl, list(ss[cl==5,]))
xycl <- c(xycl, list(ss[cl==6,]))
xycl <- c(xycl, list(ss[cl== 2 | cl==4 | cl>=7,]))
ss <- ss[cl==1,]

h1 <- hclust(dist(ss, method='euclidean'))
plot(h1)
cl <- cutree(h1, h=0.07)
table(cl)
xycl <- c(xycl, list(ss[cl==1,]))
xycl <- c(xycl, list(ss[cl==2,]))
xycl <- c(xycl, list(ss[cl==3,]))
xycl <- c(xycl, list(ss[cl==4,]))
xycl <- c(xycl, list(ss[cl==5,]))
xycl <- c(xycl, list(ss[cl>=6,]))

plot(NULL,
     xlim=c(-74.2, -73.7), ylim=c(40.5, 41),
     xlab='x', ylab='y',
     main='Hierarquical Clustering (pickup)')

for (i in 1:length(xycl))
  points(xycl[[i]], col=colors[i])
legend('topright',
       legend=paste('cluster', 1:length(xycl)),
       fill=colors[1:length(xycl)])

for (i in 1:length(xycl)) {
  xycl[[i]] <- cbind(xycl[[i]], i)
}
xycl <- Reduce(rbind, xycl)
colnames(xycl) <- c('x0', 'y0', 'cluster')

ix <- as.integer(row.names(xycl))
mean.durs <- sapply(1:10000, function(i) round(mean(data[km$cluster == i, 'duration'])))
xycl <- cbind(xycl, mean.durs[ix])
plot(factor(xycl[,3]), xycl[,4],
     xlab='cluster', ylab='duration',
     main='Duration by cluster (pickup)')

########################### PICKUP HIERARCHICAL ###########################

########################### DROPOFF HIERARCHICAL ###########################
set.seed(1234)
km <- kmeans(xy1, 10000)
save(km, file='km1.cl')

# load('km1.cl')
plot(km$centers,
     xlim=c(-74.2, -73.7), ylim=c(40.5, 41),
     xlab='x', ylab='y',
     main='Sample of data (dropoff)')

ss <- km$centers

h1 <- hclust(dist(ss, method='euclidean'))
plot(h1)
cl <- cutree(h1, h=0.2)
table(cl)
xycl <- list(ss[cl==1,])
xycl <- c(xycl, list(ss[cl==2,]))
xycl <- c(xycl, list(ss[cl==4,]))
xycl <- c(xycl, list(ss[cl==5,]))
xycl <- c(xycl, list(ss[cl>=6,]))
ss <- ss[cl==3,]

h1 <- hclust(dist(ss, method='euclidean'))
plot(h1)
cl <- cutree(h1, h=0.05)
table(cl)
xycl <- c(xycl, list(ss[cl==1,]))
xycl <- c(xycl, list(ss[cl==2,]))
xycl <- c(xycl, list(ss[cl==4,]))
xycl <- c(xycl, list(ss[cl==5,]))
xycl <- c(xycl, list(ss[cl==3 | cl>=6,]))

plot(NULL,
     xlim=c(-74.2, -73.7), ylim=c(40.5, 41),
     xlab='x', ylab='y',
     main='Hierarquical Clustering (dropoff)')

for (i in 1:length(xycl))
  points(xycl[[i]], col=colors[i])
legend('topright',
       legend=paste('cluster', 1:length(xycl)),
       fill=colors[1:length(xycl)])

for (i in 1:length(xycl)) {
  xycl[[i]] <- cbind(xycl[[i]], i)
}
xycl <- Reduce(rbind, xycl)
colnames(xycl) <- c('x0', 'y0', 'cluster')

ix <- as.integer(row.names(xycl))
mean.durs <- sapply(1:10000, function(i) round(mean(data[km$cluster == i, 'duration'])))
xycl <- cbind(xycl, mean.durs[ix])
plot(factor(xycl[,3]), xycl[,4],
     xlab='cluster', ylab='duration',
     main='Duration by cluster (dropoff)')

########################### DROPOFF HIERARCHICAL ###########################
