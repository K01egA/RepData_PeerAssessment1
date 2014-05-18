data <- read.csv(unz("activity.zip", "activity.csv"))
data$date <- as.Date(data$date, format="%Y-%m-%d")
str(data)

data.total <- aggregate(steps ~ date, data, sum, na.rm=T)
par(las=2, cex=0.7)
barplot(data.total$steps, names.arg=data.total$date, border=F, main="Total number of steps taken each day")
par(las=1, cex=1)
hist(data.total$steps, breaks=20, col="gray", xlab="Steps count", main="Histogram of the total number of steps taken each day")
mean(data.total$steps, na.rm=T)
median(data.total$steps, na.rm=T)

data.intervals <- aggregate(steps ~ interval, data, mean, na.rm=T)
plot(data.intervals$interval, data.intervals$steps, type="l", xlab="Interval", ylab="Steps count", main="Average number of steps taken by interval")

nrow(data)
sum(!complete.cases(data))

data.complete <- data
na.ind <- which(is.na(data.complete$steps), arr.ind=T)
data.complete$steps[na.ind] <- data.intervals$steps[which(data.intervals$interval == data$interval[na.ind])]

data.complete.total <- aggregate(steps ~ date, data.complete, sum)
par(las=2, cex=0.7)
barplot(data.complete.total$steps, names.arg=data.complete.total$date, border=F, main="Total number of steps taken each day")
par(las=1, cex=1)
hist(data.complete.total$steps, breaks=20, col="gray", xlab="Steps count", main="Histogram of the total number of steps taken each day")
mean(data.complete.total$steps)
median(data.complete.total$steps)

Sys.setlocale("LC_TIME", "English")  # Windows
data.split <- split(data.complete, weekdays(data.complete$date) %in% c('Saturday','Sunday'))
names(data.split) <- c("weekday", "weekend")

data.split.intervals <- lapply(data.split, function(d) aggregate(steps ~ interval, d, mean))

#par(mfrow=c(1, 1))
for (name in names(data.split.intervals)) {
    plot(data.split.intervals[[name]]$interval, data.split.intervals[[name]]$steps, type="l", xlab="Interval", ylab="Steps count", main=paste("Average number of steps taken by interval in", name))
}
