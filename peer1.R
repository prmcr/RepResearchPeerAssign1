
setwd("~/Courses/DataScience/ReproducibleResearch/peer-assign1/RepData_PeerAssessment1")
activity <- read.csv("activity.csv",sep=",",header=TRUE)
#activity$date <- strptime(tz="",activity$date,"%Y-%m-%d")
#sapply(activity,class)
#head(activity)

act.na <- activity[!is.na(activity$steps),]
comp.act <- activity[complete.cases(activity),]
summary(comp.act)
summary(act.na)

steps.date <- aggregate(act.na$steps, list(act.na$date), sum) 
#head(steps.date)
#steps.date <- aggregate(activity$steps, list(activity$date), sum) 
names(steps.date) <- c("date","steps")
#steps.date$date <- strptime(tz="",steps.date$date,"%Y-%m-%d")
summary(steps.date)
hist(steps.date$steps, xlab="Total no. of steps in a day",breaks=10)
abline(v = mean(steps.date$steps), col = "blue", lwd = 2)
abline(v = median(steps.date$steps), col = "red", lwd = 2)
dev.off()

format(round(mean(steps.date$steps), 2), nsmall = 2)

steps.interval <- aggregate(act.na$steps, list(act.na$interval), mean) 
names(steps.interval) <- c("interval","steps")
summary(steps.interval)
plot(steps.interval$interval,steps.interval$steps, xlab="5 min interval of the day",ylab="Average number of steps across all days",type="l")
dev.off()

steps.interval$interval[which.max(steps.interval$steps)]


nrow(activity)-sum(complete.cases(activity))

#library(lattice)
#xyplot(act.na$interval ~ act.na$steps | act.na$date)
#xyplot(activity$interval ~ activity$steps | activity$date)
#dev.off()

new.activity <- activity
#new.activity$steps[is.na(new.activity$steps)] <- steps.interval$interval[new.activitymean(dat[, i],  na.rm = TRUE)
#steps.interval$steps[which(steps.interval$interval == new.activity$interval[12])]

count <- 0
for(rowin in 1:nrow(new.activity)){
  if(is.na(new.activity$steps[rowin]))
  {
    count <- count + 1
    new.activity$steps[rowin] <- steps.interval$steps[which(steps.interval$interval == new.activity$interval[rowin])]
  }
}
sum(is.na(new.activity))
#apply(new.activity, 1, function(x) if(is.na(x$steps)) x$steps <- steps.interval$steps[which(steps.interval$interval == x$interval)])
summary(new.activity)
summary(activity)
new.steps.date <- aggregate(new.activity$steps, list(new.activity$date), sum) 
names(new.steps.date) <- c("date","steps")

hist(new.steps.date$steps, xlab="Total no. of steps in a day",breaks=10)
summary(new.steps.date)
abline(v = mean(new.steps.date$steps), col = "blue", lwd = 2)
abline(v = median(new.steps.date$steps), col = "red", lwd = 2)
hist(steps.date$steps, xlab="Total no. of steps in a day",breaks=10)
summary(steps.date)
abline(v = mean(steps.date$steps), col = "blue", lwd = 2)
abline(v = median(steps.date$steps), col = "red", lwd = 2)
dev.off()


new.activity$day <- (weekdays(as.Date(new.activity$date)) == "Saturday") | (weekdays(as.Date(new.activity$date)) == "Sunday")
new.activity$day[new.activity$day == TRUE] <- "weekend"
new.activity$day[new.activity$day == FALSE] <- "weekday"

summary(new.activity)
#factor(new.steps.date)

weekend.steps.interval <- aggregate(new.activity$steps[new.activity$day == TRUE], list(new.activity$interval[new.activity$day == TRUE]), mean) 
names(weekend.steps.interval) <- c("interval","steps")
plot(weekend.steps.interval$interval,weekend.steps.interval$steps, xlab="5 min interval of the day",ylab="Average number of steps across all days",type="l")

weekday.steps.interval <- aggregate(new.activity$steps[new.activity$day == FALSE], list(new.activity$interval[new.activity$day == FALSE]), mean) 
names(weekday.steps.interval) <- c("interval","steps")
plot(weekday.steps.interval$interval,weekday.steps.interval$steps, xlab="5 min interval of the day",ylab="Average number of steps across all days",type="l")
dev.off()

day.steps.interval <- aggregate(new.activity$steps, list(new.activity$interval,new.activity$day), mean) 
summary(day.steps.interval)
head(day.steps.interval)
names(day.steps.interval) <- c("interval","day","steps")

library(lattice)
tmp <- xyplot(day.steps.interval$steps~day.steps.interval$interval|day.steps.interval$day,type="l",
       xlab="interval", ylab="Number of steps",
       main="Interval vs Total number of steps",layout = c(1, 2)) 
class(tmp)
dev.off()