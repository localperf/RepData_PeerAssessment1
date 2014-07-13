
#--assessment1
#--coursera reproducible research

library (lubridate)

get.data = function() {
   data = read.csv("activity.csv", na.strings = "NA", as.is = T)
   print (dim(data))
   head(data)
   data$date = mdy(data$date)
 
   data$hour         = floor(data$interval / 100)
   data$minute       = data$interval - 100 * data$hour
   data$time.of.day  = paste(data$hour, data$minute, sep = ":")
   data$time         = data$hour + data$minute / 60
 
   table (data$time.of.day, useNA = "always")
   
   print (table(data$steps, useNA = "always"))
  
   print (str(data))
   data
}



plot.histo = function(data) {
   local = data[! is.na(data$steps),]
   steps = tapply(local$steps, local$date, sum)
   hist (steps)
   print (summary(steps))
}

plot.pattern = function (data) {
   local = data[! is.na(data$steps),]
   steps = tapply(local$steps, local$time, mean)
   times = sort(unique(local$time))
   plot (times, steps, type = "l", main = "Mean Steps by 5-Minute Interval",
         xlab = "Interval Start Time", ylab = "Mean Steps", col = "blue", lwd = 2,
         xaxt = "n")
   axis (side = 1, at = seq(0, 24, by = 4), labels = seq(0, 24, by = 4))
   
   index = which(steps == max(steps))
   print (paste("Maximum interval is at", data$time.of.day[index], round(steps[index], 1), "steps"))
   x = times[index]
   segments (x, -10, x, 0, col = "red")
   segments (-2, steps[index], 0, steps[index], col = "red")
}
   

impute.missing = function (data) {
   #--assign any missing the median value fcor the non-missing in that interval
   non.missing = data[complete.cases(data),]
   missing     = data[is.na(data$steps),]
   print (paste("Number missing =", dim(missing)[1]))
   
   medians           = tapply(non.missing$steps, non.missing$time, median)
   imputed           = data.frame(time = sort(unique(missing$time)), steps = medians)
   rownames(imputed) = NULL
   missing$steps     = NULL
   imputed           = merge(missing, imputed, all = T)
   
   imputed = imputed[,c("steps", "date", "interval", "hour", "time.of.day", "minute", "time")]
   colnames(imputed)
   colnames(non.missing)
   
   imputed = rbind(non.missing, imputed)
   dim(imputed)
   merged = imputed[order(imputed$date, imputed$time),]
   
   hist(merged$steps, main = "Steps with Imputed Medians")
   summary(merged$steps)
   merged
   }

differences = function (local = merged) {
   local$day = weekdays(local$date)
   local$weekday = local$day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
   table(local$day, local$weekday)
   local$weekday = factor(local$weekday, labels = c("weekend", "weekday"))
   table(local$day, local$weekday)
   par (mfrow = c(2,1))
   par (oma = c(1,1,3,1))
   weekend = local[local $weekday == "weekend",]
   weekday = local[local$weekday == "weekday",]
   means = tapply(weekend$steps, weekend$time, mean)
   times = sort(unique(merged$time))
   plot (times, means, main = "Weekend", xaxt = "n", xlab = "", ylab = "Number of Steps", 
         type = "l", col = "blue", lwd = 2, ylim = c(0, 200))
   means = tapply(weekday$steps, weekday$time, mean)
   plot (times, means, main = "weekday", xaxt = "n", xlab = "Interval", ylab = "Numbrer of Steps",
         type = "l", col = "blue", lwd = 2, ylim = c(0, 200))
   axis (side = 1, at = seq(0, 24, by = 4), labels = seq(0, 24, by = 4))
   mtext (outer = T, side = 3, "Mean Steps in 5-Minute Intervals")

#-----------------------------------------------------------------------------------------------------------

setwd ("d://coursera//reproducible")
data = get.data()
table (data$date, useNA = "always")
table(data$steps, useNA = "always")

plot.histo(data)
plot.pattern(data)

merged = impute.missing(data)     #--impute missing values, make histogram, and summarize steps

differences (merged)

