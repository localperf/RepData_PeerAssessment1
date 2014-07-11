
#--assessment1
#--coursera reproducible research

library (lubridate)

get.data = function() {
   data = read.csv("activity.csv", na.strings = "NA", as.is = T)
   print (dim(data))
   data$date = ymd(data$date)
   data$original.steps = data$steps
   
   
   data$hour = floor(data$interval / 100)
   data$minute = data$interval - 100 * hour
   data$time.of.day = paste(data$hour, data$minute, sep = ":")
   data$time = data$hour + data$minute / 60
 
   table (data$time.of.day)
   
   print (table(data$steps, useNA = "always"))
   data$steps[is.na(data$steps)] = 0
   print (str(data))
   data
}

plot.histo = function(data) {
   daily.steps = tapply(data$steps, data$date, sum)
   hist (steps)
   print (summary(daily.steps))
}

plot.pattern = function (data) {
   interval.steps = tapply(data$steps, data$time, mean)
   times = sort(unique(data$time))
   plot (times, interval.steps, type = "l", main = "Mean Steps by 5-Minute Interval",
         xlab = "Interval Start Time (HH:MM)", ylab = "Mean Steps", col = "blue", lwd = 2,
         xaxt = "n")
   axis (side = 1, at = seq(0, 24, by = 4), labels = seq(0, 24, by = 4))
   
   index = which(interval.steps == max(interval.steps))
   print (paste("Maximum interval is at", data$time.of.day[index]))
   #abline (v = 8 + 35/60, col = "red")
   
}
   

impute.missing = function (data) {
   #--assign any missing the median value fcor the non-missing in that interval
   non.missing = data[complete.cases(data),]
   dim(non.missing)
   medians = tapply(non.missing$original.steps, non.missing$time, median)
   missing = data[! complete.cases(data),]
   missing.times = sort(unique(missing$time))
   medians = data.frame(time = missing.times, steps = medians)
   head(medians)
   missing = merge (missing, medians, by = time)
}


setwd ("c://coursera//reproducible")
data = get.data()
table (data$date, useNA = "always")
table(data$steps, useNA = "always")

plot.histo(data)
plot.pattern(data)
which()
