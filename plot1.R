library(dplyr)
library(ggplot2)

isWeekday <- function(pDate) {
   !(weekdays(pDate) %in% c('Saturday','Sunday'))
}

# Unzip and read the raw data
activity <- read.csv(unz("activity.zip", "activity.csv"), stringsAsFactors = FALSE)

# Transformations
# 1. Create a POSIXct date from the character date column
activity$ctDate <- as.POSIXct(activity$date, format = "%Y-%m-%d")

# Calculate total steps per day, ignoring NA values
stepsPerDay <- activity %>%
   group_by(ctDate) %>%
   summarize(totalSteps = sum(steps, na.rm = TRUE))

# Calculate the mean and median of our total steps to include in the plot
avgs <- data.frame(rbind(as.double(mean(stepsPerDay$totalSteps)), as.double(median(stepsPerDay$totalSteps))))

# Create a histogram for the total steps per day
stepsHist <- ggplot(data = stepsPerDay, aes(stepsPerDay$totalSteps)) +
   geom_histogram(breaks = seq(0, 25000, by = 5000), col = "black", fill = "cornflowerblue", alpha = .5) +
   ylim(c(0,30)) +
   ggtitle("Histogram of Total Steps Per Day") +
   labs(x = "Total Steps", y = "Count") +
   geom_vline(aes(xintercept = mean(stepsPerDay$totalSteps, na.rm = TRUE), color = "Mean"), show_guide = TRUE) +
   geom_vline(aes(xintercept = median(stepsPerDay$totalSteps, na.rm = TRUE), color = "Median"), show_guide = TRUE) +
   geom_rug(col = "burlywood",alpha = .5) +
   scale_colour_manual(name="Legend", values = c(Mean = "firebrick", Median = "forestgreen")) +
   theme_bw(base_family = "Avenir", base_size = 12)
print(stepsHist)

# Calculate a summary which includes the mean and median
summary(stepsPerDay$totalSteps)

# Summarize the average number of steps across all days for each of the 5-minute intervals
avgSteps <- activity %>%
   group_by(interval) %>%
   summarize(avgSteps = mean(steps, na.rm = TRUE))

# Plot the data as a time series
stepsTs <- ggplot(data = avgSteps, aes(x = interval, y = avgSteps)) +
   geom_line()
print(stepsTs)