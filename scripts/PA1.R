# Prepare environment
library(dplyr)
library(ggplot2)

# Read the data
activity <- read.csv(unz("activity.zip", "activity.csv"), stringsAsFactors = FALSE)

# Transformations
activity$ctDate <- as.POSIXct(activity$date, format = "%Y-%m-%d")


# Calculate steps per day
stepsPerDay <- activity %>%
   group_by(ctDate) %>%
   summarize(totalSteps = sum(steps, na.rm = TRUE))

# Histogram 1
plot1 <- ggplot(data = stepsPerDay, aes(totalSteps)) +
   geom_histogram(breaks = seq(0, 25000, by = 5000), col = "black", fill = "cornflowerblue", alpha = .5) +
   ylim(c(0, 30)) +
   ggtitle("Histogram of Total Steps Per Day\n") +
   labs(x = "Total Steps", y = "Count") +
   geom_vline(aes(xintercept = mean(totalSteps, na.rm = TRUE), color = "Mean"), show_guide = TRUE) +
   geom_vline(aes(xintercept = median(totalSteps, na.rm = TRUE), color = "Median"), show_guide = TRUE) +
   geom_rug(col = "burlywood",alpha = .5) +
   scale_colour_manual(name = "Legend", values = c(Mean = "firebrick", Median = "forestgreen")) +
   theme_bw(base_family = "Avenir", base_size = 12)
print(plot1)

# Summary 1
summary(stepsPerDay$totalSteps)

# Average steps by interval
averages <- activity %>%
   group_by(interval) %>%
   summarize(avgSteps = mean(steps, na.rm = TRUE))

# Time series for averages
plot2 <- ggplot(data = averages, aes(x = interval, y = avgSteps)) +
   geom_line(color = "green4", size = 1) +
   geom_vline(aes(xintercept = as.numeric(averages[which.max(avgSteps), 1])), linetype = "dotted") +
   ggtitle("Average Steps by Interval") +
   labs(x = "Interval", y = "Average Steps") +
   theme_bw(base_family = "Avenir", base_size = 12)
print(plot2)

# Find the maximum average daily steps
max(averages$avgSteps)
as.numeric(averages[which.max(averages$avgSteps), 1])

# Calculate complete cases
lvCompletes <- complete.cases(activity)
length(lvCompletes[lvCompletes == FALSE])

# Build imputation input
imputation <- activity %>% 
   group_by(weekdays(ctDate), interval) %>% 
   summarize(mean = mean(steps, na.rm = TRUE))

# Display imputation data
head(imputation)

# Define imputation function
getImputedSteps <- function(steps, ctDate, intvl) {
   ifelse(is.na(steps), 
          as.integer(imputation[imputation$`weekdays(ctDate)` == weekdays(ctDate) & imputation$interval == intvl, 3]), 
          as.integer(steps))
}
   
# Create vector of imputed steps
v <- apply(activity, 1, function(x) { getImputedSteps(x[1], as.POSIXct(x[4]), as.integer(x[3])) })

# New dataset with imputed steps
imputedActivity <- activity
imputedActivity$steps <- v

# Recalculate total steps per day with imputed steps
imputedStepsPerDay <- imputedActivity %>%
   group_by(ctDate) %>%
   summarize(totalSteps = sum(steps))

# Histogram 2
plot3 <- ggplot(data = imputedStepsPerDay, aes(totalSteps)) +
   geom_histogram(breaks = seq(0, 25000, by = 5000), col = "black", fill = "cornflowerblue", alpha = .5) +
   ylim(c(0, 40)) +
   ggtitle("Histogram of Total Imputed Steps Per Day\n") +
   labs(x = "Total Steps", y = "Count") +
   geom_vline(aes(xintercept = mean(totalSteps), color = "Mean"), show_guide = TRUE) +
   geom_vline(aes(xintercept = median(totalSteps), color = "Median"), show_guide = TRUE) +
   geom_rug(col = "burlywood",alpha = .5) +
   scale_colour_manual(name = "Legend", values = c(Mean = "firebrick", Median = "forestgreen")) +
   theme_bw(base_family = "Avenir", base_size = 12)
print(plot3)

# Summary with imputed steps
summary(imputedStepsPerDay$totalSteps)

# Add factor variable for weekend vs. weekday to imputed activity dataset
imputedActivity <- imputedActivity %>%
   mutate(dayType = factor(ifelse(weekdays(ctDate) %in% c("Saturday","Sunday"), "Weekend", "Weekday")))

# Calculate imputed average steps per type of day
imputedAverages <- imputedActivity %>%
   group_by(dayType, interval) %>%
   summarize(avgSteps = mean(steps))

# Faceted time series plot of steps per day across day type
plot4 <- ggplot(data = imputedAverages, aes(x = interval, y = avgSteps, color = dayType)) +
   geom_line(size = 1) +
   geom_smooth(color = "black", method = "lm", se = TRUE, linetype = "dotted") +   
   facet_wrap(~ dayType, ncol = 1) +
   ggtitle(expression(atop("Average Number of Steps by Interval", atop(italic("Weekdays vs. Weekends"), "")))) +
   labs(x = "Interval", y = "Average Steps") +
   theme_bw(base_family = "Avenir", base_size = 12)
print(plot4)
