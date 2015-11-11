---
title: "Reproducible Research: Peer Assessment 1"
author: "Jason Foster"
date: "November 11, 2015"
output: html_document
---

Introduction
---

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data. 

About the Data
---
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

 - **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
 - **date**: The date on which the measurement was taken in YYYY-MM-DD format
 - **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Analysis for the Assignment
---
*Preparing the R Environment*

To perform the analysis and create the plots, we are using the dplyr and ggplot2 libraries.

```{r}
library(dplyr)
library(ggplot2)
```

**Loading and Pre-Processing the Data**

Here we unzip the archive containing the raw data and read it.  Note that we are not allowing the string dates to be converted into factors.

```{r}
activity <- read.csv(unz("activity.zip", "activity.csv"), stringsAsFactors = FALSE)
```

The date in the raw data is of class character, and for some of the analysis we require actual dates, so to the raw data we add a POSIXct date converted from the original string data.

```{r}
activity$ctDate <- as.POSIXct(activity$date, format = "%Y-%m-%d")
```

**What is mean total number of steps taken per day?**

Here, we group the data by date and calculate the sum for each using the `dplyr` package.  Note that in calculating the sum, we are ignoring NA values.  This seems to me to be more efficient than subsetting based on `complete.cases`.

```{r}
stepsPerDay <- activity %>%
   group_by(ctDate) %>%
   summarize(totalSteps = sum(steps, na.rm = TRUE))
```

Now we can plot a histogram for the total steps taken per day

```{r}
ggplot(data=stepsPerDay, aes(stepsPerDay$totalSteps)) +
   geom_histogram(breaks = seq(0, 25000, by = 5000), col = "black", fill = "cornflowerblue", alpha = .5) +
   ylim(c(0,30)) +
   ggtitle("Histogram of Total Steps Per Day") +
   labs(x = "Total Steps", y = "Count") +
   geom_vline(aes(xintercept = mean(stepsPerDay$totalSteps, na.rm = TRUE), color = "Mean"), show_guide = TRUE) +
   geom_vline(aes(xintercept = median(stepsPerDay$totalSteps, na.rm = TRUE), color = "Median"), show_guide = TRUE) +
   geom_rug(col = "burlywood",alpha = .5) +
   scale_colour_manual(name="Legend", values = c(Mean = "firebrick", Median = "forestgreen")) +
   theme_bw(base_family = "Avenir", base_size = 12)
```

The mean and median shown on the plot correspond with the following summary data.

```{r}
summary(stepsPerDay$totalSteps)
```
