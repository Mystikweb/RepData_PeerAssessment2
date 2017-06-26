if (!exists("raw_data")) {
    column_classes <- c(rep("NULL", 7), "character",
                        rep("NULL", 14), "numeric", "numeric",
                        "numeric", "character", "numeric", "character",
                        rep("NULL", 9))
    raw_data <- read.csv("data/repdata_data_StormData.csv.bz2",
                         colClasses = column_classes)
}

library(dplyr)
library(tidyr)
library(ggplot2)

event_injuries <- raw_data %>%
    group_by(EVTYPE) %>%
    summarise(total_events = n(),
              avg_injuries = mean(INJURIES),
              total_injuries = sum(INJURIES)) %>%
    filter(total_events > 100) %>%
    arrange(desc(total_injuries)) %>%
    select(event_type = EVTYPE, 
           total_events, 
           avg_injuries, 
           total_injuries)

event_fatalities <- raw_data %>%
    group_by(EVTYPE) %>%
    summarise(total_events = n(),
              avg_fatalities = mean(FATALITIES),
              total_fatalities = sum(FATALITIES)) %>%
    filter(total_events > 100) %>%
    arrange(desc(total_fatalities)) %>%
    select(event_type = EVTYPE, 
           total_events, 
           avg_fatalities, 
           total_fatalities)

health_data <- event_injuries %>%
    inner_join(event_fatalities) %>%
    filter((avg_injuries > 1 & avg_fatalities != 0) | 
               (avg_fatalities > 1 & avg_injuries != 0)) %>%
    filter(avg_injuries != total_injuries & 
               avg_fatalities != total_fatalities) %>%
    select(event_type, 
           total_events, 
           total_injuries, 
           total_fatalities)

health_long <- gather(health_data, health_type, total_amount, total_injuries:total_fatalities, factor_key = TRUE)

health_plot <- ggplot(health_long, aes(x = event_type, y = total_amount, fill=health_type)) +
    geom_bar(stat = "identity")

print(health_plot)