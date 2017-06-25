if (!exists("dataset")) {
    dataset <- read.csv("data/repdata_data_StormData.csv.bz2")  
}

event_levels <- levels(dataset$EVTYPE)

library(dplyr)

event_injuries <- dataset %>%
    group_by(EVTYPE) %>%
    summarise(total_events = n(),
              avg_injuries = mean(INJURIES),
              total_injuries = sum(INJURIES)) %>%
    filter(total_events > 100) %>%
    arrange(desc(total_injuries)) %>%
    select(event_type = EVTYPE, total_events, avg_injuries, total_injuries)

event_fatalities <- dataset %>%
    group_by(EVTYPE) %>%
    summarise(total_events = n(),
              avg_fatalities = mean(FATALITIES),
              total_fatalities = sum(FATALITIES)) %>%
    filter(total_events > 100) %>%
    arrange(desc(total_fatalities)) %>%
    select(event_type = EVTYPE, total_events, avg_fatalities, total_fatalities)

event_data <- event_injuries %>%
    inner_join(event_fatalities) %>%
    filter((avg_injuries > 1 & avg_fatalities != 0) | (avg_fatalities > 1 & avg_injuries != 0)) %>%
    filter(avg_injuries != total_injuries & avg_fatalities != total_fatalities) %>%
    select(event_type, total_events, total_injuries, total_fatalities)