if (!exists("raw_data")) {
    raw_data <- read.csv("data/repdata_data_StormData.csv.bz2")
}

library(dplyr)
library(tidyr)
library(ggplot2)

# event_injuries <- raw_data %>%
#     group_by(EVTYPE) %>%
#     summarise(total_events = n(),
#               avg_injuries = mean(INJURIES),
#               total_injuries = sum(INJURIES)) %>%
#     filter(total_events > 100) %>%
#     arrange(desc(total_injuries)) %>%
#     select(event_type = EVTYPE, 
#            total_events, 
#            avg_injuries, 
#            total_injuries)
# 
# event_fatalities <- raw_data %>%
#     group_by(EVTYPE) %>%
#     summarise(total_events = n(),
#               avg_fatalities = mean(FATALITIES),
#               total_fatalities = sum(FATALITIES)) %>%
#     filter(total_events > 100) %>%
#     arrange(desc(total_fatalities)) %>%
#     select(event_type = EVTYPE, 
#            total_events, 
#            avg_fatalities, 
#            total_fatalities)
# 
# health_data <- event_injuries %>%
#     inner_join(event_fatalities) %>%
#     filter((avg_injuries > 1 & avg_fatalities != 0) | 
#                (avg_fatalities > 1 & avg_injuries != 0)) %>%
#     filter(avg_injuries != total_injuries & 
#                avg_fatalities != total_fatalities) %>%
#     select(event_type, 
#            total_events, 
#            total_injuries, 
#            total_fatalities)

# health_long <- gather(health_data, health_type, total_amount, total_injuries:total_fatalities, factor_key = TRUE)
# 
# health_plot <- ggplot(health_long, aes(x = event_type, y = total_amount, fill=health_type)) +
#     geom_bar(stat = "identity")
# 
# print(health_plot)


property_data <- raw_data %>%
    filter(PROPDMG > 0) %>%
    mutate_at(vars(PROPDMGEXP), funs(toupper)) %>%
    mutate(property_dmg = case_when(
        .$PROPDMGEXP == "H" ~ .$PROPDMG * 100,
        .$PROPDMGEXP == "K" ~ .$PROPDMG * 1000,
        .$PROPDMGEXP == "M" ~ .$PROPDMG * 1000000,
        .$PROPDMGEXP == "B" ~ .$PROPDMG * 1000000000,
        TRUE ~ .$PROPDMG * 1
    )) %>%
    group_by(EVTYPE) %>%
    summarise(property_events = n(),
              total_property_dmg = sum(property_dmg)) %>%
    filter(property_events > 100) %>%
    select(event_type = EVTYPE, 
           property_events,
           total_property_dmg)

crop_data <- raw_data %>%
    filter(CROPDMG > 0) %>%
    mutate_at(vars(CROPDMGEXP), funs(toupper)) %>%
    mutate(crop_dmg = case_when(
        .$CROPDMGEXP == "H" ~ .$CROPDMG * 100,
        .$CROPDMGEXP == "K" ~ .$CROPDMG * 1000,
        .$CROPDMGEXP == "M" ~ .$CROPDMG * 1000000,
        .$CROPDMGEXP == "B" ~ .$CROPDMG * 1000000000,
        TRUE ~ .$CROPDMG * 1
    )) %>%
    group_by(EVTYPE) %>%
    summarise(crop_events = n(),
              total_crop_dmg = sum(crop_dmg)) %>%
    filter(crop_events > 100) %>%
    select(event_type = EVTYPE,
           crop_events,
           total_crop_dmg)

economy_data <- property_data %>%
    inner_join(crop_data, by = c("event_type")) %>%
    mutate(total_damages = total_property_dmg + total_crop_dmg,
           total_events = property_events + crop_events) %>%
    arrange(desc(total_damages)) %>%
    top_n(5)