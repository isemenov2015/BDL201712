require(lubridate)
require(dplyr)
dt <- read.csv('BD_LAB_EXAMPLE_SAMPLE.csv', stringsAsFactors = FALSE)
dt$event_start_date <- dmy_hms(dt$event_start_date)
dt$cost_numeric <- as.numeric(dt$cost)
dat <- as.Date(dt$event_start_date)
dwka <- format(dat, '%a')
dwkn <- as.numeric(format(dat, '%w'))
hist(dwkn, breaks = -.5 + 0:7, labels = unique(dwka[order(dwkn)]), main = 'Events by weekdays', xlab = 'Day of week')
hrs <- hour(dt$event_start_date)
hist(as.numeric(hrs), breaks = -.5 + 0:24, labels = unique(hrs[order(hrs)]), main = 'Events by time', xlab = 'Hour')
barplot(table(dt$event_sub[which(dt$event == 'network_ser')]), las = 2, main = 'Network events by type')
dt_calls <- dt[which(dt$event == 'network_ser' & dt$event_sub == 'onnet_voice'),]
dt_calls$call_duration <- as.numeric(dt_calls$call_duration_minutes)
activity <- dt_calls %>% group_by(hash_number_A) %>% summarise(count = n(), total_duration = sum(call_duration))
hist(agileness$count / 92, main = 'No of calls per day distribution', xlab = 'No of calls per day')
hrs <- hour(dt$event_start_date[which(dt$event == 'network_ser' & dt$event_sub == 'onnet_voice')])
hist(as.numeric(hrs), breaks = -.5 + 0:24, labels = unique(hrs[order(hrs)]), main = 'Phone calls by time', xlab = 'Hour')
interests <- dt_calls %>% group_by(interest_1) %>% summarise(count = n())

interests_filtered <- dt %>% group_by(interest_1) %>% summarise(count = n())
barplot(interests_filtered$count, names.arg = interests_filtered$interest_1, horiz = TRUE, las = 2)

interests_filtered <- dt %>% group_by(interest_2) %>% summarise(count = n())
barplot(interests_filtered$count, names.arg = interests_filtered$interest_2, horiz = TRUE, las = 2)

interests_filtered <- dt %>% group_by(interest_3) %>% summarise(count = n())
barplot(interests_filtered$count, names.arg = interests_filtered$interest_3, horiz = TRUE, las = 2)

interests_filtered <- dt %>% group_by(interest_4) %>% summarise(count = n())
barplot(interests_filtered$count, names.arg = interests_filtered$interest_4, horiz = TRUE, las = 2)

interests_filtered <- dt %>% group_by(interest_5) %>% summarise(count = n())
barplot(interests_filtered$count, names.arg = interests_filtered$interest_5, horiz = TRUE, las = 2)
