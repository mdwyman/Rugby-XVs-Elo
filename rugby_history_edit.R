load("elo_rugby/rugby_history_transformed.rdata")

match_data_edit <- match_data

table(match_data$events.sport)

no_sport <- match_data$matchId[is.na(match_data$events.sport)]
sum(is.na(match_data$events.sport))

#pulls out sevens data but need to "re-factor" 
sevens_data <- subset(match_data, events.sport == "mrs")
#gets rid of unused levels in data
sevens_data <- as.data.frame(lapply(sevens_data, function(x) if(is.factor(x)) factor(x) else x))

length(unique(sevens_data$events.id)) #--> 148 events

save(sevens_data, file = "elo_rugby/sevens.rdata")

grepl("1999/00", sevens_data$events.label) # returns logicals for each record

#Get 1999/00 series data
ws99_data <- subset(sevens_data, grepl("1999/00", sevens_data$events.label))
ws99_data <- as.data.frame(lapply(ws99_data, function(x) if(is.factor(x)) factor(x) else x))

#create teams list for 1999/00
teams <- sort(unique(c(levels(ws99_data$teams.name2),levels(ws99_data$teams.name1))))

#sorting games chronologically
#getting start time data: ws99_data$time.millis[grep("Dubai", ws99_data$events.label)]



