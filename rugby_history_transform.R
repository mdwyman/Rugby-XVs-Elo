#rugby_history_transform transforms raw data:
# -takes events list and turns each column into a new column in master df
# -takes teams list and turns each row/column into a new column in master df


reformat <- function(od) {
    if (length(od$events[[1]]) != 0) {
        new_data <- data.frame(matchId = od$matchId, venue.id = od$venue$id
                           , venue.city = od$venue$city
                           , venue.name = od$venue$name
                           , venue.country = od$venue$country
                           , time.millis = od$time$millis
                           , time.label = od$time$label
                           , attendance = od$attendance
                           , teams.id1 = od$teams[[1]][1,1]
                           , teams.name1 = od$teams[[1]][1,2]
 #                         , teams.abbr1 = od$teams[[1]][1,3]
                           , teams.id2 = od$teams[[1]][2,1]
                           , teams.name2 = od$teams[[1]][2,2]
 #                         , teams.abbr2 = od$teams[[1]][2,3]
                           , scores1 = od$scores[[1]][1]
                           , scores2 = od$scores[[1]][2]
                           , status = od$status, outcome = od$outcome
                           , events.id = od$events[[1]]$id
                           , events.label = od$events[[1]]$label
                           , events.sport = od$events[[1]]$sport
                           , events.start = od$events[[1]]$start
                           , events.end = od$events[[1]]$end)
    } else {
        new_data <- data.frame(matchId = od$matchId, venue.id = od$venue$id
                               , venue.city = od$venue$city
                               , venue.name = od$venue$name
                               , venue.country = od$venue$country
                               , time.millis = od$time$millis
                               , time.label = od$time$label
                               , attendance = od$attendance
                               , teams.id1 = od$teams[[1]][1,1]
                               , teams.name1 = od$teams[[1]][1,2]
                               , teams.id2 = od$teams[[1]][2,1]
                               , teams.name2 = od$teams[[1]][2,2]
                               , scores1 = od$scores[[1]][1]
                               , scores2 = od$scores[[1]][2]
                               , status = od$status, outcome = od$outcome)
    }
                           
    return(new_data)
}

load("elo_rugby/rugby_history_raw.rdata")

match_data <- reformat(gdata[1,])
curr_date_millis <- as.numeric(as.POSIXct(Sys.Date(), digits = 12))*1000-2*7*24*3600*1000

for (i in 2:length(gdata$matchId)) {
    if (gdata$time$millis[i] < curr_date_millis) {
        match_data <- merge(match_data, reformat(gdata[i,]), all = TRUE)
    }
#    print(gdata[[i]]$matchId)
}

save(match_data, file = "elo_rugby/rugby_history_transformed.rdata")

#Duplicates of some matches are due to multiple event labels, e.g. when
#a continental competition doubles as WC qualifying.
