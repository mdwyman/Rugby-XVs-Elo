##Rugby_history gets all international match data from 
##Pulselive's repository. Data is in json format and is then converted
##to a data frame to be used in Elo calculation

library("curl")
library("jsonlite")

convertRowNames <- function(dat, pagen = 0) {
    row.names(dat) <- as.character(100*pagen+as.integer(row.names(dat)))
    try(row.names(dat$time) <- 
            as.character(100*pagen+as.integer(row.names(dat$time))),
        silent = TRUE)
    try(row.names(dat$venue) <- 
            as.character(100*pagen+as.integer(row.names(dat$venue))),
        silent = TRUE)
    return(dat)
}

addExtras <- function(dat1, dat2, new_cols, pagen) {
    gcols <- length(dat1)
    grows <- length(dat1$matchId)
    nNAs <- rep(NA, grows)
    for (j in 1:length(new_cols)) {
        if (new_cols[j] == "venue") {
            dat1$venue <- data.frame(rep(NA, 100), rep(NA,100)
                                     , rep(NA,100), rep(NA,100))
            names(dat1$venue) <- names(dat2$venue)
            try(row.names(dat1$venue) <- 
                    as.character(100*pagen+as.integer(row.names(dat1$venue))),
                silent = TRUE)
        } else {
            dat1$new_col <- nNAs
            names(dat1)[gcols + j] <- new_cols[j]
        }
    }
    return(dat1)
}

data_url <- "http://cmsapi.pulselive.com/rugby/match.json?pageSize=100&page=0"
base_url <- "http://cmsapi.pulselive.com/rugby/match.json?pageSize=100&page="
document <- fromJSON(data_url)

game.data <- document$content
loop_pages <- document$pageInfo$numPages - 1
#loop_pages <- 10

gdata <- convertRowNames(game.data)

for (i in 1:loop_pages) {
    data_url <- paste0(base_url, as.character(i))
    document <- fromJSON(data_url)
    new.data <- document$content
    ndata <- convertRowNames(new.data, pagen = i)
    
    #Check to see if ndata has new columns
    new_cols1 <- setdiff(names(ndata),names(gdata))
    if (length(new_cols1) != 0) {
        gdata <- addExtras(gdata, ndata, new_cols1, pagen = i)
    }
    
    new_cols2 <- setdiff(names(gdata),names(ndata))
    if (length(new_cols2) != 0) {
        ndata <- addExtras(ndata, gdata, new_cols2, pagen = i)
    }
    
    #if so add column(s) to gdata, fill with NAs then  merge the two
    # game.data["eventPhase"] <-
    
    gdata <- rbind(gdata, ndata)
    print(paste("Got page", as.character(i)))
}

full_cols <- c("matchId", "venue.id", "venue.name", "venue.city"
               , "venue.country", "time.millis", "time.label", "attendance"
               , "teams.id1", "teams.name1", "teams.abbr1", "teams.id2"
               , "teams.name2", "teams.abbr2", "scores1", "scores2", "status"
               , "outcome", "events.id", "events.label", "events.sport"
               , "events.start.millis", "events.start.label"
               , "events.end.millis", "events.end.label" )

## need to add other columns once team issue figured out 
cols <- c("matchId","teams.id1","teams.name1","teams.abbr1"
          ,"teams.id2","teams.name2","teams.abbr2"
          ,"scores1","scores2","outcome","events.id"
          ,"events.label")

