##Rugby_history gets all international match data from 
##Pulselive's repository. Data is in json format and is then converted
##to a data frame to be used in Elo calculation

library("curl")
library("jsonlite")

data_url <- "http://cmsapi.pulselive.com/rugby/match.json?pageSize=100&page=0"
base_url <- "http://cmsapi.pulselive.com/rugby/match.json?pageSize=100&page="
document <- fromJSON(data_url)

game.data <- document$content
loop_pages <- document$pageInfo$numPages - 1

gdata <- convertGame(game.data)

for (i in 1:loop_pages) {
    data_url <- paste0(base_url, as.character(i))
    document <- fromJSON(data_url)
    new.data <- document$content
    ndata <- convertGame(new.data, pagen = i)
    gdata <- rbind(gdata, ndata)
}

convertGame <- function(dat, pagen = 0) {
    row.names(dat) <- as.character(100*pagen+as.integer(row.names(dat)))
    row.names(dat$time) <- as.character(100*pagen+as.integer(row.names(dat$time)))
    row.names(dat$venue) <- as.character(100*pagen+as.integer(row.names(dat$venue)))
    return(dat)
}

## rjson stuff
#library("rjson")

##document <- fromJSON(file = data_url, method = 'C')

x <- lapply(document$content, unlist) #use with rjson but not jsonlite


## rename team.id, team.name, team.abbreviation to distinguish two teams
for (xlist in 1:length(x)) {
    try(names(x[[xlist]])[names(x[[xlist]]) == "teams.name"]
        <- c("team.name1","team.name2"), silent = TRUE)
    try(names(x[[xlist]])[names(x[[xlist]]) == "teams.id"]
        <- c("team.id1","team.id2"), silent = TRUE)
    try(names(x[[xlist]])[names(x[[xlist]]) == "teams.abbreviation"]
        <- c("team.abbr1","team.abbr2"), silent = TRUE)
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

## doesn't handle non-event games
y <- sapply(x, function(X) X[cols]) 

## convert numerical columns to numeric
numcols <- c("matchId","team.id1","team.id2","scores1","scores2","events.id")
z <- as.data.frame(t(y), stringsAsFactors=FALSE)
z[numcols] <- lapply(numcols, function(X) as.numeric(as.character(z[[X]])))
