require(ggplot2)  # ggplot
require(plyr)

# convert HH:MM:SS to seconds
time.to.seconds<- function(time) {
  time<-strsplit(time, ":")[[1]]
  return(as.numeric(time[1]) * 3600 + as.numeric(time[2]) * 60 +
           as.numeric(time[3]))
}  


# convert TRIP_ID into the index of the levels of TRIP_IDs
get.levels <- function(data){
  trips <- data$TRIP_ID
  levs <- levels(factor(trips))
  return(levs)
}
get.key <- function(x, levs){
  numkeys <- length(levs)
  i <- 0
  while(i < numkeys){
    i <- i +1
    if(levs[i] == x){
      return(i)
    }
  }
}

#----------------------------------------------------------

# # import data
raw_data=read.csv(file="passenger-count.csv", header=TRUE)


# get just data for 24 and 48 bus lines
data_small <- subset(raw_data, ((ROUTE == 48 | ROUTE == 24) & MO == 10 & (DAY == 2 | DAY == 5)))


# take some of the columns, but not all
data_small <- data.frame(ROUTE = data_small$ROUTE,
                         TRIP_ID = data_small$TRIP_ID,
                         DAY = data_small$DAY,
                         DIR = data_small$DIR,
                         STOP_NAME = data_small$STOP_NAME,
                         STOP_ID = data_small$STOP_ID,
                         LOAD = data_small$LOAD,
                         ON = data_small$ON,
                         OFF = data_small$OFF,
                         TIMESTOP = data_small$TIMESTOP
                         )



# add new columns with modifications of exiting data 
data_small$SECONDS <- -1   # save TIMESTOP values as seconds 

max <- nrow(data_small)
i <- 0
while(i < max){
  i <- i + 1
  
  b <- data_small[i,10]
  b <- toString(b)
  b <- time.to.seconds(b)
  data_small[i,11] <- b  
}

# save data for both 24 and 48 bus lines on Oct 2
data_smaller <- subset(data_small, DAY == 2 & SECONDS<=86400 & SECONDS>=21600)
write.csv(data_smaller, "passenger-count-24-48.csv", row.names=FALSE)


#----------------------------------------------------------------------------

# take subset for just the 24 bus line, on Oct 5, and save it
data_smallest <- subset(data_small, ROUTE == 24 & DAY == 5 & SECONDS<=86400 & SECONDS>=21600)

# assign TRIP_KEY, based on TRIP_ID, for evenly space bus runs for graphs
data_smallest$TRIP_KEY <- -1  
levs <- get.levels(data_smallest)
trips <- data_smallest$TRIP_ID

max <- nrow(data_smallest)
i <- 0
while(i < max){
  i <- i + 1
  
  key <- get.key(trips[i], levs)
  data_smallest[i,12] <- key
  
}

write.csv(data_smallest, "passenger-count-24.csv", row.names=FALSE)

#----------------------------------------------------------------------------





