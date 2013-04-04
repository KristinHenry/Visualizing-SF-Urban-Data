require(ggplot2)  # ggplot
require(plyr)
require(reshape2)
require(RColorBrewer)



# # import data
p_data <- read.csv(file="passenger-count-24.csv", header=TRUE)


# take subset of day's bus runs, before 10am
p_data_1 <- subset(p_data,  TRIP_ID >= 526 & TRIP_ID <= 900 & SECONDS <= 36000)



# sort by trip key
p_data_1 <- p_data_1[order(p_data_1$DIR, p_data_1$TRIP_KEY), ]


p <- ggplot(p_data_1, aes(x = SECONDS, 
                        y = TRIP_KEY,  
                        group = TRIP_KEY, 
                        color = factor(DIR,labels=c("inbound","outbound")), #factor(DIR),
                        size = LOAD),
            panel.grid.major = theme_blank(),
            panel.grid.minor = theme_blank(),
            panel.border = theme_blank(),
            panel.background = theme_blank()
            ) 

 
p <- p + scale_color_manual(values=c("#66CC99", "#330066")) 
p <- p + guides(color=guide_legend(override.aes = list(size = 6)))

p <- p + scale_size_area(max_size = 6)

p <- p + geom_point(alpha = 0.7) 
 
p <- p + labs(x="Time, on 10/5/12", y="Bus Trips", title="Morning passenger counts on the SF MUNI 24-Divisadero bus line", 
              color="Trip Direction ", size="Passengers")


# set time ticks/labels on axis
hour_first = 5
hour_last = 10
p <- p + scale_x_continuous(breaks = c(seq(hour_first*3600, hour_last*3600, by=3600)),
                          labels = c("5am", "6am", "7am", "8am", "9am", "10am"))

# relocate legend
p <- p + theme(legend.position = c(.15, .65))

# clean up the graph details
p <- p + theme(axis.ticks.x = element_blank())
p <- p + theme(axis.ticks.y = element_blank())
p <- p + theme(axis.text.y = element_blank())
p <- p + theme(panel.grid.major.y = element_blank())
p <- p + theme(panel.grid.minor.y = element_blank())
p <- p + theme(panel.grid.major.x = element_line(color = "#cccccc"))
p <- p + theme(panel.background = element_blank())

print(p)
ggsave("midterm1.png", width = 9, height = 5)

# # #----------------------------------------------------------------------------------


p_data_temp <- subset(p_data, 
                      (STOP_NAME == " CASTRO ST&17TH ST SE-NS/BZ" | 
                       STOP_NAME == " CASTRO ST&MARKET ST SW-FS/BB" ))

p_data_2 <- data.frame(p_data_temp$STOP_ID)
p_data_2$TIMESTOP <- p_data_temp$TIMESTOP
p_data_2$SECONDS <- p_data_temp$SECONDS
p_data_2$DIR <- p_data_temp$DIR
p_data_2$LOAD <- p_data_temp$LOAD
p_data_2$OFF <- p_data_temp$OFF
p_data_2$ON <- p_data_temp$ON
p_data_2$CONTINUES <- p_data_temp$LOAD - p_data_temp$ON  #- p_data_temp$OFF #

# # clean data
p_data_2 <- subset(p_data_2, (SECONDS<=86400 & SECONDS>=21600))
#p_data_2 <- subset(p_data_2, (SECONDS<=72000 & SECONDS>=21600))
# 
# correct for negative values of "continues"
max <- nrow(p_data_2)
i <- 0
while(i < max){
  i <- i + 1
  if(p_data_2[i,8] < 0){
    p_data_2[i,8] <- 0
  }
}

# make sure small-mults headings are meaningful
i <- 0
while(i < max){
  i <- i +1
  if(p_data_2[i,4] == 0){
    p_data_2[i,4] <- "Inbound"
  } else {
    p_data_2[i,4] <- "Outbound"
  }
}


# sort by time
p_data_2 <- p_data_2[order(p_data_2$DIR, p_data_2$SECONDS), ]

# workaround -- adjust spacing, if bars are too close
i <- 0
max <- nrow(p_data_2)
while(i < (max-1)){
  i <- i + 1
  dif <- p_data_2[i+1,3] - p_data_2[i,3]   
  
  # if too close, and same dir, shift over
  if(dif <= 450 & p_data_2[i,4] == p_data_2[i+1,4]){    
    p_data_2[i+1,3] <- p_data_2[i,3] + 520 
  } 
}


temp <- p_data_2[, c(3, 4, 5, 8, 7,6)]
mx <- melt(temp, id.vars=1:3)


p <- ggplot(mx, aes(x=SECONDS, y=value, fill=variable)) 
p <- p  + geom_bar(alpha=0.7, stat="identity", width=444, binwidth=2)
p <- p  + facet_wrap( ~ DIR, ncol = 1) 
p <- p + scale_fill_manual(values=c("#1B779E", "#330066", "#CCCCCC"))

p <- p + labs(x="Time, on 10/5/12", y="Passengers", title="Passenger changes for Castro at Market stop: SF MUNI 24-Divis", 
     fill="Passenger Activity")

# set time ticks/labels on axis
hour_first = 6 
hour_last = 24
p <- p + scale_x_continuous(expand = c(0, 0),
                            breaks = c(seq(hour_first*3600, hour_last*3600, by=2*3600)),
                            labels = c("6am", "8am", "10am", "12noon",
                                     "2pm", "4pm", "6pm", "8pm", "10pm", "12midnight"))


p <- p + scale_y_continuous(expand = c(0,0))
p <- p + theme(panel.background = element_blank())
p <- p + theme(axis.ticks.x = element_blank())
p <- p + theme(axis.ticks.y = element_blank())
p <- p + theme(strip.background = element_blank())
p <- p + theme(panel.grid.major.y = element_line(color = "#eeeeee"))
p <- p + theme(panel.grid.major.x = element_line(color = "#eeeeee"))


# Move label to bottom
p <- p + theme(legend.position = "bottom")

print(p)
ggsave("midterm2.png", width = 9, height = 5)


# ----------------------------------------------------------------------------


# bubble plot of loads over time, to show when over passenger capacity

p_data_3 <- subset(p_data, (SECONDS<=86400 & SECONDS>=21600))


p <- ggplot(p_data_3, aes(x = SECONDS, 
                        color = factor(DIR,labels=c("Inbound","Outbound")) 
                        ),
            panel.grid.major = theme_blank(),
            panel.grid.minor = theme_blank(),
            panel.border = theme_blank(),
            panel.background = theme_blank()) 

p <- p + scale_color_manual(values=c("#66CC99", "#330066")) 
p <- p + scale_size_area(max_size = 3)

label <- "Capacity" 

p <- p + geom_hline(aes(yintercept=63), colour="#E01B6A") + 
         geom_text(aes(77000,63,label = "Passenger Capacity = 63", vjust = -1), size=3, colour="#000000")

p <- p + geom_hline(aes(yintercept=54), colour="#888696") + 
  geom_text(aes(77000,54,label = "85% Load Standard  = 54", vjust = -1), size=3, colour="#000000")


p <- p + geom_point(alpha = 0.4, position = "jitter",
                    aes(y = LOAD, size = LOAD))

p <- p + theme(legend.position = c(.15, .90), legend.direction="horizontal")

p <- p + labs(x="Time, on 10/5/12", y="Passengers", title="Passenger counts, as recorded at various stops and runs of the SF MUNI 24-Divis line", 
                   color="Direction", size="Passengers")

hour_first = 6 
hour_last = 24
p <- p + scale_x_continuous(#expand = c(0, 0),
                            breaks = c(seq(hour_first*3600, hour_last*3600, by=2*3600)),
                            labels = c( "6am", "8am", "10am", "12noon",
                                     "2pm", "4pm", "6pm", "8pm", "10pm", "12"))


p <- p + theme(panel.background = element_blank())
p <- p + theme(panel.grid.major.y = element_line(color = "#eeeeee"))
p <- p + theme(panel.grid.major.x = element_line(color = "#eeeeee"))
p <- p + theme(axis.ticks.x = element_blank())
p <- p + theme(axis.ticks.y = element_blank())

print(p)
ggsave("midterm3.png", width = 9, height = 5)


