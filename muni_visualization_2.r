require(ggplot2)  # ggplot


# # import data
p_data <- read.csv(file="passenger-count-24-48.csv", header=TRUE)
p_data_4 <- p_data 


p <- ggplot(p_data_4, aes(x = SECONDS, y = LOAD, group = ROUTE,
                          color = factor(DIR,labels=c("Inbound","Outbound"))))

p <- p + geom_point(alpha = 0.4, position = "jitter")

p <- p + scale_color_manual(values=c("#66CC99", "#330066"))

p <- p + labs(x="", y="Passengers", title="SF MUNI passengers, on the 24 and 48 lines, as reported Oct 2, 2012", 
                   color="Direction", size="Passengers")

p <- p + geom_hline(aes(yintercept=54), colour="#888696") + 
      geom_text(aes(77000,54,label = "85% Load Standard  = 54", vjust = -1), size=3, colour="#000000")

hour_first = 6 
hour_last = 24
p <- p + scale_x_continuous(breaks = c(seq(hour_first*3600, hour_last*3600, by=2*3600)),
                            labels = c( "6am", "8am", "10am", "12noon",
                                        "2pm", "4pm", "6pm", "8pm", "10pm", "12"))

p <- p + theme(panel.background = element_blank())
p <- p + theme(panel.grid.major.y = element_line(color = "#eeeeee"))
p <- p + theme(panel.grid.major.x = element_line(color = "#eeeeee"))
p <- p + theme(axis.ticks.x = element_blank())
p <- p + theme(axis.ticks.y = element_blank())


p <- p  + facet_wrap( ~ ROUTE, ncol = 2)
p <- p + theme(legend.position = "bottom")

print(p)

ggsave("midterm4.png", width = 9, height = 5)

