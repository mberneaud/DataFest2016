############################################## Setting WD
setwd("/Users/laurencehendry/GitHub/DataFest2016/Laurence")
############################################## Packages
library(sjPlot)
library(acs)
library(ggplot2)
library(ecp)
############################################## Multiplot

# Define multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

############################################## change points 
# Clean date
stubhub.acdc.google.trend$Date <- as.Date(stubhub.acdc.google.trend$Week, "%Y%m%d")
#stubhub.acdc.google.trend$Date <- as.numeric(stubhub.acdc.google.trend$Week)
stubhub.acdc.google.trend$identifier <- rep("ACDC", nrow(stubhub.acdc.google.trend))
temp_df <- stubhub.acdc.google.trend
temp_df$Count <- seq(1:nrow(temp_df))
sub_change_points <- data.frame()
for (i in unique(temp_df$identifier)) {
  temp <- subset(temp_df, identifier == i)
  temp_out <- e.divisive(X = as.matrix(temp['acdc.stubhub'])) # Extract position of the estimated change points
  estimated_cp <- temp_out$estimates[
    seq(from = 1, to = length(temp_out$estimates) - 1)] # Put change points in original data frame for indiv.
  est_changes <- rep(0, nrow(temp)) 
  est_changes[estimated_cp] <- 1 # Combine into one data frame with all units
  temp$est_changes <- est_changes
  sub_change_points <- rbind(sub_change_points,
                             temp)
}
est_cps <- temp_df 
est_cps <- filter(est_cps, est_changes == 1)
temp$Date <- as.Date(stubhub.acdc.google.trend$Week, "%Y%m%d")
temp_graph <- ggplot(temp, aes(x = Count, y = acdc.stubhub)) +
  geom_line()+
  geom_vline(xintercept = est_cps$Count, linetype = 'dashed', colour = 'red') +
  geom_vline(xintercept = 608, colour = 'red')+
  geom_vline(xintercept = 595, linetype = 'dashed', colour = 'black')+
  ggtitle('CPs for ("Stubhub" and "ACDC") from Google Trends')+
  xlab("Dates") + 
  ylab("Demand")
temp_graph

############################################## correct event dates for graphs
table_event_dates$Date <- as.Date(table_event_dates$event_dt, "%d/%m/%Y")
?as.Date

############################################## change points for complete
setwd("/Users/laurencehendry/GitHub/DataFest2016/Laurence")
complete <- read.csv("Complete.csv")
# Clean date
complete$Week <- gsub(pattern = "-", replacement = "", complete$V1)
complete$Week <- substr(complete$Week, 1, 8)
complete$Date <- as.Date(complete$Week, "%Y%m%d")
#complete$Date <- as.numeric(complete$Week)
#stubhub.acdc.google.trend$identifier <- rep("ACDC", nrow(stubhub.acdc.google.trend))
temp_df <- complete
temp_df$identifier <- complete$V3
#temp_df$Count <- seq(1:nrow(temp_df))
sub_change_points <- data.frame()
for (i in unique(temp_df$identifier)) {
  temp <- subset(temp_df, identifier == i)
  temp_out <- e.divisive(X = as.matrix(temp['V2'])) # Extract position of the estimated change points
  estimated_cp <- temp_out$estimates[
    seq(from = 1, to = length(temp_out$estimates) - 1)] # Put change points in original data frame for indiv.
  est_changes <- rep(0, nrow(temp)) 
  est_changes[estimated_cp] <- 1 # Combine into one data frame with all units
  temp$est_changes <- est_changes
  sub_change_points <- rbind(sub_change_points,
                             temp)
}
# Graph van Halen
temp_for_graph <- sub_change_points %>% filter(V3 == "stubhub van halen")
temp_for_graph$Count <- seq(1:nrow(temp_for_graph))
#temp_for_graph <- temp_for_graph[500:639,]
maximum <- max(temp_for_graph$V2)
temp_for_graph$V2_new <- NA
for (i in 1:nrow(temp_for_graph)){
  temp_for_graph$V2_new[i] <- (temp_for_graph$V2[i]/maximum)*100
}
est_cps <- filter(temp_for_graph, est_changes == 1)
temp_graph <- ggplot(temp_for_graph, aes(x = Count, y = V2_new)) +
  geom_line()+
  geom_vline(xintercept = est_cps$Count, colour = 'red') +
  geom_vline(xintercept = 588, colour = 'blue', size = 1) + #van Hale Tribute
  geom_vline(xintercept = 601, colour = 'red') + #Actual van Hale
  ggtitle('"Stubhub and van Halen" in Google Traffic')+
  xlab("Dates") + 
  ylab("Demand")
#xlim(500, 639)+
#ylim(0,50)
#theme(axis.ticks = element_blank(), axis.text.y = element_blank())
p0 <- temp_graph + coord_fixed(ratio=0.6)
# Graph AC/DC
temp_for_graph <- sub_change_points %>% filter(V3 == "stubhub acdc")
temp_for_graph$Count <- seq(1:nrow(temp_for_graph))
temp_for_graph <- temp_for_graph[500:639,]
maximum <- max(temp_for_graph$V2)
temp_for_graph$V2_new <- NA
for (i in 1:nrow(temp_for_graph)){
  temp_for_graph$V2_new[i] <- (temp_for_graph$V2[i]/maximum)*100
}
est_cps <- filter(temp_for_graph, est_changes == 1)
temp_graph <- ggplot(temp_for_graph, aes(x = Count, y = V2_new)) +
  geom_line()+
  geom_vline(xintercept = est_cps$Count, colour = 'red', size = 1) +
  geom_vline(xintercept = 607, colour = 'blue', size = 1) + #van Hale Tribute
  #geom_vline(xintercept = 632, colour = 'blue', size = 1) + #Actual van Hale
  ggtitle('"AC/DC and Stubhub" in Google Traffic')+
  xlab("Dates") + 
  ylab("Demand (%)")
#xlim(500, 639)+
#ylim(0,50)
#theme(axis.ticks = element_blank(), axis.text.y = element_blank())
p1 <- temp_graph + coord_fixed(ratio=0.18)
p1
# Graph Sting
temp_for_graph <- sub_change_points %>% filter(V3 == "stubhub sting")
temp_for_graph$Count <- seq(1:nrow(temp_for_graph))
#temp_for_graph <- temp_for_graph[500:639,]
maximum <- max(temp_for_graph$V2)
temp_for_graph$V2_new <- NA
for (i in 1:nrow(temp_for_graph)){
  temp_for_graph$V2_new[i] <- (temp_for_graph$V2[i]/maximum)*100
}
est_cps <- filter(temp_for_graph, est_changes == 1)
temp_graph <- ggplot(temp_for_graph, aes(x = Count, y = V2_new)) +
  geom_line()+
  geom_vline(xintercept = est_cps$Count, colour = 'red') +
  geom_vline(xintercept = 337, colour = 'blue', size = 1) + #van Hale Tribute
  #geom_vline(xintercept = 601, colour = 'red') + #Actual van Hale
  ggtitle('"Stubhub and Sting" in Google Traffic')+
  xlab("Dates") + 
  ylab("Demand")
#xlim(500, 639)+
#ylim(0,50)
#theme(axis.ticks = element_blank(), axis.text.y = element_blank())
p2 <- temp_graph + coord_fixed(ratio=0.6)
p2
# Graph Maroon 5
install.packages("Hmisc")
library(Hmisc)
minor.tick(nx=n, ny=n, tick.ratio=n)
temp_for_graph <- sub_change_points %>% filter(V3 == "stubhub maroon 5")
temp_for_graph$Count <- seq(1:nrow(temp_for_graph))
temp_for_graph <- temp_for_graph[500:639,]
maximum <- max(temp_for_graph$V2)
temp_for_graph$V2_new <- NA
temp_for_graph$time_distance <- NA
event_week <- 584
for (i in 1:nrow(temp_for_graph)){
  temp_for_graph$V2_new[i] <- (temp_for_graph$V2[i]/maximum)*100
  temp_for_graph$time_distance[i] <- temp_for_graph$Count[i]-event_week
}
est_cps <- filter(temp_for_graph, est_changes == 1)
est_cps$test <- NA
for (i in 1:nrow(est_cps)){
  est_cps$Count[i] <- est_cps$Count[i]-event_week
}
temp_graph <- ggplot(temp_for_graph, aes(x = time_distance, y = V2_new)) +
  geom_line()+
  geom_vline(xintercept = est_cps$Count, colour = 'red', size = 1) +
  geom_vline(xintercept = 0, colour = 'blue', size = 1) + #Concert
  ggtitle('"Maroon 5 and Stubhub" in Google Traffic')+
  xlab("Weeks from concert") + 
  ylab("Demand (%)")
#xlim(500, 639)+
#ylim(0,50)
#theme(axis.ticks = element_blank(), axis.text.y = element_blank())
p3 <- temp_graph + 
  coord_fixed(ratio=0.18) +
  theme(axis.ticks = element_line(size = 1))
p3
?geom_vline()
?theme
# Graph Jennifer Lopez
temp_for_graph <- sub_change_points %>% filter(V3 == "stubhub jennifer lopez")
temp_for_graph$Count <- seq(1:nrow(temp_for_graph))
#temp_for_graph <- temp_for_graph[500:639,]
maximum <- max(temp_for_graph$V2)
temp_for_graph$V2_new <- NA
for (i in 1:nrow(temp_for_graph)){
  temp_for_graph$V2_new[i] <- (temp_for_graph$V2[i]/maximum)*100
}
est_cps <- filter(temp_for_graph, est_changes == 1)
temp_graph <- ggplot(temp_for_graph, aes(x = Count, y = V2_new)) +
  geom_line()+
  geom_vline(xintercept = est_cps$Count, colour = 'red') +
  geom_vline(xintercept = 631, colour = 'blue', size = 1) + #van Hale Tribute
  #geom_vline(xintercept = 601, colour = 'red') + #Actual van Hale
  ggtitle('"Stubhub and Jennifer Lopez" in Google Traffic')+
  xlab("Dates") + 
  ylab("Demand")
#xlim(500, 639)+
#ylim(0,50)
#theme(axis.ticks = element_blank(), axis.text.y = element_blank())
p4 <- temp_graph + coord_fixed(ratio=0.6)
p4
# Graph Billy Idol
temp_for_graph <- sub_change_points %>% filter(V3 == "stubhub billy idol")
temp_for_graph$Count <- seq(1:nrow(temp_for_graph))
#temp_for_graph <- temp_for_graph[500:639,]
maximum <- max(temp_for_graph$V2)
temp_for_graph$V2_new <- NA
for (i in 1:nrow(temp_for_graph)){
  temp_for_graph$V2_new[i] <- (temp_for_graph$V2[i]/maximum)*100
}
est_cps <- filter(temp_for_graph, est_changes == 1)
temp_graph <- ggplot(temp_for_graph, aes(x = Count, y = V2_new)) +
  geom_line()+
  geom_vline(xintercept = est_cps$Count, colour = 'red') +
  geom_vline(xintercept = 596, colour = 'blue', size = 1) + #van Hale Tribute
  #geom_vline(xintercept = 601, colour = 'red') + #Actual van Hale
  ggtitle('"Stubhub and Billy Idol" in Google Traffic')+
  xlab("Dates") + 
  ylab("Demand")
#  xlim(582, 639)
#ylim(0,50)