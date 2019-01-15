# Cleveland dot plot #

library(ggplot2)
library(ggpubr)
library(plyr)

#import data table containing EO records, with column coding for taxa groupings (column dat$Group)
dat <- read.csv(file="EOdat.csv")

#keep only tracked or watchlist species
dat2 <- dat[(dat$EO_TRACK != "N"),]

#Aggregate data to count number of EO in each taxa group
AggDat <- count(dat2, "Group")
AggDat$Groupt <- toTitleCase(AggDat$Group)

names(AggDat) #newly aggregated table column names, "Group", "freq", which is the count of members in each group

#function ggdotchart from the ggpubr package creates a Cleveland dot plot
#plot is sorted from smallest to largest group
#label=x specifies vector containing information for labeling each data point (here, the actual counts)
#theme_cleveland() adds the dashed lines for each row of data
#rremove() suppressed the legend, can be used for other parts of the plot as well, such as axis titles
#aesthetics can be further modified within the ggdotchart() function

ggdotchart(AggDat, "Group", "freq", color="Group", shape=19, 
           sorting="descending", rotate=TRUE, 
           dot.size=4, ylab="Number of observations", label="freq") + rremove("legend") + theme_cleveland()