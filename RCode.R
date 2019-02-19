library(ggplot2)
library(RColorBrewer)
library(arcgisbinding)
library(lubridate)
library(ggpubr)
library(vegan)
library(dplyr)

setwd("H:/Annual Report")

arc.check_product()

#import data table containing EO records, with column coding for taxa groupings (column dat$Group)
dat <- arc.open("W:/Heritage/Heritage_Data/Biotics_datasets.gdb/eo_ptreps")
dat <- arc.select(dat)

dat2 <- as.data.frame(dat)
dat2$Year <-  parse_date_time(dat2$LASTOBS, c("Ymd","bdY","dby","Ym","Y","Y!")) #this works! mostly what is left is just "NO DATE" and a handful of other weird ones
dat2$Year <- year(dat2$Year) #just year


#keep only tracked or watchlist species
dat3 <- dat2[(dat2$EO_TRACK != "N"),]

#Create grouping variable

dat3$ELCODE_2 <- substr(dat3$ELCODE, 1, 2)
dat3$ELCODE_5 <- substr(dat3$ELCODE, 1, 5) #for Leps

dat3 <- within(dat3, ELCODE_2[ELCODE_2 == 'II' & ELCODE_5 == 'IILEP'] <- 'IILEP') #identify which II's are Leps

Group <- read.csv(file="ElcodexGroup_lookuptable.csv") #read in look-up table to match first two letters of Elcode to species groups

dat4 <- merge(dat3, Group, by="ELCODE_2", all.x=TRUE)

dat4 <- dat4[(dat4$Group != "NA"),]

# stacked density plot #

#set x axis to only display data from 1900-2018

plot <- ggplot(dat4, aes(Year, fill=Group))

#the adjust factor changes how smooth the density curve is by adjusting bandwidth--larger numbers are smoother curves
#the theme places the legend within the plot, in the top left corner, offset somewhat from edge
plot + geom_density(adjust=1.5, position="stack") + 
  xlim(1900, 2018) + ylab("Proportion of observations") + xlab("Year") +
  scale_fill_brewer(palette="Set3") + theme_pubr() +
  theme(legend.position = c(0.1, 1), legend.justification = c(0.1, 1))


AggDat <- count(dat4, Group)
AggDat <- AggDat[-11,] #remove NAs

names(AggDat) #newly aggregated table column names, "Group", "n", which is the count of members in each group

#function ggdotchart from the ggpubr package creates a Cleveland dot plot
#plot is sorted from smallest to largest group
#label=x specifies vector containing information for labeling each data point (here, the actual counts)
#theme_cleveland() adds the dashed lines for each row of data
#rremove() suppressed the legend, can be used for other parts of the plot as well, such as axis titles
#aesthetics can be further modified within the ggdotchart() function

AggDat <- AggDat[order(AggDat$n, decreasing=TRUE),]
AggDat$Group <- factor(AggDat$Group, levels=c("Leps","Non-vascular plants","Fish","Communities","Mussels","Mammals","Other inverts","Birds","Herps","Vascular plants"))

ggscatter(AggDat, "Group", "n", color="black", fill="Group", shape=21, rotate=TRUE, palette="Set3",
           size=4.3, ylab="Number of observations", label="n") + rremove("legend") + theme_cleveland()
unique(dat4$Year)

Yrs <- count(dat4, Year)

write.csv(Yrs, "Obs_perYear.csv")
#Make a summary table w/ just the number of obs per group from 2018
Dat2018 <- dat4[(dat4$Year == "2018"),]
AggDat2018 <- count(Dat2018, Group)

write.csv(Dat2018, file="Just_2018.csv")

