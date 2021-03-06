library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(arcgisbinding)

arc.check_product()

#import data table containing EO records, with column coding for taxa groupings (column dat$Group)
dat <- arc.open("W:/Heritage/Heritage_Data/Biotics_datasets.gdb/eo_ptreps")
dat <- arc.select(dat)
names(dat)

dat2$Year <-  parse_date_time(dat2$LASTOBS, c("Ymd","bdY","dby","Ym","Y","Y!")) #this works for cleaning dates, mostly what is left is just "NO DATE" and a handful of other weird ones
dat2$Year <- year(dat2$Year) #pull out just year

#clean EO data--remove rows missing data for year
dat2 <- dat[complete.cases(dat),]

#keep only tracked or watchlist species
dat2 <- dat[(dat$EO_TRACK != "N"),]

#make sure year is numeric, not a factor
dat2$YEAR <- as.numeric(as.character(dat2$YEAR))

# stacked density plot #

#set x axis to only display data from 1900-2018

plot <- ggplot(dat2, aes(YEAR, fill=Group))

#the adjust factor changes how smooth the density curve is by adjusting bandwidth--larger numbers are smoother curves
#the theme places the legend within the plot, in the top left corner, offset somewhat from edge
plot + geom_density(adjust=1.5, position="stack") + 
  xlim(1900, 2018) + ylab("Proportion of observations") + xlab("Year") +
  scale_fill_brewer(palette="Set3") + theme_pubr() +
  theme(legend.position = c(0.1, 1), legend.justification = c(0.1, 1))

