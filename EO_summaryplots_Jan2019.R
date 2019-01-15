library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)


dat <- read.csv(file.choose())
names(dat)

dat2 <- dat[complete.cases(dat),]
dat2 <- dat[(dat$EO_TRACK != "N"),]
dat2$YEAR <- as.numeric(as.character(dat2$YEAR))

# stacked density plot #
plot <- ggplot(dat2, aes(YEAR, fill=Group))

plot + geom_density(adjust=1.5, position="stack") + xlim(1900, 2018) + ylab("PROPORTION OF OBSERVATIONS") + scale_fill_brewer(palette="Set3") + theme_minimal()

plot(dat2$YEAR)

# bar plot #

# as two separate plots #
datVP <- dat2[(dat2$Group == "VASCULAR PLANTS"),]
datOthr <- dat2[(dat2$Group != "VASCULAR PLANTS"),]

plotvp <- ggplot(datVP, aes(Group))

plotvp + geom_bar(fill="#BC80BD") + theme(legend.position = "none") + theme_minimal()

plotO <- ggplot(datOthr, aes(Group, fill=Group))

plotO + geom_bar() + scale_fill_brewer(palette="Set3") + theme_minimal() + theme(aspect.ratio=c(1,2),legend.position = "none") 

# Cleveland dot plot ? #

AggDat <- read.csv(file.choose())
names(AggDat)
ggdotchart(AggDat, "Group", "Count", color="Group", shape=19, sorting="descending", rotate=TRUE, dot.size=4, label="Count") + theme_cleveland()
?ggdotchart
