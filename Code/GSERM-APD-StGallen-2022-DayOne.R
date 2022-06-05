# Introduction #########################################
# GSERM - St. Gallen (2022)
#
# Analyzing Panel Data
# Prof. Christopher Zorn
#
# Day One: Introduction to panel / TSCS data.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load packages (install as needed, using
# install.packages()), and set some options:

library(RCurl)
library(readr)
library(RColorBrewer)
library(colorspace)
library(foreign)
library(psych)
library(lme4)
library(plm)
library(gtools)
library(boot)
library(plyr)
library(dplyr)
library(texreg)
library(statmod)
library(pscl)
library(naniar)
# install.packages("ExPanDaR")
library(ExPanDaR)

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# setwd(); change / uncomment as necessary:
#
# setwd("~/Dropbox (Personal)/GSERM/Panel 2022/Notes and Slides")
#
# Tiny TSCS data example: ##############################

tinyURL<-"https://raw.githubusercontent.com/PrisonRodeo/GSERM-Panel-2022/master/Data/tinyTSCSexample.txt"
temp<-getURL(tinyURL)
tiny <- read.table(textConnection(temp),header=TRUE)
rm(temp, tinyURL)
tiny

aggXS <- ddply(tiny, .(ID), summarise,
               Year = mean(Year),
               Female = Female[1],
               GOP = mean(GOP),
               Approve = mean(Approve))
aggXS

aggT <- ddply(tiny, .(Year), summarise,
              Female = mean(Female),
              Pres=Pres[1],
              GOP = mean(GOP),
              Approve=mean(Approve))
aggT


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dimensions of Variation: Toy Example... ######

toy<-data.frame(ID=as.character(rep(LETTERS[1:4],4)),
                t=append(append(rep(1,4),rep(2,4)),
                         append(rep(3,4),rep(4,4))),
                X=c(1,3,5,7,2,4,6,8,3,5,7,9,4,6,8,10),
                Y=c(20,15,10,5,19,14,9,4,18,13,8,3,17,12,7,2))
toy$label<-paste0("i=",toy$ID,",t=",toy$t)

pdf("VariationScatter1.pdf",7,6)
par(mar=c(4,4,2,2))
with(toy, plot(X,Y,pch=c(19,15,17,18),
               col=t,xlim=c(0,11),ylim=c(0,20),
               ))
with(toy, text(X,Y,labels=label,pos=1))
dev.off()

# Means:

mY.ID<-tapply(toy$Y,toy$ID,mean)
mY.t<-tapply(toy$Y,toy$t,mean)
mX.ID<-tapply(toy$X,toy$ID,mean)
mX.t<-tapply(toy$X,toy$t,mean)

# #2

pdf("VariationScatter2.pdf",7,6)
par(mar=c(4,4,2,2))
with(toy, plot(X,Y,pch=c(19,15,17,18),
               col=t,xlim=c(0,11),ylim=c(0,20),
))
with(toy, text(X,Y,labels=label,pos=1))
abline(h=mY.ID,lty=2)
text(10,mY.ID,label=c("Mean of Y for unit A","Mean of Y for unit B",
                      "Mean of Y for unit C","Mean of Y for unit D"),
      pos=3,cex=0.8)
dev.off()

# #3

pdf("VariationScatter3.pdf",7,6)
par(mar=c(4,4,2,2))
with(toy, plot(X,Y,pch=c(19,15,17,18),
               col=t,xlim=c(0,11),ylim=c(0,20),
))
with(toy, text(X,Y,labels=label,pos=1))
abline(h=mY.t,lty=2,col=c(1,2,3,4))
text(10,mY.t,label=c("Mean of Y for Time 1","Mean of Y for Time 2",
                      "Mean of Y for Time 3","Mean of Y for Time 4"),
     pos=3,cex=0.8,col=c(1,2,3,4))
dev.off()

# #4

pdf("VariationScatter4.pdf",7,6)
par(mar=c(4,4,2,2))
with(toy, plot(X,Y,pch=c(19,15,17,18),
               col=t,xlim=c(0,11),ylim=c(0,20),
))
with(toy, text(X,Y,labels=label,pos=1))
abline(v=mX.ID,lty=2)
text(mX.ID,c(20,19,18,17),
     label=c("Mean of X for unit A","Mean of X for unit B",
                      "Mean of X for unit C","Mean of X for unit D"),
     cex=0.8)
dev.off()

# #5

pdf("VariationScatter5.pdf",7,6)
par(mar=c(4,4,2,2))
with(toy, plot(X,Y,pch=c(19,15,17,18),
               col=t,xlim=c(0,11),ylim=c(0,20),
))
with(toy, text(X,Y,labels=label,pos=1))
abline(v=mX.t,lty=2,col=c(1,2,3,4))
text(mX.t,c(20,19,18,17),
     label=c("Mean of X for Time 1","Mean of X for Time 2",
                     "Mean of X for Time 3","Mean of X for Time 4"),
     cex=0.8,col=c(1,2,3,4))
dev.off()

# Means, within- and between...

with(toy, describe(Y))
Ymeans <- ddply(toy,.(ID),summarise,
                Y=mean(Y))
with(Ymeans, describe(Y)) # between-unit variation

toy <- ddply(toy,.(ID), mutate,
             Ymean=mean(Y))
toy$within <- with(toy, Y-Ymean)
with(toy, describe(within)) # within-unit variation
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Make: U.S. Supreme Court data, 1946-2020 ####

SCDBVotes<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/GSERM-Panel-2022/master/Data/SCDB-2020.csv")

# Create term-level case counts:

Cases<-aggregate(docketId~term+justice,SCDBVotes,length)
Cases<-aggregate(docketId~term,Cases,max)
Cases<-plyr::rename(Cases, c("docketId"="NCases"))

# Aggregate to the justice-term level:

SCData<-ddply(SCDBVotes,.(justice,justiceName,term),summarise,
              LiberalPct=(mean(direction-1,na.rm=TRUE)*100),
              MajPct=(mean(majority-1,na.rm=TRUE)*100))

# Merge justice appointing president + Segal-Cover ideology
# measures:

SegalCover<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/GSERM-Panel-2022/master/Data/Segal-Cover.csv")
SegalCover<-as.data.frame(SegalCover)
SegalCover$YearApptd<-SegalCover$Year
SegalCover$Year<-NULL

SCData <- merge(SCData,SegalCover,by=c("justice"),
                all.x=TRUE,all.y=FALSE)

# Merge case counts:

SCData <- merge(SCData,Cases,by=c("term"),
                all.x=TRUE,all.y=FALSE)

# Fix Rehnquist + Kagan (2009): 

SCData<-SCData[SCData$Order!=32,]
SCData<-SCData[is.nan(SCData$LiberalPct)==FALSE,]

# Add Chief Justice variable:

SCData$CJ<-NULL
SCData$ChiefJustice<-"FMVinson"
SCData$ChiefJustice<-ifelse(SCData$term>1952 & SCData$term<1969,
                            paste("EWarren"),SCData$ChiefJustice)
SCData$ChiefJustice<-ifelse(SCData$term>1968 & SCData$term<1986,
                            paste("WEBurger"),SCData$ChiefJustice)
SCData$ChiefJustice<-ifelse(SCData$term>1985 & SCData$term<2005,
                            paste("WHRehnquist"),SCData$ChiefJustice)
SCData$ChiefJustice<-ifelse(SCData$term>2004,
                            paste("JRoberts"),SCData$ChiefJustice)
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Visualize, etc. Supreme Court data ####

summary(SCData)

# How many justices and terms per justice?

length(unique(SCData$justice))

JData<-ddply(SCData,.(justice),summarise,
             Terms=n())

pdf("TermsServed.pdf",6,5)
par(mar=c(4,4,2,2))
hist(JData$Terms,col="grey80",freq=TRUE,
     main="",xlab="Terms Served")
abline(v=mean(JData$Terms),lwd=2,lty=2)
dev.off()

# Missing Data:

pdf("SCtMissingData.pdf",7,6)
vis_miss(SCData)
dev.off()

# Variation: Liberal Voting Percentage:
#
# Total variation:

with(SCData, describe(LiberalPct))

# Between-Justice variation:

LibMeans <- ddply(SCData,.(justice),summarise,
                MeanLibPct=mean(LiberalPct))
with(LibMeans, describe(MeanLibPct))

# Within-Justice variation:

SCData <- ddply(SCData,.(justice), mutate,
             LibMean=mean(LiberalPct))
SCData$LibWithin <- with(SCData, LiberalPct-LibMean)
with(SCData, describe(LibWithin))

# Variation: Justice Ideology (no temporal variation 
# within units):

# Total variation:

with(SCData, describe(Ideology))

# Between-Justice variation:

IdeoMeans <- ddply(SCData,.(justice),summarise,
                  MeanIdeo=mean(Ideology))
with(IdeoMeans, describe(MeanIdeo))

# Within-Justice variation (hint - there is none):

SCData <- ddply(SCData,.(justice), mutate,
                IdeoMean=mean(Ideology))
SCData$IdeoWithin <- with(SCData, Ideology-IdeoMean)
with(SCData, describe(IdeoWithin))

# Variation: Number of cases decided per term 
# (note: there is no cross-sectional variation 
# within a given term):

# Total variation:

with(SCData, describe(NCases))

# Between-Term variation:

NCMeans <- ddply(SCData,.(term),summarise,
                   MeanNCases=mean(NCases))
with(NCMeans, describe(MeanNCases))


# Within-Term variation (none):

SCData <- ddply(SCData,.(term), mutate,
                NCMean=mean(NCases))
SCData$NCWithin <- with(SCData, NCases-NCMean)
with(SCData, describe(NCWithin))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Interactive Visualizations Using ExPanDaR ####

write.csv(SCData,"SCPanelData.csv")
ExPanD()

# ... and then proceed interactively...

# fin
