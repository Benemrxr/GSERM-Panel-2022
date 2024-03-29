# Intro things ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# GSERM - St. Gallen (2022)
#
# Analyzing Panel Data
# Prof. Christopher Zorn
#
# Day Three: Panel Dynamics.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load packages (install as needed, using
# install.packages()), and set some options:

library(RCurl)
library(readr)
library(haven)
library(psych)
library(gtools)
library(boot)
library(plyr)
library(dplyr)
library(texreg)
library(statmod)
library(pscl)
library(lme4)
library(plm)
library(stargazer)
library(sandwich)
library(prais)
library(nlme)
library(panelAR) # Note: This package is currently (6/6/22) archived; see
                 # https://stackoverflow.com/questions/71664861/r-package-panelar-not-available-to-install
                 # for installation instructions.
# install.packages("tseries")
library(tseries)
# install.packages("pcse")
library(pcse)
# install.packages("pgmm")
library(pgmm)
# install.packages("dynpanel")
library(dynpanel)
# install.packages("panelView")
library(panelView)
library(OrthoPanels)
library(dotwhisker)
library(performance)

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# setwd("~/Dropbox (Personal)/GSERM/Panel-2022/Notes and Slides")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Possibly-useful robust thing...
# 
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
rm(url_robust)
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Robust SEs mini-sim: ####

set.seed(3844469)
X <- rnorm(10)
Y <- 1 + X + rnorm(10)
df10 <- data.frame(ID=seq(1:10),X=X,Y=Y)
fit10 <- lm(Y~X,data=df10)
summary(fit10)
rob10 <- vcovHC(fit10,type="HC1")
sqrt(diag(rob10))

# "Clone" each observation 100 times:

df1K <- df10[rep(seq_len(nrow(df10)),each=100),]
df1K <- pdata.frame(df1K, index="ID")
fit1K <- lm(Y~X,data=df1K)
summary(fit1K)
summary(fit1K, cluster="ID")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# GLS-ARMA models ####
#
# Pull the WDI data...

WDI<-read_csv("https://github.com/PrisonRodeo/GSERM-Panel-2022/raw/main/Data/WDI.csv")

# Add a "Cold War" variable:

WDI$ColdWar <- with(WDI,ifelse(Year<1990,1,0))

# Keep a numeric year variable (for -panelAR-):

WDI$YearNumeric<-WDI$Year

# summary(WDI)
#
# Make the data a panel dataframe:

WDI<-pdata.frame(WDI,index=c("ISO3","Year"))

# Summary statistics:

vars<-c("WomenBusLawIndex","PopGrowth","UrbanPopulation","FertilityRate",
        "GDPPerCapita","NaturalResourceRents","ColdWar")
subset<-WDI[vars]
subset<-subset[complete.cases(subset),] # listwise deletion
subset$lnGDPPerCap<-log(subset$GDPPerCapita)
subset$GDPPerCapita<-NULL
describe(subset,fast=TRUE)

# How much autocorrelation in those variables?

PG<-pdwtest(PopGrowth~1,data=subset)
UP<-pdwtest(UrbanPopulation~1,data=subset)
FR<-pdwtest(FertilityRate~1,data=subset)
GDP<-pdwtest(lnGDPPerCap~1,data=subset)
NRR<-pdwtest(NaturalResourceRents~1,data=subset)
CW<-pdwtest(ColdWar~1,data=subset)

rhos<-data.frame(Variable=c("Population Growth","Urban Population",
                            "Fertility Rate","GDP Per Capita",
                            "Natural Resource Rents","Cold War"),
                 Rho = c(1-(PG$statistic/2),1-(UP$statistic/2),
                         1-(FR$statistic/2),1-(GDP$statistic/2),
                         1-(NRR$statistic/2),1-(CW$statistic/2)))

stargazer(rhos,summary=FALSE,out="Rhos.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Models:

# Pooled OLS:

OLS<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
        log(GDPPerCapita)+NaturalResourceRents+ColdWar, 
        data=WDI,model="pooling")
summary(OLS)

pdwtest(OLS)

# Prais-Winsten:

PraisWinsten<-panelAR(WomenBusLawIndex~PopGrowth+UrbanPopulation+
              FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
              ColdWar, data=WDI,panelVar="ISO3",timeVar="YearNumeric",
              autoCorr="ar1",panelCorrMethod="none",
              rho.na.rm=TRUE)
summary(PraisWinsten)
PraisWinsten$panelStructure$rho

# Make a LaTeX table:
#
# texreg(list(OLS,PraisWinsten),
#        custom.model.names=c("OLS","Prais-Winsten"),
#        custom.coef.names=c("Intercept","Population Growth","Urban Population",
#                            "Fertility Rate","ln(GDP Per Capita)",
#                            "Natural Resource Rents","Cold War"), 
#        stars=0.05)
#
# Note: This ^^ doesn't work at the moment, because panelAR is 
# currently (June 2022) a bit broken.
#
# GLS with homoscedasticity & unit-specific AR(1) autocorrelation:

GLS<-gls(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
         log(GDPPerCapita)+NaturalResourceRents+ColdWar,
         data=WDI,correlation=corAR1(form=~1|ISO3),
         na.action="na.omit")

summary(GLS)


# PCSEs:

PCSE<-panelAR(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
           log(GDPPerCapita)+NaturalResourceRents+ColdWar,
           data=WDI,panelVar="ISO3",timeVar="YearNumeric",
           autoCorr="ar1",panelCorrMethod="pcse",
           rho.na.rm=TRUE)

summary(PCSE)
PCSE$panelStructure$rho

# Make a nice comparison plot, the hard way:

hats<-data.frame(term=rep(c("(Intercept)","Pop. Growth",
                 "Urban Population","Fertility Rate",
                 "ln(GDP Per Capita)","Nat. Resource Rents",
                 "ColdWar"),4),
                 model=c(rep("OLS",7),rep("P-W",7),
                             rep("GLS",7),rep("PCSE",7)),
                 estimate=c(coef(OLS),coef(PraisWinsten),
                               coef(GLS),coef(PCSE)),
                 std.error=c(sqrt(diag(vcov(OLS))),
                                 sqrt(diag(vcov(PraisWinsten))),
                                 sqrt(diag(vcov(GLS))),
                                 sqrt(diag(vcov(PCSE))))
)

pdf("DynModelLadderPlot.pdf",6,4)
dwplot(hats,
       vline=geom_vline(xintercept=0,linetype=2),
       dot_args = list(aes(shape = model))) +
       theme_classic() +
       xlab("Coefficient Estimate") +
       guides(shape = guide_legend("Model"),
              colour = guide_legend("Model"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dynamics:  ####
#
# # Simulating unit roots, etc.
#
# Leaving this commented out for now...
# 
# set.seed(7222009)
# 
# # T=250 N(0,1) errors:
# 
# T <- 250
# Time <- seq(1:T)
# u <- rnorm(T)
# 
# # I(1) process:
# 
# I1<-cumsum(u)
# 
# # AR(1) with rho = 0.9 (done "by hand):
# 
# rho9 <- numeric(T)
# rho9[1] <- u[1] # initialize T=1 to the first error
# for(i in 2:T) {
#   rho9[i] <- (0.9* rho9[i-1]) + u[i]
# }
# 
# # Plot:
# 
# pdf("TSIllustrated.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(Time,I1,t="l",lwd=2,lty=3,col="red",
#      xlab="Time",ylab="Y")
# lines(Time,rho9,lwd=2,lty=2,col="blue")
# lines(Time,u,lwd=2)
# legend("bottomleft",
#        legend=c("N(0,1) errors","Unit Root","AR(1) with rho=0.9"),
#        col=c("black","red","blue"),bty="n",
#        lwd=c(2,2,2),lty=c(1,3,2))
# dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Panel unit root tests... ####
#
# Data:

WBLI<-data.frame(ISO3=WDI$ISO3,Year=WDI$Year,
                 WBLI=WDI$WomenBusLawIndex)
WBLI<-na.omit(WBLI)  # remove missing
WBLI<-pdata.frame(WBLI,index=c("ISO3","Year")) # panel data
WBLI.W<-data.frame(split(WBLI$WBLI,WBLI$ISO3)) # "wide" data

purtest(WBLI.W,exo="trend",test="levinlin",pmax=2)
purtest(WBLI.W,exo="trend",test="hadri",pmax=2)
purtest(WBLI.W,exo="trend",test="madwu",pmax=2)
purtest(WBLI.W,exo="trend",test="ips",pmax=2)

# Gather the statistics:

ur1<-purtest(WBLI.W,exo="trend",test="levinlin",pmax=2)
ur2<-purtest(WBLI.W,exo="trend",test="hadri",pmax=2)
ur3<-purtest(WBLI.W,exo="trend",test="madwu",pmax=2)
ur4<-purtest(WBLI.W,exo="trend",test="ips",pmax=2)

urs<-matrix(nrow=4,ncol=5)
for(i in 1:4){
  nom<-get(paste0("ur",i))
  urs[i,1]<-nom$statistic$method
  urs[i,2]<-nom$statistic$alternative
  urs[i,3]<-names(nom$statistic$statistic)
  urs[i,4]<-round(nom$statistic$statistic,3)
  urs[i,5]<-round(nom$statistic$p.value,4)
}

urs<-data.frame(urs)
colnames(urs)<-c("Test","Alternative","Statistic",
                   "Estimate","P-Value")
URTests<-stargazer(urs,summary=FALSE,
                   title="Panel Unit Root Tests: WBRI",
                   out="URTests.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some regression models, with dynamics... ####
#
# Lagged -dependent-variable model:

WDI$WBLI.L <- plm::lag(WDI$WomenBusLawIndex,n=1) # be sure to use the
# -plm- version of -lag-

LDV.fit <- lm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
                FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                ColdWar,data=WDI)

FD.fit <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+ColdWar,
              data=WDI,effect="individual",model="fd")

FE.fit <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+ColdWar,
              data=WDI,effect="individual",model="within")

LDV.FE.fit <- plm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
                    FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                    ColdWar,data=WDI,effect="individual",model="within")

# Next: Arellano-Bond model. Do not run, unless
# you are young, and have lots of time on your
# hands...

AB.fit<-pgmm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
               FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
               ColdWar|lag(WomenBusLawIndex,2:20),data=WDI,
             effect="individual",model="twosteps")

# Table:

texreg(list(LDV.fit,FD.fit,FE.fit,LDV.FE.fit),
       custom.model.names=c("Lagged Y","First Difference","FE","Lagged Y + FE"),
       custom.coef.names=c("Intercept","Lagged WBLI",
                           "Population Growth","Urban Population",
                           "Fertility Rate","ln(GDP Per Capita)",
                           "Natural Resource Rents","Cold War"),
       digits=3,stars=0.05)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Trend things... ####
#
# Trend illustration simulation:

set.seed(2719)
Tobs<-40 
X<-cumsum(rnorm(Tobs))+5
u<-rnorm(T,0,2)
T<-1:Tobs
Y<-10+X+u
Yt<-5+X+0.5*T+u

pdf("TrendPlot.pdf",5,6)
par(mar=c(4,4,2,2))
plot(T,Yt,t="l",lwd=2,lty=2,ylim=c(0,35),
     ylab="Y",col="blue")
lines(T,Y,lty=3,lwd=2,col="orange")
lines(T,X,lwd=2,col="black")
legend("topleft",bty="n",lwd=2,lty=c(2,3,1),
       col=c("blue","orange","black"),
       legend=c("Y2","Y1","X"))
dev.off()

# Regressions:

f1<-lm(Y~X)
f2<-lm(Yt~X)
f3<-lm(Yt~X+T)

stargazer(f1,f2,f3,omit.stat="f")

# Make a better trend variable in the WDI data:

WDI$Trend <- WDI$YearNumeric-1950

# FE w/o trend:

FE  <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
              log(GDPPerCapita)+NaturalResourceRents+ColdWar,
              data=WDI,effect="individual",model="within")

# FE with trend:

FE.trend <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+
              FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
              ColdWar+Trend,data=WDI,effect="individual",
              model="within")

# FE with trend + interaction:

FE.intx <- plm(WomenBusLawIndex~PopGrowth+
                UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+
                ColdWar+Trend+ColdWar*Trend,
                data=WDI,effect="individual",model="within")

# A table:

TableTrend <- stargazer(FE,FE.trend,FE.intx,
                    title="FE Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War",
                                       "Trend (1950=0)","Cold War x Trend"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="Trendy.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Orthogonal Parameters Model: ####

WDI$lnGDPPerCap<-log(WDI$GDPPerCapita) # create this variable

set.seed(7222009)
OPM.fit <- opm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
               lnGDPPerCap+NaturalResourceRents+ColdWar,
               data=WDI,index=c("ISO3","Year"),n.samp=1000)

# Ladder plot of estimates & CIs:

pdf("OPM-Ladder.pdf",8,6)
par(mar=c(4,12,2,2))
caterplot(OPM.fit,parm=c("beta","rho"),
          main=c(""),xlab="Parameter Estimate",
          labels=c("Population Growth",
                   "Urban Population","Fertility Rate",
                   "ln(GDP Per Capita)","Natural Resource Rents",
                   "Cold War","Rho"))
abline(v=c(0),lty=2)
dev.off()

# Short- and long-run effects:

SREs<-summary(OPM.fit)$quants[3:8,3]
LREs<-numeric(6)
for(i in 1:6){
  LREs[i]<-quantile(OPM.fit$samples$beta[,i]/(1-OPM.fit$samples$rho),
                    probs=c(0.50))
}

print(cbind(round(SREs,4),round(LREs,4)))

# fin
#
# Ignore this:
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simulate some panel unit root tests:
# 
# reps<-50 # How many sims?
# T<-10     # Panel length
# N<-5     # N of units
# ARs<- c(0.40,1.0) # no unit root, moderate AR(1) + unit root
# 
# LLs<-data.frame(matrix(ncol=length(ARs),nrow=reps))
# Hs<-data.frame(matrix(ncol=length(ARs),nrow=reps))
# MWs<-data.frame(matrix(ncol=length(ARs),nrow=reps))
# IPSs<-data.frame(matrix(ncol=length(ARs),nrow=reps))
# colnames(LLs)<-c("NoUR","UR")
# colnames(Hs)<-c("NoUR","UR")
# colnames(MWs)<-c("NoUR","UR")
# colnames(IPSs)<-c("NoUR","UR")
# 
# set.seed(7222009)
# for (j in 1:length(ARs)) {
#   for(i in 1:reps) {
#     DF<-data.frame(ID=rep(1:N,each=T),
#                    Time=rep(1:T,N),
#                    Y=NA)
#     for(l in 1:N) {
#       Y<-numeric(T) # T-length series
#       u <- rnorm(T) # N(0,1) noise
#       boof<-rnorm(1) # random N(0,1) start
#       Y[1]<-boof    # initialize series
#       for(k in 2:T) {
#         Y[k] <- (ARs[j] * Y[k-1]) + u[k] # AR series
#       }
#       DF[DF$ID==l,]$Y<-Y
#     } 
#     DF<-pdata.frame(DF,index=c("ID","Time"))
#     DFY<-DF$Y
#     LL<-purtest(DFY,exo="trend",test="levinlin",pmax=2)
#     H<-purtest(DFY,exo="trend",test="hadri",pmax=2)
#     MW<-purtest(DFY,exo="trend",test="madwu",pmax=2)
#     IPS<-purtest(DFY,exo="trend",test="ips",pmax=2)
#     LLs[i,j]<-LL$statistic$p.value
#     Hs[i,j]<-H$statistic$p.value
#     MWs[i,j]<-MW$statistic$p.value
#     IPSs[i,j]<-IPS$statistic$p.value
#     print(i)
#   }
# }
# 
# plot(density(IPSs$NoUR))
# lines(density(IPSs$UR),lty=2)
# plot(density(Hs$NoUR))
# lines(density(Hs$UR),lty=2)
# plot(density(MWs$NoUR))
# lines(density(MWs$UR),lty=2)
# plot(density(LLs$NoUR))
# lines(density(LLs$UR),lty=2)