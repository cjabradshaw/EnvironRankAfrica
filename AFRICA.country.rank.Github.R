## R code (Version 3.5.2) for the following paper:
## BRADSHAW, CJA & E DI MININ. 2019. Socio-economic predictors of environmental degradation
## among African nations. Scientific Reports In press
##
## Corey J.A. Bradshaw (Flinders University, Australia) & Enrico Di Minin (University of Helsinki, Finland)
##
## code updated May 2019
##
## CONTACT: corey.bradshaw@flinders.edu.au
## globalecologyflinders.com

## remove everything
rm(list = ls())

## libraries
library(boot)
library(lme4)
library(sem)
library(semGOF) # note: might not function in later versions of R
library(dismo)
library(gbm)

## set working directory
setwd("~/...") # SET ACCORDINGLY

# background data for sorting
contreg <- read.table("continent.country.csv", sep=",", header=T)
contnum <- read.table("cntry.num.code.csv", sep=",", header=T)

## remove small-island nations (except Madagascar): Cabo Verde, Seychelles, Réunion, Comoros, Mauritius, São Tomé & Príncipe, Mayotte, Saint Helena-Ascension-Tristan da Cunha
smisnat <- c("CPV", "SYC", "REU", "COM", "MUS", "STP", "MYT", "SHN") 

## regions of Africa
regions.AFR <- read.table("Africa.regions.csv", sep=",", header=T)

## ENVIRONMENT VARIABLES
# Ecological Footprint consumption per capita
EFcons.PC <- read.table("EFconsPC12.csv", sep=",", header=T)
EFcons.reg <- merge(EFcons.PC, contreg, by="cntry.code")
AFR.EFcons.reg <- subset(EFcons.reg, cont=="AFR")
AFR.EFcons.sort <- AFR.EFcons.reg[order(AFR.EFcons.reg[,8], decreasing=F),]

# Megafauna Conservation Index
MCI <- read.table("megafaunaconserv.csv", sep=",", header=T)
mci.reg <- merge(MCI, contreg, by="cntry.code")
AFR.mci.reg <- subset(mci.reg, cont=="AFR")
AFR.mci.sort <- AFR.mci.reg[order(AFR.mci.reg[,8], decreasing=T),]

# proportion of land area protected
landprot <- read.table("landprot.csv", sep=",", header=T)
landprot.reg <- merge(landprot, contreg, by="cntry.code")
AFR.landprot.reg <- subset(landprot.reg, cont=="AFR")
AFR.landprot.sort <- AFR.landprot.reg[order(AFR.landprot.reg[,4], decreasing=F),]

# Threatened species (IUCN 2016 data ([CR+EN+VU+NT]/[CR+EN+VU+NT+LC]))
AFR.threat <- read.table("AFR.threat.csv", sep=",", header=T)
AFR.threat$prop.threat <- apply(AFR.threat[,c(5:8)], 1, sum, na.rm=T)/apply(AFR.threat[,c(5:8,11)], 1, sum, na.rm=T)
AFR.threat.reg <- merge(AFR.threat, contreg, by="cntry.code")
AFR.threat.sort <- AFR.threat.reg[order(AFR.threat.reg[,13], decreasing=F),]

# freshwater removals (% of internal resources)
freshwrem <- read.table("freshwrem.csv", sep=",", header=T)
freshwrem.reg <- merge(freshwrem, contreg, by="cntry.code")
freshwrem.sort <- freshwrem.reg[order(freshwrem.reg[,2], decreasing=T),]
AFR.freshwrem.reg <- subset(freshwrem.reg, cont=="AFR")
AFR.freshwrem.sort <- AFR.freshwrem.reg[order(AFR.freshwrem.reg[,2], decreasing=F),]

# forest loss 2000 - 2012 (Hansen Science 2013)
forestloss <- read.table("forestloss.csv", sep=",", header=T)
forestloss.reg <- merge(forestloss, contreg, by="cntry.code")
landarea <- read.table("land.area.csv", sep=",", header=T)
forestlossla.reg <- merge(forestloss.reg, landarea, by="cntry.code")
AFR.forestloss.reg <- subset(forestlossla.reg, cont=="AFR")
AFR.forestloss.reg$netloss.area <- (AFR.forestloss.reg$tot.loss - AFR.forestloss.reg$tot.gain)/AFR.forestloss.reg$land.area15
AFR.forestloss.sort <- AFR.forestloss.reg[order(AFR.forestloss.reg[,18], decreasing=F),]

# FHI (forest harvest index; Furukawa et al. 2015)
FHI.AFR <- read.table("wood.data.csv", sep=",", header=T)
FHI.AFR$FHI.tot <- apply(FHI.AFR[,c(5,6)],1,sum,na.rm=T)
FHI.AFR$prod.tot <- apply(FHI.AFR[,c(3,4)],1,sum,na.rm=T)
GFCL.FHI.fit <- lm(log10(FHI.AFR$FHI.tot) ~ log10(FHI.AFR$GFCL))

# livestock (cattle, pigs, buffaloes, sheep, goats) per ha of arable land
livestock <- read.table("livestock.csv", sep=",", header=T)
livestock.tot <- xtabs(livestock$no.ha ~ livestock$country)
livestock.tot.dat <- data.frame(names(livestock.tot), as.numeric(livestock.tot))
colnames(livestock.tot.dat) <- c("country", "livestock.tot")
cntry.unique <- as.data.frame(unique(livestock$country))
colnames(cntry.unique) <- "country"
cntry.codes <- (livestock[, c(1,2)])
cntry.code.unique <- unique(merge(cntry.unique, cntry.codes, by="country", all=F))
livestock.tot.out <- merge(livestock.tot.dat, cntry.code.unique, by="country")
livestock.tot.reg <- merge(livestock.tot.out, contreg, by="cntry.code")
AFR.livestock <- subset(livestock.tot.reg, cont=="AFR")
AFR.livestock.sort <- AFR.livestock[order(AFR.livestock[,3], decreasing=F),]

livestock.all <- read.table("alllivestock.csv", sep=",", header=T) 
livestock.all.reg <- merge(livestock.all, contreg, by="country")
AFR.livestock.all <- subset(livestock.all.reg, cont=="AFR")
AFR.livestock.all$cntry.code <- factor(AFR.livestock.all$cntry.code)
unique(AFR.livestock.all$tem)
AFR.livestock.red <- subset(AFR.livestock.all, tem=="Cattle" | tem=="Pigs" | tem=="Buffaloes" | tem=="Sheep" | tem=="Goats")
AFR.livestock.red$tem <- factor(AFR.livestock.red$tem)

## arable land (to estimate total livestock numbers)
## ha arable land in 2014
arable <- read.table("arableland.csv", sep=",", header=T)
arable.reg <- merge(arable, contreg, by="cntry.code")
AFR.arable <- subset(arable.reg, cont="AFR")
AFR.livestock.num <- merge(AFR.livestock.sort, AFR.arable)
AFR.livestock.num$livestock.num <- round(AFR.livestock.num$arable2014 * AFR.livestock.num$livestock.tot, 0)
AFR.livestock.final <- AFR.livestock.num[!(AFR.livestock.num$cntry.code %in% smisnat),]

# permanent cropland (% of total land area; World Bank)
setwd("~/Documents/Papers/Books/Jigsaw Utopia/chapters/Chapter 11 - Agriculture & Food Supply/data/World Bank data/") # SET ACCORDINGLY
cropland <- read.table("cropland.csv", sep=",", header=T)
cropland.reg <- merge(cropland, contreg, by="cntry.code")
AFR.cropland.reg <- subset(cropland.reg, cont=="AFR")
AFR.cropland.sort <- AFR.cropland.reg[order(AFR.cropland.reg[,2], decreasing=F),]
AFR.cropland.sort[1:15, ]

# using arable land as denominator instead
AFR.cropland.arable <- merge(AFR.cropland.reg, AFR.arable, by="cntry.code")
AFR.cropland.arable2 <- merge(AFR.cropland.arable, landarea, by="cntry.code")
AFR.cropland.arable2$clp.arable <- ((AFR.cropland.arable2$cropland14/100)*AFR.cropland.arable2$land.area15) / AFR.cropland.arable2$arable2014

# greenhouse gas emissions (CO2e 2013 per capita) World Bank
emissions <- read.table("pcemiss.csv", sep=",", header=T)
emissions.reg <- merge(emissions, contreg, by="cntry.code")
AFR.emissions.reg <- subset(emissions.reg, cont=="AFR")
AFR.emissions.sort <- AFR.emissions.reg[order(AFR.emissions.reg[,2], decreasing=F),]


# rank, sort, combine, remove small-island nations
# ecological footprint
AFR.EFcons.list <- AFR.EFcons.reg[!(AFR.EFcons.reg$cntry.code %in% smisnat),]
AFR.EFcons.list$EFrank <- rank(AFR.EFcons.list$EFconsPC12, na.last="keep", ties.method="average")
EFrank.dat <- AFR.EFcons.list[,c(1,13)]
EFsc <- as.numeric(scale(AFR.EFcons.list$EFconsPC12, center=T, scale=T))
hist(EFsc, col="black", main="ecological footprint", border="white", xlab="scaled/centred value", ylab="frequency")
EFsc.dat <- data.frame(AFR.EFcons.list[,1], EFsc) 
colnames(EFsc.dat)[1] <- "cntry.code"
EF.all <- merge(EFrank.dat,EFsc.dat,by="cntry.code")

# megafauna conservation index
AFR.mci.list <- AFR.mci.reg[!(AFR.mci.reg$cntry.code %in% smisnat),]
AFR.mci.list$MCIrank <- rank(-AFR.mci.list$MCI, na.last="keep", ties.method="average")
MCIrank.dat <- AFR.mci.list[,c(1,15)]
MCIsc <- as.numeric(scale(-AFR.mci.list$MCI, center=T, scale=T))
hist(MCIsc, col="black", main="megafauna conservation index", border="white", xlab="scaled/centred value", ylab="frequency")
MCIsc.dat <- data.frame(AFR.mci.list[,1], MCIsc) 
colnames(MCIsc.dat)[1] <- "cntry.code"
MCI.all <- merge(MCIrank.dat,MCIsc.dat,by="cntry.code")

# threatened species
AFR.thr.list <- AFR.threat.reg[!(AFR.threat.reg$cntry.code %in% smisnat),]
AFR.thr.list$THRrank <- rank(AFR.thr.list$prop.threat, na.last="keep", ties.method="average")
THRrank.dat <- AFR.thr.list[,c(1,18)]
THRsc <- as.numeric(scale(AFR.thr.list$prop.threat, center=T, scale=T))
hist(THRsc, col="black", main="proportion threatened species", border="white", xlab="scaled/centred value", ylab="frequency")
THRsc.dat <- data.frame(AFR.thr.list[,1], THRsc) 
colnames(THRsc.dat)[1] <- "cntry.code"

# freshwater removal
AFR.fwr.list <- AFR.freshwrem.reg[!(AFR.freshwrem.reg$cntry.code %in% smisnat),]
AFR.fwr.list$FWRrank <- rank(AFR.fwr.list$freshw.rem14, na.last="keep", ties.method="average")
FWRrank.dat <- AFR.fwr.list[,c(1,7)]
FWRsc <- as.numeric(scale(AFR.fwr.list$freshw.rem14, center=T, scale=T))
hist(FWRsc, col="black", main="freshwater removal", border="white", xlab="scaled/centred value", ylab="frequency")
FWRsc.dat <- data.frame(AFR.fwr.list[,1], FWRsc) 
colnames(FWRsc.dat)[1] <- "cntry.code"

# forest loss
AFR.fsl.list <- AFR.forestloss.reg[!(AFR.forestloss.reg$cntry.code %in% smisnat),]
AFR.fsl.list$FSLrank <- rank(AFR.fsl.list$netloss.area, na.last="keep", ties.method="average")
FSLrank.dat <- AFR.fsl.list[,c(1,20)]
FSLsc <- as.numeric(scale(AFR.fsl.list$netloss.area, center=T, scale=T))
hist(FSLsc, col="black", main="forest loss", border="white", xlab="scaled/centred value", ylab="frequency")
FSLsc.dat <- data.frame(AFR.fsl.list[,1], FSLsc) 
colnames(FSLsc.dat)[1] <- "cntry.code"

# forest harvest index
AFR.fhi.list <- FHI.AFR[!(FHI.AFR$cntry.code %in% smisnat),]
AFR.fhi.list$FHIrank <- rank(AFR.fhi.list$FHI.tot, na.last="keep", ties.method="average")
FHIrank.dat <- AFR.fhi.list[,c(2,10)]
FHIsc <- as.numeric(scale(AFR.fhi.list$FHI.tot, center=T, scale=T))
hist(FHIsc, col="black", main="forest harvest", border="white", xlab="scaled/centred value", ylab="frequency")
FHIsc.dat <- data.frame(AFR.fhi.list[,1], FHIsc) 
colnames(FHIsc.dat)[1] <- "cntry.code"

# livestock
AFR.lvs.list <- AFR.livestock.sort[!(AFR.livestock.sort$cntry.code %in% smisnat),]
AFR.lvs.list$LVSrank <- rank(AFR.lvs.list$livestock.tot, na.last="keep", ties.method="average")
LVSrank.dat <- AFR.lvs.list[,c(1,8)]
LVSsc <- as.numeric(scale(AFR.lvs.list$livestock.tot, center=T, scale=T))
hist(LVSsc, col="black", main="livestock density", border="white", xlab="scaled/centred value", ylab="frequency")
LVSsc.dat <- data.frame(AFR.lvs.list[,1], LVSsc) 
colnames(LVSsc.dat)[1] <- "cntry.code"

# cropland
AFR.crp.list <- AFR.cropland.sort[!(AFR.cropland.sort$cntry.code %in% smisnat),]
AFR.crp.list$CRPrank <- rank(AFR.crp.list$cropland14, na.last="keep", ties.method="average")
CRPrank.dat <- AFR.crp.list[,c(1,7)]
CRPsc <- as.numeric(scale(AFR.crp.list$cropland14, center=T, scale=T))
hist(CRPsc, col="black", main="cropland extent", border="white", xlab="scaled/centred value", ylab="frequency")
CRPsc.dat <- data.frame(AFR.crp.list[,1], CRPsc) 
colnames(CRPsc.dat)[1] <- "cntry.code"

# emissions
AFR.emi.list <- AFR.emissions.sort[!(AFR.emissions.sort$cntry.code %in% smisnat),]
AFR.emi.list$EMIrank <- rank(AFR.emi.list$pcemiss13, na.last="keep", ties.method="average")
EMIrank.dat <- AFR.emi.list[,c(1,7)]
EMIsc <- as.numeric(scale(AFR.emi.list$pcemiss13, center=T, scale=T))
hist(EMIsc, col="black", main="emissions", border="white", xlab="scaled/centred value", ylab="frequency")
EMIsc.dat <- data.frame(AFR.emi.list[,1], EMIsc) 
colnames(EMIsc.dat)[1] <- "cntry.code"


#########################################
# merge
ranks1.dat1 <- merge(EFrank.dat, MCIrank.dat, by="cntry.code", all.x=T, all.y=T)
ranks1.dat2 <- merge(ranks1.dat1, THRrank.dat, by="cntry.code", all.x=T, all.y=T)
ranks1.dat3 <- merge(ranks1.dat2, FWRrank.dat, by="cntry.code", all.x=T, all.y=T)
ranks1.dat4 <- merge(ranks1.dat3, FSLrank.dat, by="cntry.code", all.x=T, all.y=T)
ranks1.dat5 <- merge(ranks1.dat4, LVSrank.dat, by="cntry.code", all.x=T, all.y=T)
ranks1.dat6 <- merge(ranks1.dat5, CRPrank.dat, by="cntry.code", all.x=T, all.y=T)
ranks1.dat <- merge(ranks1.dat6, EMIrank.dat, by="cntry.code", all.x=T, all.y=T)

# merge scaled datasets
ranks2.dat1 <- merge(EFsc.dat, MCIsc.dat, by="cntry.code", all.x=T, all.y=T)
ranks2.dat2 <- merge(ranks5.dat1, THRsc.dat, by="cntry.code", all.x=T, all.y=T)
ranks2.dat3 <- merge(ranks5.dat2, FWRsc.dat, by="cntry.code", all.x=T, all.y=T)
ranks2.dat4 <- merge(ranks5.dat3, FSLsc.dat, by="cntry.code", all.x=T, all.y=T)
ranks2.dat5 <- merge(ranks5.dat4, LVSsc.dat, by="cntry.code", all.x=T, all.y=T)
ranks2.dat6 <- merge(ranks5.dat5, CRPsc.dat, by="cntry.code", all.x=T, all.y=T)
ranks2.dat <- merge(ranks5.dat6, EMIsc.dat, by="cntry.code", all.x=T, all.y=T)


#########################################
#########################################
## CHOICE
## Choose which ranks.dat?
ranks.dat <- ranks1.dat
#########################################
#########################################

#####################################
# remove countries with < 8 metrics
cntry.vec <- names(table(ranks.dat$cntry.code))
lcntr <- length(cntry.vec)
flag.kp <- rep(0,lcntr)
for (i in 1:lcntr) {
  cntry.dat <- subset(ranks.dat, cntry.code == cntry.vec[i])
  flag.kp[i] <- ifelse(length(which(is.na(cntry.dat[,c(2:dim(ranks.dat)[2])])==F)) < 8, 0, 1)
}
flag.cntry <- data.frame(cntry.vec, flag.kp)
colnames(flag.cntry)[1] <- "cntry.code"
ranks.dat.mrg <- merge(ranks.dat, flag.cntry, by="cntry.code", all.x=T, all.y=T)
ranks.dat.enough8 <- subset(ranks.dat.mrg, flag.kp==1)

# median rank
ranks.dat.enough8$median.rank <- apply(ranks.dat.enough8[,2:dim(ranks.dat)[2]], 1, median, na.rm=T)
hist(ranks.dat.enough8$median.rank, col="black", main="environmental performance", border="white", xlab="scaled/centred value", ylab="frequency")

# geometric mean rank
ranks.dat.enough8$geom.rank <- 10^(apply(log10(ranks.dat.enough8[,2:dim(ranks.dat)[2]]), 1, mean, na.rm=T))
hist(ranks.dat.enough8$geom.rank)
plot(ranks.dat.enough8$median.rank, ranks.dat.enough8$geom.rank, pch=19)

# sort
ranks.dat.sort8 <- ranks.dat.enough8[order(ranks.dat.enough8[,dim(ranks.dat.enough8)[2]], decreasing=F),]
ranks.dat.sort8
ranks.dat.sort8b <- merge(ranks.dat.sort8, contreg, by="cntry.code")

## corelation matrix of component parts
env.cor.mat8 <- cor(na.omit(ranks.dat.sort8[,c(2:dim(ranks.dat)[2])]), method="kendall")
env.cor.mat8



#########################################
#########################################
## CHOICE
## Choose which ranks.dat?
ranks.dat <- ranks1.dat
#########################################
#########################################

#####################################
# remove countries with < 7 metrics
cntry.vec <- names(table(ranks.dat$cntry.code))
lcntr <- length(cntry.vec)
flag.kp <- rep(0,lcntr)
for (i in 1:lcntr) {
  cntry.dat <- subset(ranks.dat, cntry.code == cntry.vec[i])
  flag.kp[i] <- ifelse(length(which(is.na(cntry.dat[,c(2:dim(ranks.dat)[2])])==F)) < 7, 0, 1)
}
flag.cntry <- data.frame(cntry.vec, flag.kp)
colnames(flag.cntry)[1] <- "cntry.code"
ranks.dat.mrg <- merge(ranks.dat, flag.cntry, by="cntry.code", all.x=T, all.y=T)
ranks.dat.enough7 <- subset(ranks.dat.mrg, flag.kp==1)

# median rank
ranks.dat.enough7$median.rank <- apply(ranks.dat.enough7[,2:dim(ranks.dat)[2]], 1, median, na.rm=T)

# geometric mean rank
ranks.dat.enough7$geom.rank <- 10^(apply(log10(ranks.dat.enough7[,2:dim(ranks.dat)[2]]), 1, mean, na.rm=T))
plot(ranks.dat.enough7$median.rank, ranks.dat.enough7$geom.rank, pch=19)

# provide 'biodiversity-only' rank
ranks.dat.enough7$biodiv.geom.rank <- 10^(apply(log10(ranks.dat.enough7[,c(3,4,6)]), 1, mean, na.rm=T))
plot(ranks.dat.enough7$geom.rank, ranks.dat.enough7$biodiv.geom.rank, pch=19)
cor(na.omit(ranks.dat.enough7[,c(12,13)], method="kendall"))

# sort
ranks.dat.sort7 <- ranks.dat.enough7[order(ranks.dat.enough7[,dim(ranks.dat.enough7)[2]], decreasing=F),]
ranks.dat.sort7b <- merge(ranks.dat.sort7, contreg, by="cntry.code")

## corelation matrix of component parts
env.cor.mat7 <- cor(na.omit(ranks.dat.sort7[,c(2:dim(ranks.dat)[2])]), method="kendall")
env.cor.mat7



## SOCIO-ECONOMICS
## population density
popN <- read.table("pop.yr.csv", sep=",", header=T)
popN.land <- merge(popN, landarea, by="cntry.code")
popN.reg <- merge(popN.land, contreg, by="cntry.code")
popN.reg$popD15 <- popN.reg$X2015/popN.reg$land.area15
AFR.popN.reg <- subset(popN.reg, cont=="AFR")

## population growth rate
pop.gr <- read.table("pop.growth.yr.csv", sep=",", header=T)
pop.reg <- merge(pop.gr, contreg, by="cntry.code")
pop.reg$mean.gr <- rowMeans(pop.reg[,2:57], na.rm=T)
AFR.pop.reg <- subset(pop.reg, cont=="AFR")
popdat <- merge(AFR.popN.reg, AFR.pop.reg, by="cntry.code", all.x=T, all.y=T)
pop.dat <- popdat[!(popdat$cntry.code %in% smisnat),]
pop.dat$popD.rnk <- rank(pop.dat$popD15, na.last="keep", ties.method="average")
pop.dat$popGR.rnk <- rank(pop.dat$mean.gr, na.last="keep", ties.method="average")

# continuous, scaled
pop.dat$popD.sc <- scale(pop.dat$popD15, center=T, scale=T)
hist(pop.dat$popD.sc, col="black", main="population density", border="white", xlab="scaled/centred value", ylab="frequency")
pop.dat$popGR.sc <- scale(pop.dat$mean.gr, center=T, scale=T)


#########################################
#########################################
## CHOICE
# merge with env ranks data
# which ranks.sort (7 or 8)?
ranks.dat.sort <- ranks.dat.sort8
#########################################
#########################################

pop.env <- merge(ranks.dat.sort, pop.dat, by="cntry.code", all.x=T, all.y=T)

## fertility
pop.dat <- read.table("PRB.pop.data.csv", sep=",", header=T)
fert.reg <- merge(pop.dat, contreg, by="cntry.code")
AFR.fert.reg <- subset(fert.reg, cont=="AFR")
fert.env <- merge(ranks.dat.sort, AFR.fert.reg, by="cntry.code", all.x=T, all.y=T)

## wealth
## gross national income
gni <- read.table("gnipc.csv", sep=",", header=T)
gni.reg <- merge(gni, contreg, by="cntry.code")
AFRgni <- subset(gni.reg, cont=="AFR")
gni.env <- merge(ranks.dat.sort, AFRgni, by="cntry.code", all.x=T, all.y=T)

## PPP-adjusted per-capita gross domestic product
gdppcppp <- read.table("gdppcppp.csv", sep=",", header=T) # income share of lowest 10%
gdppcppp.reg <- merge(gdppcppp, contreg, by="cntry.code")
AFRgdppcppp <- subset(gdppcppp.reg, cont=="AFR")
AFR.gdppcppp <- AFRgdppcppp[!(AFRgdppcppp$cntry.code %in% smisnat),]
AFR.gdppcppp$GDP.rnk <- rank(-AFR.gdppcppp$gdppcppp.1115, na.last="keep", ties.method="average")
AFR.gdppcppp$GDP.sc <- scale(-AFR.gdppcppp$gdppcppp.1115, center=T, scale=T)
gdppcppp.env <- merge(ranks.dat.sort, AFR.gdppcppp, by="cntry.code", all.x=T, all.y=T)
gni.gdp <- merge(AFRgni, AFR.gdppcppp, by="cntry.code", all.x=T, all.y=T)

## Gini coefficient of wealth distribution
gini <- read.table("gini0514.csv", sep=",", header=T)
gini.reg <- merge(gini, contreg, by="cntry.code")
AFRgini <- subset(gini.reg, cont=="AFR")
AFR.gini <- AFRgini[!(AFRgini$cntry.code %in% smisnat),]
AFR.gini$GINI.rnk <- rank(AFR.gini$giniavg0514, na.last="keep", ties.method="average")
AFR.gini$GINI.sc <- scale(AFR.gini$giniavg0514, center=T, scale=T)
gini.env <- merge(ranks.dat.sort, AFR.gini, by="cntry.code", all.x=T, all.y=T)

## income share held by the lowest 10% (poverty)
incshl10 <- read.table("incshl10.csv", sep=",", header=T) # income share of lowest 10%
incshl10.reg <- merge(incshl10, contreg, by="cntry.code")
AFR.incshl10 <- subset(incshl10.reg, cont=="AFR")
incshl10.env <- merge(ranks.dat.sort, AFR.incshl10, by="cntry.code", all.x=T, all.y=T)
gini.incshl10 <- merge(AFR.gini, AFR.incshl10, by="cntry.code", all.x=T, all.y=T)

## poverty gap (Poverty gap at national poverty lines is the mean shortfall 
## from the poverty lines (counting the nonpoor as having zero shortfall) as 
## a percentage of the poverty lines. This measure reflects the depth of poverty as well as its incidence)
povgap <- read.table("povgap.csv", sep=",", header=T)
povgap.reg <- merge(povgap, contreg, by="cntry.code")
AFRpovgap <- subset(povgap.reg, cont=="AFR")
AFR.povgap <- AFRpovgap[!(AFRpovgap$cntry.code %in% smisnat),]
AFR.povgap$POVGAP.rnk <- rank(AFR.povgap$povgap0615, na.last="keep", ties.method="average")
AFR.povgap$POVGAP.sc <- scale(AFR.povgap$povgap0615, center=T, scale=T)
povgap.env <- merge(ranks.dat.sort, AFR.povgap, by="cntry.code", all.x=T, all.y=T)
plot((povgap.env$povgap0615), povgap.env$median.rank, pch=19, xlab="poverty gap", ylab="median environmental performance rank")
AFR.povgap.gini <- merge(AFR.povgap, AFR.gini, by="cntry.code", all.x=T, all.y=T)
fit.AFR.pggini <- lm(AFR.povgap.gini$giniavg0514 ~ AFR.povgap.gini$povgap06)
cor(na.omit(AFR.povgap.gini[,c(7,13)]), method="kendall")
povgap.gini <- (merge(AFR.povgap, AFR.gini, by="cntry.code", all.x=T, all.y=T))
povgapgdp <- (merge(AFR.povgap, AFR.gdppcppp, by="cntry.code", all.x=T, all.y=T))
povgap.gdp <- povgapgdp[!(povgapgdp$cntry.code %in% smisnat),]

## literacy/education
## Percentage of the population age 15 and above who can, with understanding, read and write a short, simple statement
## on their everyday life. Generally, ‘literacy’ also encompasses ‘numeracy’, the ability to make simple arithmetic 
## calculations. This indicator is calculated by dividing the number of literates aged 15 years and over by the corresponding 
## age group population and multiplying the result by 100
adlit <- read.table("adlit.csv", sep=",", header=T)
adlit.reg <- merge(adlit, contreg, by="cntry.code")
AFRadlit <- subset(adlit.reg, cont=="AFR")
AFR.adlit <- AFRadlit[!(AFRadlit$cntry.code %in% smisnat),]
AFR.adlit$ADLIT.rnk <- rank(-AFR.adlit$adlit0615, na.last="keep", ties.method="average")
AFR.adlit$ADLIT.sc <- scale(-AFR.adlit$adlit0615, center=T, scale=T)
adlit.env <- merge(ranks.dat.sort, AFR.adlit, by="cntry.code", all.x=T, all.y=T)
gdp.adlit <- merge(AFR.gdppcppp, AFR.adlit, by="cntry.code", all.x=T, all.y=T)
gdp.adlit.list <- gdp.adlit[!(gdp.adlit$cntry.code %in% smisnat),]

## young (15-24) female literacy
yfemlit <- read.table("yfemlit.csv", sep=",", header=T)
yfemlit.reg <- merge(yfemlit, contreg, by="cntry.code")
AFR.yfemlit <- subset(yfemlit.reg, cont=="AFR")
yfemlit.env <- merge(ranks.dat.sort, AFR.yfemlit, by="cntry.code", all.x=T, all.y=T)

## Ibrahim Index of African Governance
## Money, Politics & Transparency
iiag.dat <- read.table("overallgovernance.csv", sep=",", header=T)
iiag.2015 <- subset(iiag.dat, year==2015)
iiag.2015$cntr.code2 <- as.character(iiag.2015$cntr.code2)
iiag.2015$cntr.code2[which(iiag.2015$country == "Namibia")] <- c("NM") # fix 2-code Namibia from NA to NM
cc2to3 <- read.table("cc2to3.csv", sep=",", header=T)
iiag <- merge(iiag.2015, cc2to3, by="cntr.code2", all.x=T, all.y=T)
iiag.list <- iiag[!(iiag$cntry.code %in% smisnat),]
iiagsort <- iiag.list[order(iiag.list[,4], decreasing=T),]
iiag.sort <- iiagsort[!(iiagsort$cntry.code %in% smisnat),]
iiag.sort$GOV.rnk <- rank(-iiag.sort$governance, na.last="keep", ties.method="average")
iiag.sort$GOV.sc <- scale(-iiag.sort$governance, center=T, scale=T)
hist(iiag.sort$GOV.sc, col="black", main="Governance Quality", border="white", xlab="scaled/centred value", ylab="frequency")
iiag.env <- merge(ranks.dat.sort, iiag.sort, by="cntry.code", all.x=T, all.y=T)


## relationship between ENV ranks and proportion of land protected
## re-rank land protected
AFRlandprot <- AFR.landprot.sort[!(AFR.landprot.sort$cntry.code %in% smisnat),]
AFRlandprot$PROTr <- rank(AFRlandprot$rank, na.last="keep", ties.method="average")
AFRlandprot$PROTsc <- scale(AFRlandprot$pclandprot, center=T, scale=T)
ranks.landprot <- merge(ranks.dat.sort, AFRlandprot, by="cntry.code")
xy <- data.frame(ranks.landprot$PROTr, ranks.landprot$median.rank)
colnames(xy) <- c("x", "y")
fit.lin.xy <- lm(y~x, data=xy)
summary(fit.lin.xy)



## STRUCTURAL EQUATION MODELS
# model structure
# ENV.rnk ~ popD + popGR + GDP + GINI + GOV + PROT

# combine all data into one data.frame
dat.sem.1 <- merge(pop.env, AFR.gdppcppp, by="cntry.code", all.x=T, all.y=T)
dat.sem.2 <- merge(dat.sem.1, AFR.gini, by="cntry.code", all.x=T, all.y=T)
dat.sem.3 <- merge(dat.sem.2, AFR.adlit, by="cntry.code", all.x=T, all.y=T)
dat.sem.4 <- merge(dat.sem.3, iiag.sort, by="cntry.code", all.x=T, all.y=T)
dat.sem.5 <- merge(dat.sem.4, chinaide.reg, by="cntry.code", all.x=T, all.y=T)
dat.sem.6 <- merge(dat.sem.5, AFRlandprot, by="cntry.code", all.x=T, all.y=T)

## all components in ENV included
# median rank
dat.sem.7 <- data.frame(dat.sem.6$cntry.code, dat.sem.6$median.rank, dat.sem.6$popD.sc, dat.sem.6$popGR.sc, dat.sem.6$GDP.sc, dat.sem.6$GINI.sc, dat.sem.6$ADLIT.sc, dat.sem.6$GOV.sc, dat.sem.6$AID.sc, dat.sem.6$PROTsc)
# geometric rank
#dat.sem.7 <- data.frame(dat.sem.6$cntry.code, dat.sem.6$geom.rank, dat.sem.6$popD.rnk, dat.sem.6$popGR.rnk, dat.sem.6$GDP.rnk, dat.sem.6$GINI.rnk, dat.sem.6$ADLIT.rnk, dat.sem.6$GOV.rnk, dat.sem.6$AID.rnk, dat.sem.6$PROTr)

colnames(dat.sem.7) <- c("cntry.code", "ENVr", "popDr", "popGRr", "GDPr", "GINIr", "ADLITr", "GOVr", "AIDr", "PROTr")
dat.sem <- dat.sem.7[!(dat.sem.7$cntry.code %in% smisnat),]

# remove NAs
datsem <- na.omit(dat.sem)
head(datsem)

# covariance matrix for path analysis
cov.mat <- cov(model.matrix(~ ENVr + popDr + popGRr + GDPr + GINIr + GOVr + PROTr, data=datsem))[-1,-1]

# correlation matrix
cor.mat <- cor(na.omit(datsem[,c(2:8,10)]), method="kendall")


## models
# saturated model
mod1 <- specifyModel()
  popDr -> ENVr, beta1, NA
  popGRr -> ENVr, beta2, NA
  GDPr -> ENVr, beta3, NA
  GINIr -> ENVr, beta4, NA
  GOVr -> ENVr, beta5, NA
  PROTr -> ENVr, beta6, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr -> GOVr, pi3, NA
  GOVr -> PROTr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA
  
# model 2 only popD
mod2 <- specifyModel()
  popDr -> ENVr, beta1, NA
  popDr -> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr <-> GOVr, pi3, NA
  GOVr -> PROTr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA
  
# only popGRr
mod3 <- specifyModel()
  popGRr -> ENVr, beta1, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr -> GOVr, pi3, NA
  GOVr -> PROTr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA
  
# GDP only
mod4 <- specifyModel()
  GDPr -> ENVr, beta1, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr -> GOVr, pi3, NA
  GOVr -> PROTr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA
  
# model 5 popD & GDP
mod5 <- specifyModel()
  popDr -> ENVr, beta1, NA
  GDPr -> ENVr, beta2, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr -> GOVr, pi3, NA
  GOVr -> PROTr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA
  
# model 6 only GINI
mod6 <- specifyModel()
  GINIr -> ENVr, beta1, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr -> GOVr, pi3, NA
  GOVr -> PROTr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA
  
# model 7 only GOV
mod7 <- specifyModel()
  GOVr -> ENVr, beta1, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr <-> GOVr, pi3, NA
  GOVr -> PROTr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA
  
# model 8 popD, GDP & GINI
mod8 <- specifyModel()
  popDr -> ENVr, beta1, NA
  GDPr -> ENVr, beta2, NA
  GINIr -> ENVr, beta3, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr -> GOVr, pi3, NA
  GOVr -> PROTr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA
  
# model 9 popD, GDP, GINI, PROT
mod9 <- specifyModel()
  popDr -> ENVr, beta1, NA
  GDPr -> ENVr, beta2, NA
  GINIr -> ENVr, beta3, NA
  PROTr -> ENVr, beta4, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr -> GOVr, pi3, NA
  PROTr <- GOVr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA

# model 10 only PROT
mod10 <- specifyModel()
  PROTr -> ENVr, beta1, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr <-> GOVr, pi3, NA
  GOVr -> PROTr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA
  
# model 11 popD, PROT
mod11 <- specifyModel()
  popDr -> ENVr, beta1, NA
  PROTr -> ENVr, beta2, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr -> GOVr, pi3, NA
  PROTr <- GOVr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA

# model 12 popD, GDP, GINI, GOV
mod12 <- specifyModel()
  popDr -> ENVr, beta1, NA
  GDPr -> ENVr, beta2, NA
  GINIr -> ENVr, beta3, NA
  GOVr -> ENVr, beta4, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr -> GOVr, pi3, NA
  PROTr <- GOVr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA

# model 13 popD & GOV
mod13 <- specifyModel()
  popDr -> ENVr, beta1, NA
  GOVr -> ENVr, beta2, NA
  popDr <-> popGRr, pi1, NA
  popDr -> GDPr, pi2, NA
  popDr -> GOVr, pi3, NA
  GOVr -> PROTr, pi4, NA
  GDPr <-> GOVr, pi5, NA
  GINIr <-> GOVr, pi6, NA
  popDr <-> popDr, gam1, NA
  popGRr <-> popGRr, gam2, NA
  GDPr <-> GDPr, gam3, NA
  GINIr <-> GINIr, gam4, NA
  GOVr <-> GOVr, gam5, NA
  PROTr <-> PROTr, gam6, NA
  ENVr <-> ENVr, gam7, NA
  
# model labels for output table
  model1 <- "POPD+POPG+GDP+GINI+GOV+PROT"
  model2 <- "POPD"
  model3 <- "POPG"
  model4 <- "GDP"
  model5 <- "POPD+GDP"
  model6 <- "GINI"
  model7 <- "GOV"
  model8 <- "POPD+GDP+GINI"
  model9 <- "POPD+GDP+GINI+PROT"
  model10 <- "PROT"
  model11 <- "POPD+PROT"
  model12 <- "POPD+GDP+GINI+GOV"
  model13 <- "POPD+GOV"
mod.lab.vec <- c(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13)  

  
  ## fit path models
  sem.mod1 <- sem(mod1, cov.mat, dim(datsem)[1])
  sem.mod2 <- sem(mod2, cov.mat, dim(datsem)[1])
  sem.mod3 <- sem(mod3, cov.mat, dim(datsem)[1])
  sem.mod4 <- sem(mod4, cov.mat, dim(datsem)[1])
  sem.mod5 <- sem(mod5, cov.mat, dim(datsem)[1])
  sem.mod6 <- sem(mod6, cov.mat, dim(datsem)[1])
  sem.mod7 <- sem(mod7, cov.mat, dim(datsem)[1])
  sem.mod8 <- sem(mod8, cov.mat, dim(datsem)[1])
  sem.mod9 <- sem(mod9, cov.mat, dim(datsem)[1])
  sem.mod10 <- sem(mod10, cov.mat, dim(datsem)[1])
  sem.mod11 <- sem(mod11, cov.mat, dim(datsem)[1])
  sem.mod12 <- sem(mod12, cov.mat, dim(datsem)[1])
  sem.mod13 <- sem(mod13, cov.mat, dim(datsem)[1])
  
  # plots
  #library(qgraph)
  #qgraph(sem.mod8, layout="spring", label.cex=1.3)

  library(semPlot)
  semPaths(sem.mod8, what="col", whatLabels="stand", layout="spring", label.cex=1.3, edge.label.cex=1.3, nCharNodes=5)
  
  # summaries
  sum1 <- summary(sem.mod1)
  sum2 <- summary(sem.mod2)
  sum3 <- summary(sem.mod3)
  sum4 <- summary(sem.mod4)
  sum5 <- summary(sem.mod5)
  sum6 <- summary(sem.mod6)
  sum7 <- summary(sem.mod7)
  sum8 <- summary(sem.mod8)
  sum9 <- summary(sem.mod9)
  sum10 <- summary(sem.mod10)
  sum11 <- summary(sem.mod11)
  sum12 <- summary(sem.mod12)
  sum13 <- summary(sem.mod13)
  
  # standardised coefficients
  stdCoef(sem.mod1)
  stdCoef(sem.mod2)
  stdCoef(sem.mod3)
  stdCoef(sem.mod4)
  stdCoef(sem.mod5)
  stdCoef(sem.mod6)
  stdCoef(sem.mod7)
  stdCoef(sem.mod8, digits=3)
  stdCoef(sem.mod9, digits=3)
  stdCoef(sem.mod10)
  stdCoef(sem.mod11)
  stdCoef(sem.mod12)
  stdCoef(sem.mod13)
  
  # SEM goodness-of-fit
  # a cutoff value close to .95 for TLI, BL89, CFI, RNI, and Gamma Hat;
  # a cutoff value close to .90 for Mc; a cutoff value close to .08 for SRMR;
  # and a cutoff value close to .06 for RMSEA
  # incremental fit index, also known as Bollen's IFI, is also relatively insensitive to sample size.
  # Values that exceed .90 are regarded as acceptable, although this index can exceed 1.
  # To compute the IFI, first the difference between the chi square of the independence model--
  # in which variables are uncorrelated--and the chi-square of the target model is calculated.
  # Next, the difference between the chi-square of the target model and the df for the target model is calculated.
  # The ratio of these values represents the IFI.
  #### ***** note: might not function in later versions of R

  # McDonald's Non-Centrality Index
  Mc.vec <- c(summaryGOF(sem.mod1, digits=4)$Mc,summaryGOF(sem.mod2, digits=4)$Mc,summaryGOF(sem.mod3, digits=4)$Mc,summaryGOF(sem.mod4, digits=4)$Mc,summaryGOF(sem.mod5, digits=4)$Mc,summaryGOF(sem.mod6, digits=4)$Mc,summaryGOF(sem.mod7, digits=4)$Mc,summaryGOF(sem.mod8, digits=4)$Mc,summaryGOF(sem.mod9, digits=4)$Mc,summaryGOF(sem.mod10, digits=4)$Mc,summaryGOF(sem.mod11, digits=4)$Mc,summaryGOF(sem.mod12, digits=4)$Mc,summaryGOF(sem.mod13, digits=4)$Mc)
  # Bollen's Incremental Fit Index
  IFI.vec <- c(summaryGOF(sem.mod1, digits=4)$IFI,summaryGOF(sem.mod2, digits=4)$IFI,summaryGOF(sem.mod3, digits=4)$IFI,summaryGOF(sem.mod4, digits=4)$IFI,summaryGOF(sem.mod5, digits=4)$IFI,summaryGOF(sem.mod6, digits=4)$IFI,summaryGOF(sem.mod7, digits=4)$IFI,summaryGOF(sem.mod8, digits=4)$IFI,summaryGOF(sem.mod9, digits=4)$IFI,summaryGOF(sem.mod10, digits=4)$IFI,summaryGOF(sem.mod11, digits=4)$IFI,summaryGOF(sem.mod12, digits=4)$IFI,summaryGOF(sem.mod13, digits=4)$IFI)

  # summary table
  mod.lab <- 1:13
  df.vec <- c(sum1$df, sum2$df, sum3$df, sum4$df, sum5$df, sum6$df, sum7$df, sum8$df, sum9$df, sum10$df, sum11$df, sum12$df, sum13$df)
  chisq.vec <- c(sum1$chisq, sum2$chisq, sum3$chisq, sum4$chisq, sum5$chisq, sum6$chisq, sum7$chisq, sum8$chisq, sum9$chisq, sum10$chisq, sum11$chisq, sum12$chisq, sum13$chisq)

  # BIC ranks
  delta.IC <- function(x) x - min(x) ## where x is a vector of an IC
  weight.IC <- function(x) (exp(-0.5*x))/sum(exp(-0.5*x)) ## Where x is a vector of dIC
  
  BIC.vec <- c(sum1$BIC, sum2$BIC, sum3$BIC, sum4$BIC, sum5$BIC, sum6$BIC, sum7$BIC, sum8$BIC, sum9$BIC, sum10$BIC, sum11$BIC, sum12$BIC, sum13$BIC)
  dBIC.vec <- delta.IC(BIC.vec)
  wBIC.vec <- weight.IC(dBIC.vec)
  wBIC.vec
  
  # results dataframe
  # table<-cbind(mod.lab,df.vec,chisq.vec,BIC.vec,dBIC.vec,wBIC.vec,Mc.vec,IFI.vec)
  # colnames(table)<-c("model","df","chisq","BIC","dBIC","wBIC","Mc","IFI")
  table<-cbind(mod.lab,df.vec,chisq.vec,BIC.vec,dBIC.vec,wBIC.vec)
  colnames(table)<-c("model","df","chisq","BIC","dBIC","wBIC")
  rownames(table)<- mod.lab.vec
  
  # table sorted by wBIC
  summary.table<-table[order(table[,6],decreasing=TRUE),1:6]
  summary.table

  ## Boosted regression trees for scaled values (choose above accordingly)
  # "POPD+POPG+GDP+GINI+GOV+PROT"
  plot(datsem[,3],datsem[,2],pch=19, xlab="population density", ylab='environmental performance')
  brt.fit <- gbm.step(datsem, gbm.x = attr(datsem, "names")[c(3:6,8,10)], gbm.y = attr(datsem, "names")[2], family="gaussian", tolerance = 0.0001, learning.rate = 0.0003, bag.fraction=0.8, tree.complexity = 2, tolerance.method = "auto", plot.main=T, plot.folds=F, max.trees=100000)
  summary(brt.fit)
  gbm.plot(brt.fit)
  tmp <- gbm.plot.fits(brt.fit, v=0)
  D2 <- 100 * (brt.fit$cv.statistics$deviance.mean - brt.fit$self.statistics$mean.resid) / brt.fit$cv.statistics$deviance.mean
  D2 # % deviance explained
  CV.cor <- 100 * brt.fit$cv.statistics$correlation.mean
  CV.cor.se <- 100 *brt.fit$cv.statistics$correlation.se
  print(c(CV.cor, CV.cor.se))
  
  brt.fit$gbm.call$gbm.x
  # POPD
  gbm.plot(brt.fit, variable.no=1, smooth=T, common.scale=T, write.title=F, y.label="fit", x.label="scaled/centred value", show.contrib=T, plot.layout=c(1,1), lwd=2)
  # POPG
  gbm.plot(brt.fit, variable.no=2, smooth=T, common.scale=T, write.title=F, y.label="fit", x.label="scaled/centred value", show.contrib=T, plot.layout=c(1,1), lwd=2)
  # GDP
  gbm.plot(brt.fit, variable.no=3, smooth=T, common.scale=T, write.title=F, y.label="fit", x.label="scaled/centred value", show.contrib=T, plot.layout=c(1,1), lwd=2)
  # GINI
  gbm.plot(brt.fit, variable.no=4, smooth=T, common.scale=T, write.title=F, y.label="fit", x.label="scaled/centred value", show.contrib=T, plot.layout=c(1,1), lwd=2)
  # PROT
  gbm.plot(brt.fit, variable.no=5, smooth=T, common.scale=T, write.title=F, y.label="fit", x.label="scaled/centred value", show.contrib=T, plot.layout=c(1,1), lwd=2)

