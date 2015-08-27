
1. run the regular diet script
2. run the biomass freq script

source("F:/DATA/SLBE/R scripts/Benthos biomass and frequency/biomassfreqscript.R")

3. run this guy:

d <- CompiledFishLogandDiets
d$Month <- month(d$Date)
d$Year <- as.factor(d$Year)
#d<- join(d, PCRw, by=c("Year","Event","Site"), type="left", match="first")
d$DayNum <- yday(d$Date)

c <- CompiledBenthosLogandFreq
#levels(c$Status)
selected<-c("Processed")
c <- c[c$Status %in% selected,]
c$Month <- month(c$Date)
c$DayNum <- yday(c$Date)
#c<-subset(c, Year >=2011)
c$Year <- as.factor(c$Year)

#Get the perc. weight with some estimations...
#First, how many biomass estimations are missing from the benthos data?
#length(which(is.na(d$Av.biomass.mg))) # How many missing
#length(which(!is.na(d$Av.biomass.mg))) # Not missing
#Proportion missing:
#length(which(is.na(d$Av.biomass.mg)))/(length(which(is.na(d$Av.biomass.mg)))+length(which(!is.na(d$Av.biomass.mg))))
#let's try to fill in some of these missing biomasses with benthos data!
# ben1<-join(NA.omit.biomass,CompiledBenthosLog,by="YearSER") ##dunno

#make a df of all the animals that have been measured
measured.benthos<-subset(c, Num.critters.averaged.4.biomass>="1")
#make a df of all the animals that havent been measured yet
unmeasured.benthos <-subset(c, is.na(c$ Num.critters.averaged.4.biomass))


avg.biomass <- summaryBy(Av.biomass.mg ~ Taxon,data=measured.benthos, FUN=c(mean))
avg.biomass.order<- summaryBy(Av.biomass.mg ~ Order,data=measured.benthos, FUN=c(mean))

names(avg.biomass)[names(avg.biomass)=="Av.biomass.mg.mean"]<-"biomass.mg.mean.general"
names(avg.biomass.order)[names(avg.biomass.order)=="Av.biomass.mg.mean"]<-"biomass.mg.mean.general"


d<-join(d,avg.biomass.order, by=c("Order"))

d$est.final.biomass.mg<-ifelse(is.na(d$Sum.biomass.mg), d$biomass.mg.mean.general*d$Total.Organisms.sum, d$Sum.biomass.mg)

#what diet items are still NA
unmeasured.diet.items <-subset(d, is.na(d$est.final.biomass.mg))
write.csv(unmeasured.diet.items, "unmeasured_diet.csv")

diet.fish.traps <-subset(d, Family=="Fish Spp")
attach(diet.fish.traps)
plot(Total.Time..decimal., Count.sum, main="Number of fish in goby diets vs trap time", 
     xlab="trap time ", ylab="number of fish in diet ", pch=19)



c<-join(c,avg.biomass.order,by=c("Order"))

c$est.final.biomass.mg<-ifelse(is.na(c$Sum.biomass.mg), c$biomass.mg.mean.general*c$Total.Organisms.sum, c$Sum.biomass.mg)
c<-c[,order(names(c))]
#Fill in estimates if none have been measured in benthos yet
#Bytho - Bilkovic and Lehman 1997 biomass for size1-4 Lake Michigan in ug
d$est.final.biomass.mg[d$Taxon =="Bythotrephes"] <- ifelse(
  is.na(d$est.final.biomass.mg[d$Taxon =="Bythotrephes"]),
  mean(c(133.9*0.001,621.6*0.001)), 
  d$est.final.biomass.mg[d$Taxon =="Bythotrephes"])

d$est.final.biomass.mg[d$Order=="Copepoda (sub class)"] <- ifelse(
  is.na(d$est.final.biomass.mg[d$Order=="Copepoda (sub class)"]),
  avbiobenOrd$biomass.mg.mean.general[avbiobenOrd$Order == "Copepoda (sub class)"],
  d$est.final.biomass.mg[d$Order=="Copepoda (sub class)"])

rm(avbiobenOrd, avbioben)