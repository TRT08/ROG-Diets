
###################
#####        ######
#####READ ME ######
#####        ######
###################

#  RUN THESE FIRST TO GET THE NECESSARY FILES FOR THIS SCRIPT TO RUN ########
setwd("F:/DATA/SLBE/R scripts/Diet biomass and frequency/")#set directory
source("dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.diets<- NA.omit.biomass
rm(NA.omit.biomass)
setwd("F:/DATA/SLBE/R scripts/ROG CPUE/")#set directory
source("compiledataforCPUE.R",echo=TRUE, max.deparse.length=10000)
setwd("F:/DATA/SLBE/R scripts/Benthos biomass and frequency/")#set directory
source("biomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.benthos<- NA.omit.biomass
rm(NA.omit.biomass)
setwd("F:/DATA/SLBE/R scripts/Diet analysis")#set directory 


library(doBy)
library(reshape)
library("ggplot2")
library(vegan)
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)
library(extrafont)
library(ggplot2)
library(plyr)
library(outliers)
library(forams)
library(FSA)
#if you don't have FSA installed it is a little tricky...ask TRT for help

#####SET UP THEME FOR YOUR GRAPHS~!!! GGPLOT!!! ##########
#font_import(pattern='times') #Get Times New Roman
#loadfonts(device = "win")

Goby_theme <- theme_bw()+theme(
text = element_text(family="Times New Roman", colour="black", size=14))+
theme(panel.grid.major = element_blank())+
theme(panel.grid.minor = element_blank())+
theme(axis.line = element_line(colour = "black"))+
theme(legend.key = element_blank())+
theme(axis.title.y = element_text(vjust=1))+ 
theme(strip.background = element_rect(fill = 'gray97'))

Goby_pres_theme <- theme_bw()+theme(
  text = element_text(colour="black", size=17))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.key = element_blank())+
  theme(axis.title.y = element_text(vjust=1))+ 
  theme(strip.background = element_rect(fill = 'gray97'))

##########################################################  

###RUN FIRST####
names(CompiledFishLogandDiets)
d <- CompiledFishLogandDiets
d$Month <- month(d$Date)
f<- CompiledFishLog
f$Month <- month(f$Date)
#Extract processed fish only
levels(f$Status)
f <- f[which(f$Status %in% c('Frozen - Stomach Removed - Stomach Processed','Processed')), ]
######

########################################################################################################
#Check diet items for outliers in length(mm) once multiplied by the microscope multiplier
outties <- fishdiet2[,c(1,9,19,20,23)]
dietschecked<-read.csv("dietoutord.csv")
names(outties)[names(outties)=="Length"]<- "Microscope.Length"
source("FindOutliers.R",echo=TRUE, max.deparse.length=10000)
dietoutord<-outord
dietsout<- join(dietoutord,dietschecked, type="left")
#dietsout <- dietsout[-which(dietsout$Checked==1), ]

########################################################################################################
#Get new equations for TL based on our HW data

# Get benthos/scope data from Database
db <- "F:/DATA/SLBE/AVBOT Database.accdb"
con2 <- odbcConnectAccess2007(db)
RawBenthosLog <- sqlFetch(con2, "Inventory Control Log - Benthos")
RawBenthosData <- sqlFetch(con2, "Compiled Benthic Analysis")
RawBenthosTaxa <- sqlFetch(con2, "Potential Benthos Taxon List")
Rawscopes <- sqlFetch(con2, "Microscope Magnifications")
close(con2)
colnames(RawBenthosLog) <- make.names(colnames(RawBenthosLog), unique = TRUE)
colnames(RawBenthosData) <- make.names(colnames(RawBenthosData), unique = TRUE)
colnames(RawBenthosTaxa) <- make.names(colnames(RawBenthosTaxa), unique = TRUE)
colnames(Rawscopes) <- make.names(colnames(Rawscopes), unique = TRUE)
#######

ben <- subset(RawBenthosData, select = -1)
names(ben)[names(ben)=="Taxon"] <- "ID"  #Prep for joining
ben <- join(ben, RawBenthosTaxa, by ="ID", type = "left", match = "all")
###Take out only the animals with TL/HW
TLHW <- subset(ben, Family == "Chironomidae" | Family == "Asellidae")
#Melt diet data so each measured animal has its own row--make sure to include all columns except the "X1","X2",etc.
meltedben<- melt(TLHW, id=c("YearSER","Total.Organisms", "Count", "Scope.Name",  "Mag" ,  "Notes","ID","TaxonomicGroup","Taxon" ,"Order", "Family" ,"a","b","c",     
                           "factor","Mass.Unit","Source","Equation"))
#Change names to melt with microscope magnifiers
names(meltedben)[names(meltedben)=="variable"] <- "Animal.measured"
names(meltedben)[names(meltedben)=="value"] <- "Length"
#Merge scopes multiplier and data
ben2 <- join(meltedben, Rawscopes, by = c("Scope.Name","Mag"), type = "left", match = "all")
ben2 <- ben2[ , -which(names(ben2) %in% c("ID"))]
#Find true length of organisms (in mm)
ben2$Length <- as.numeric(ben2$Length, na.rm = TRUE)
ben2$mm.length <- ben2$Length * ben2$Multiplier

###Line up the HW and TL
TLHW <- cast(ben2,YearSER + Animal.measured ~ Taxon, fun.aggregate=mean, value="mm.length")
colnames(TLHW) <- make.names(colnames(TLHW), unique = TRUE)

#Get best fit equations
names(TLHW)
plot(TLHW$Chironominae.TL, TLHW$Chironominae.HW)
fit <- lm(Chironominae.TL ~ Chironominae.HW, data=TLHW)
summary(fit)

########################################################################################################

#Total Number of fish diets / year
table(f$Year.1)

##Plot - Get histograms of fish lengths usef per year for diets
f2010 <- f[which(f$Year.1=='2010'), ]
f2011 <- f[which(f$Year.1=='2011'), ]
f2012 <- f[which(f$Year.1=='2012'), ]
f2013 <- f[which(f$Year.1=='2013'), ]

hist(f2011$Lengthmm, breaks = 20,main="2011", xlim=c(0,200), ylim=c(0,100))
hist(f2012$Lengthmm, breaks = 20,main="2012", xlim=c(0,200), ylim=c(0,100))
hist(f2013$Lengthmm, breaks = 20,main="2013", xlim=c(0,200), ylim=c(0,50))

#Plot - Combined histogram/freq poly of lengths vs year
f$Year<- as.factor(f$Year.1)
myplot<- qplot(Lengthmm, data = f, geom = "freqpoly", binwidth = 5, colour = Year, xlab="Length (mm)",ylab="Count")
myplot + Goby_theme + scale_color_manual(values=c("#D6D6D6", "#808080", "#000000"))

###table size classes by year
#Two ways of viewing
summaryBy(SizeClass ~ Year.1 + SizeClass, data = f, FUN=length)
cast(f,  Year.1~SizeClass, fun.aggregate=length)

###Are there enough diets for each year? Rarefaction curves
freq <- cast(d, Year+ YearSERUnique ~ Family, value='Total.Organisms.sum', sum)
row.names(freq)<-freq$YearSERUnique
freq <- freq[ ,-which(names(freq) %in% c("YearSERUnique","???", NA, "Empty"))] ###Choose cols/animals to remove
freq <- freq[, colSums(freq == 0) != nrow(freq)] 
freq2011 <- freq[which(freq$Year=='2011'), ]
freq2012 <- freq[which(freq$Year=='2012'), ]
freq2013 <- freq[which(freq$Year=='2013'), ]
freq2011 <- freq2011[,-1]
freq2012 <- freq2012[,-1]
freq2013 <- freq2013[,-1]
freq2011<- data.matrix(freq2011)
freq2012<- data.matrix(freq2012)
freq2013<- data.matrix(freq2013)
rare2011<-specaccum(freq2011, method="rarefaction")
rare2012<-specaccum(freq2012, method="rarefaction")
rare2013<-specaccum(freq2013, method="rarefaction")
plot(rare2011, xlab="Number of stomachs needed", ylab="Number of Diet taxa", main="2011", ci.type = "polygon", ci.col="Gray", ci.lty=0)
plot(rare2012, xlab="Number of stomachs needed", ylab="Number of Diet taxa", main="2012", ci.type = "polygon", ci.col="Gray", ci.lty=0)
plot(rare2013, xlab="Number of stomachs needed", ylab="Number of Diet taxa", main="2013", ci.type = "polygon", ci.col="Gray", ci.lty=0)

####Bar Plot Diet Items over Time by Year
Grab<- d[ , c("Month","Order","Year","Total.Organisms.sum","SizeClass","GeneralLoc","Depth.m.standard","DepNonDep")]
agg <- aggregate(data=Grab, Total.Organisms.sum~ Year+Month+Order, function(x) sum(x))

SampleSize <- aggregate(data=d, YearSERUnique~ Year+Month, function(x) length(unique(x)))
plotovertime<- join(agg,SampleSize,by=c("Year","Month"))

ggplot(plotovertime, aes(x=Month, y=Total.Organisms.sum, fill=Order)) +
  geom_bar(stat="identity", position = "fill") +
 Goby_theme + scale_x_continuous(breaks=pretty_breaks(n=10))+
  labs(x="Month", y="Proportion of diet", title="Goby diet items over time")+
  facet_grid(Year ~ .)
###Or to export to excel to make easier graphs....
#write.csv(plotovertime,"plotovertime.csv")

###By size class ### THIS IS A REALLY NICE GRAPH!
agg2 <- aggregate(data=Grab, Total.Organisms.sum~ Year+Month+SizeClass+Order, function(x) sum(x))
ggplot(agg2, aes(x=Month, y=Total.Organisms.sum, fill=Order)) +
  geom_bar(stat="identity", position = "fill") +
  Goby_theme + scale_x_continuous(breaks=pretty_breaks(n=10))+
  labs(x="Month", y="Proportion of diet", title="Goby diet items over time")+
  facet_grid(Year ~ SizeClass + .)

agg3 <- aggregate(data=Grab[Grab$Year=="2012",], Total.Organisms.sum~DepNonDep+Depth.m.standard+Month+Order, function(x) sum(x))
ggplot(agg3, aes(x=Month, y=Total.Organisms.sum, fill=Order)) +
  geom_bar(stat="identity", position = "fill") +
  Goby_theme + scale_x_continuous(breaks=pretty_breaks(n=10))+
  labs(x="Month", y="Proportion of diet", title="Goby diet items over time")+
  facet_grid(DepNonDep ~ Depth.m.standard +.)


#####Graph size of fish vs. size of diet items####
lengths <- join(NA.omit.biomass.diets,CompiledFishLog, by="YearSERUnique")
#"Lengthmm"= fish"mm.length"=diet items

#Plot all fish vs all diet items
ggplot(lengths, aes(x = Lengthmm, y = mm.length)) + geom_point() + geom_smooth(method = "lm", se = TRUE, colour="black") +  Goby_theme +
  labs(x="Round goby total length (mm)", y="Diet item total length (mm)", title="All years")+
  scale_x_continuous(breaks=pretty_breaks(n=10)) + scale_y_continuous(breaks=pretty_breaks(n=10), limits=c(0, 40))

#Plot fish size vs. quagga size
Q <- lengths[which(lengths$Diet.Item =="Quagga"),] #isolate quaggas
ggplot(Q, aes(x = Lengthmm, y = mm.length)) + geom_point() + geom_smooth(method = "lm", se = TRUE, colour="black") +  Goby_theme +
  labs(x="Round goby total length (mm)", y="Quagga mussel total length (mm)", title="All years")+
  scale_x_continuous(breaks=pretty_breaks(n=10)) + scale_y_continuous(breaks=pretty_breaks(n=10), limits=c(0, 40))

###How many diet items actually have lengths associated with them--e.g. how many were measured?
length(which(is.na(fishdiet2$mm.length))) # How many missing lengths
length(which(!is.na(fishdiet2$mm.length))) # How many have lengths

##Summary of diet item lengths
dietlengths<- lengths[!is.na(lengths$mm.length),]
summaryBy(mm.length ~ Diet.Item,data=dietlengths, FUN=c(mean,sd,min,max,length))

#Compare the distributions of the lengths of diet items and benthos critters with  Kolmogorov-Smirnov tests
Nob2<- NA.omit.biomass.benthos
names(Nob2)[names(Nob2)=="Taxon"]<- "Diet.Item"
#Quaggas
ksTest(Nob2$mm.length[Nob2$Diet.Item=="Quagga"], dietlengths$mm.length[dietlengths$Diet.Item=="Quagga"])
#harpacticoids
ksTest(Nob2$mm.length[Nob2$Diet.Item=="Harpacticoid"], dietlengths$mm.length[dietlengths$Diet.Item=="Harpacticoid"])
#Copepods in general
ksTest(Nob2$mm.length[Nob2$Order=="Copepoda (sub class)"], dietlengths$mm.length[dietlengths$Order=="Copepoda (sub class)"])
#Cladocera in general
ksTest(Nob2$mm.length[Nob2$Order=="Cladocera"], dietlengths$mm.length[dietlengths$Order=="Cladocera"])
#Ostracods in general
ksTest(Nob2$mm.length[Nob2$Order=="Ostracoda"], dietlengths$mm.length[dietlengths$Order=="Ostracoda"])
#Veneroida in general
ksTest(Nob2$mm.length[Nob2$Order=="Veneroida"], dietlengths$mm.length[dietlengths$Order=="Veneroida"])

#show difference size between benthos/diets for dressenids
hist(Nob2$mm.length[Nob2$Order=="Veneroida"],col=rgb(0,0,1,1/4),xlab="Dressenid length (mm)",main="Purple=Benthos, pink=diets")
hist(dietlengths$mm.length[dietlengths$Order=="Veneroida"],col=rgb(1,0,0,1/4), xlab="Dressenid length (mm)",add=TRUE)

######################################################################################################
###Change in diet over time? by season, by goby size class, by food type
d3<- d[ , which(names(d) %in% c("Month","Order","Total.Organisms.sum","SizeClass","Year"))]
d3<-d3[!is.na(d3$SizeClass),]
d3$Month <- as.factor(d3$Month)
levels(d3$Month)[levels(d3$Month) %in%  c("5","6","7","8")] <- "Summer"
levels(d3$Month)[levels(d3$Month) %in%  c("9","10","11")] <- "Fall"
test<- summaryBy(Total.Organisms.sum ~ Order + Month + SizeClass + Year,data=d3, FUN=c(sum))
test<-test[!is.na(test$Order),]
test[is.na(test)] <- 0 

test <- test[-which(test$Order %in% c("???","Cestoda","Hemiptera","Nematoda","Odonata","Thysanoptera","Empty","Decapoda")),] 
test$Order<-droplevels(test$Order)

glm.model = glm(Total.Organisms.sum.sum ~ Order * Month * SizeClass * Year, data=test, family=poisson)
anova(glm.model, test="Chisq")

######################################################################################################
###Compile diet tables
##Frequency of occurance by diet item ** COULD SWITCH OUT FOR FAMILY OR ORDER IF NEED BE
count.di<- count(d, c("Diet.Item"))
count.di$prop.Occurance <- count.di$freq/length(unique(d$YearSERUnique)) #get proportion of critter/total stomachs
#now get the percent number
perc1<- summaryBy(Total.Organisms.sum ~ Diet.Item,data=d, FUN=c(sum))
perc1$prop.Number<-perc1$Total.Organisms.sum.sum/(sum(perc1$Total.Organisms.sum.sum))
diettable1<- join(count.di,perc1)
#####Index of importance calculations
#Propn.Number X propn.occurance
diettable1$Index.of.importance <- diettable1$prop.Number * diettable1$prop.Occurance

#Get the perc. weight with some estimations...
#First, how many biomass estimations are missing from the diet data?
length(which(is.na(d$Av.biomass.mg))) # How many missing
length(which(!is.na(d$Av.biomass.mg))) # Not missing
#Proportion missing:
length(which(is.na(d$Av.biomass.mg)))/(length(which(is.na(d$Av.biomass.mg)))+length(which(!is.na(d$Av.biomass.mg))))
#let's try to fill in some of these missing biomasses with benthos data and other diet data!
ben1<-join(NA.omit.biomass.benthos,CompiledBenthosLog,by="YearSER")
avbioben<- summaryBy(biomass.mg ~ GeneralLoc + Taxon,data=ben1, FUN=c(mean))
names(avbioben)[names(avbioben)=="Taxon"]<- "Diet.Item"
d2<-join(d,avbioben,by=c("GeneralLoc","Diet.Item"))
#average the benthos data
fish1<-join(NA.omit.biomass.diets,CompiledFishLog,by="YearSERUnique")
avbiofish<- summaryBy(biomass.mg ~ GeneralLoc + Diet.Item,data=fish1, FUN=c(mean))
names(avbiofish)[names(avbiofish)=="biomass.mg.mean"]<-"biomass.mg.mean.fish.diets"
d2<-join(d2,avbiofish,by=c("GeneralLoc","Diet.Item"))
attach(d2)
d2$est.final.biomass.mg<-ifelse(is.na(Final.biomass.mg), biomass.mg.mean, Final.biomass.mg)
d2$est.final.biomass.mg<-ifelse(is.na(est.final.biomass.mg), biomass.mg.mean.fish.diets, est.final.biomass.mg)
detach(d2)
#Now how many are missing?
length(which(is.na(d2$est.final.biomass.mg))) # How many missing
length(which(!is.na(d2$est.final.biomass.mg))) # Not missing
length(which(is.na(d2$est.final.biomass.mg)))/(length(which(is.na(d2$est.final.biomass.mg)))+length(which(!is.na(d2$est.final.biomass.mg))))
#slightly better.... may not be enough to do the weight stuff/IRI :(
percW<- summaryBy(est.final.biomass.mg ~ Diet.Item,data=d2, FUN=c(sum))
percW<- percW[which(!is.na(percW$est.final.biomass.mg.sum)),]
percW$prop.Weight<-percW$est.final.biomass.mg.sum/(sum(percW$est.final.biomass.mg.sum))

#Join all together
diettable1<- join(diettable1,percW)

#Round the numbers and view the table
diettable1[,-1] <-round(diettable1[,-1],4) #the "-1" excludes column 1
diettable1
#write.csv(diettable1, "diettable.csv")


#another way to do this; frequency BY YEAR
count.di2<- count(d, c("Year","Diet.Item"))
count.di2$Year<-as.factor(count.di2$Year)
numfish<- data.frame(t(table(f$Year.1)))# get number of fish processed
names(numfish)[names(numfish)=="Var2"] <- "Year"
names(numfish)[names(numfish)=="Freq"] <- "NumStomachs"
freqbyyear<-join(count.di2,numfish,by="Year")
freqbyyear$prop <-freqbyyear$freq/freqbyyear$NumStomachs


