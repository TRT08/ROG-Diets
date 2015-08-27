#####Run dietbiomasfreqscript and Runme4diets first!!

library(ggplot2)
library(scales)
library(compositions)
library(adehabitat)
library(lme4)
library(paleoMAS)
library(doBy)
library(reshape)
library(reshape2)
library(scales)
library(lubridate)
library(extrafont)
library(plyr)
library(outliers)
library(forams)
library(vegan)
library(FSA)
library(compare)

#  RUN THESE FIRST TO GET THE NECESSARY FILES FOR THIS SCRIPT TO RUN ########
source("F:/DATA/SLBE/R scripts/Diet biomass and frequency/dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.diets<- NA.omit.biomass
rm(NA.omit.biomass)
source("F:/DATA/SLBE/R scripts/Benthos biomass and frequency/biomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.benthos<- NA.omit.biomass
rm(NA.omit.biomass)
setwd("F:/DATA/SLBE/R scripts/Diet biomass and frequency/")#set directory 

logfile <- file("output.log") #creates a backlog of the script activity (look in folder)
sink(logfile, append=TRUE) 
sink(logfile, append=TRUE, type="message")
source("dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000) #Load the data and run calculations
sink()  # Restore output to console
sink(type="message")
rm(fishdiet2,NA.omit.biomass)

######################################################################
###README####
###ALL FISH AND FISH PARTS HAVE BEEN EXCLUDED FROM ANALYSIS
###ALL EMPTY FISH STOMACHS HAVE BEEN EXCLUDED FROM ANALYSIS
###the d dataframe and the measured.biomass frame come from CompiledFishLogandDiets
###and CompiledBenthosLogandFreq, respectively.  I had to go trhough and change several family and order names
###Gammaridae --> Amphipoda, Canthocamptidae --> Harpacticoida, Cyclopidae --> Cyclopoida are a couple
###so that they were the same between the two datasets and to facilitate everything i just saved
###them as csvs so you can just read them into R.  
###IF ANYTHING CHANGES IN THE DATABASE, you gotta go through and do that again.
###6/10/2015  there were a couple of crayfish, generic hemiptera and a parasite that dont have biomass estimates yet
###fill in odonate biomass. havent found any in benthos yet sooooo yeah.  May be a good idea to find a generic
###odonate biomass to put in here...although there was only one found in all fish.
#d$biomass.mg.mean.general[d$Order =="Odonata"] <- d$est.final.biomass.mg[d$Order =="Odonata"]
#names(CompiledFishLogandDiets)
#d <- CompiledFishLogandDiets
#d <- subset(CompiledFishLogandDiets, Cheese.=="N") #remove cheesiness
# <-d[!(d$Diet.Item %in% c("Fish", "Fish Egg", "Empty", "Fish Spp (whole or mostly)", "Fish scales" ,"???")),]
# <-d[!(d$Order %in% c("Fish", "???", "Empty")),]
#d$Month <- month(d$Date)

#c <- CompiledBenthosLogandFreq
#levels(c$Status)
#selected<-c("Processed")
#c <- c[c$Status %in% selected,]
#c$Month <- month(c$Date)
#c$DayNum <- yday(c$Date)
#c<-subset(c, Year >=2011)
#c$Year <- as.factor(c$Year)

######
##fill in holes (heh) with average biomass from benthos data.
##unhash if anything changes in the database.  if thats the case:
##make a df of all the animals that have been measured
#measured.benthos<-subset(c, Num.critters.averaged.4.biomass>="1")
##make a df of all the animals that havent been measured yet
#unmeasured.benthos <-subset(c, is.na(c$ Num.critters.averaged.4.biomass))

#write.csv(measured.benthos, "MeasuredBenthos.csv")

#this is the file that has the names of the animals
#that has matching animal names for both the benthic and diet dataframes
measured.benthos<-read.csv(file="F:\\DATA\\SLBE\\R scripts\\Diet biomass and frequency\\MeasuredBenthos.csv", header=TRUE, sep=",")

# get average biomass per family and order
avg.biomass<- summaryBy(Av.biomass.mg ~ Family,data=measured.benthos, FUN=c(mean))
avg.biomass.order<- summaryBy(Av.biomass.mg ~ Order,data=measured.benthos, FUN=c(mean))

#change the names of the average biomasses so that they are easier to use for later.
names(avg.biomass)[names(avg.biomass)=="Av.biomass.mg.mean"]<-"biomass.mg.mean.general"
names(avg.biomass.order)[names(avg.biomass.order)=="Av.biomass.mg.mean"]<-"biomass.mg.mean.general"

##If you have to go in and make a .csv file (only if something changes with the data)
#of the average biomasses be sure to go in there and change the following family names:
##Gammaridae --> Amphipoda, Canthocamptidae --> Harpacticoida, Cyclopidae --> Cyclopoida
##so that it matches the family names in the diet avg biomass spreadsheet.
#write.csv(avg.biomass, "AvgBiomass.csv")

#get the d dataframe ready

#calculate total number of bugs per stomach
perc <- summaryBy(Total.Organisms.sum ~ YearSERUnique,data=d, FUN=c(sum))
names(perc)[names(perc)=="Total.Organisms.sum.sum"] <-"Gut.total"
d<-join(d,perc, by=c("YearSERUnique"))

#join the average biomasses for each order
d<-join(d,avg.biomass.order, by=c("Order"), match="all")


d$est.final.biomass.mg<-ifelse(is.na(d$Final.biomass.mg), d$biomass.mg.mean.general, d$Final.biomass.mg)

#the est.final.biomass.mg column is then one you will use from now on for anything biomass related

#what diet items are still NA
unmeasured.diet.items <-subset(d, is.na(d$est.final.biomass.mg))
#6/10/2015  there were a couple of crayfish, generic hemiptera and a parasite that dont have biomass estimates yet

#fill in odonate biomass. havent found any in benthos yet sooooo yeah
#d$biomass.mg.mean.general[d$Order =="Odonata"] <- d$est.final.biomass.mg[d$Order =="Odonata"]


d <- d[!is.na(d$est.final.biomass.mg),] #remove all animals w/o a biomass estimate


##make df that has each fish with biomasses and N of each diet item

volume.est <-subset(d[,c("YearSERUnique", "Diet.Item", "Order" , "Family" ,"TrophicPosition", 
                            "Total.Organisms.sum", "Gut.total","est.final.biomass.mg")])
volume.est <-volume.est[!is.na(volume.est$TrophicPosition),] #remove nematodes (no trophic position...yet)

#calculate the percent of each organism (N in the equation)
volume.est$percent<-(volume.est$Total.Organisms.sum/volume.est$Gut.total)

##Now make a column that has the total biomass in each fish stomach
volume.est$tot.biomass.org<- (volume.est$Gut.total*volume.est$est.final.biomass.mg)
biomass.sum<-summaryBy(tot.biomass.org~YearSERUnique, data=volume.est, FUN=c(sum))
names(biomass.sum)[names(biomass.sum)=="tot.biomass.org.sum"]<-"tot.biomass.per.fish"
volume.est<-join(volume.est,biomass.sum,by=c("YearSERUnique"))




#change the names of the columns so that they fit in the equation
names(volume.est)[names(volume.est)=="percent"] <-"N"
names(tot.biomass.per.fish)[names(tot.biomass.per.fish)=="tot.biomass.per.fish"]<- "mass.a"
names(volume.est)[names(volume.est)=="est.final.biomass.mg"] <-"mass.i"
trophic.calc<-join(biomass.and.N,tot.biomass.per.fish,by=c("YearSERUnique"))

########OOOOOOOOOOOOOK-  this is fucking up. or its stupid or im stupid or both.
#I think where things are fucking up is the final.biomass.mg column.  that needs to have the general biomass*N
#IS THAT SO MUCH TO ASK?!




#use trophic.calc dataframe to now calculate Vi
##Vi = (n*mass.i/mass.a)
trophic.calc$V<-((trophic.calc$N*trophic.calc$mass.i)/trophic.calc$mass.a)


trophic.calc$Vi.Ti<-(trophic.calc$Vi*trophic.calc$TrophicPosition)

troph.per.fish<-summaryBy(trophic.calc$Vi.Ti~YearSERUnique, data=trophic.calc, FUN=c(sum))




diet.biomass <- cast(d, YearSERUnique ~ Order, value='est.final.biomass.mg', sum)

diet.biomass <-diet.biomass[rowSums(diet.biomass) > 0, ] #gets rid of all the empty stomachs
diet.biomass <-na.omit(diet.biomass)

diet.N <- cast(d, YearSERUnique ~ Order, value='Total.Organisms.sum', sum)
diet.N <-diet.N[rowSums(diet.N) > 0, ] #gets rid of all the empty stomachs