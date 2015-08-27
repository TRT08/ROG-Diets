########################################################################################
#######selectivity indices.  RUN diet & benthic biomass freq script thing!!#############
#################################################################################
#  RUN THESE FIRST TO GET THE NECESSARY FILES FOR THIS SCRIPT TO RUN ########
source("F:/DATA/SLBE/R scripts/Diet biomass and frequency/dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.diets<- NA.omit.biomass
rm(NA.omit.biomass)
source("F:/DATA/SLBE/R scripts/Benthos biomass and frequency/biomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.benthos<- NA.omit.biomass
rm(NA.omit.biomass)
setwd("F:/DATA/SLBE/R scripts/Round Goby Diet Analysis/Selectivity/")#set directory 

#delete any zoops that are pelagic from both benthos and diet
B<-CompiledBenthosLogandFreq[!(CompiledBenthosLogandFreq$Habitat %in% c("Migrator", "Pelagic")),]
D<-CompiledFishLogandDiets[!(CompiledFishLogandDiets$Habitat %in% c("Migrator", "Pelagic")),]

##pool all diets by Event, Year, DepNonDep
diet.order <- cast(CompiledFishLogandDiets, Event + Year + Site.Condition.For.Event + Empty.Stomach. ~ Order, value='Total.Organisms.sum', sum)
diet.order <- subset(diet.order, Empty.Stomach.=="N")
diet.order$Empty.Stomach. <- NULL
diet.order<-diet.order[!is.na(diet.order$Site.Condition.For.Event),]
diet.order <-diet.order[ , -which(names(diet.order) %in% c("???", "Acanthocephala", "Cestoda", "Decapoda","Empty"
                                                           ,"Fish","Hemiptera","Odonata", "Thysanoptera" ))] #gets rid of all the guys in diets but not in benthos

#Make columns for all the guys that are in the benthos but not in the diets and call them 0
diet.order$Arhynchobdellida<-0
diet.order$Diplostraca<-0
diet.order$Ephemeroptera<-0
diet.order$Nemertea<-0
diet.order$Plecoptera<-0
diet.order$Rhabdocoela<-0
diet.order$Trichoptera<-0
diet.order$Trombidiformes<-0

#alphabetize all the diet itemz
dlabels<-diet.order[,1:3]
ditems<-diet.order[,4:23]
ditems<-ditems[,order(names(ditems))]

diet<-cbind(dlabels,ditems)



##benthos avail--Freq determination by Event, Year, site condition
benthos.avail <- cast(CompiledBenthosLogandFreq, Event + Year + Site.Condition.For.Event ~ Order, value='Total.Organisms.sum', sum)
benthos.avail<-benthos.avail[!is.na(benthos.avail$Site.Condition.For.Event),]
benthos.avail$c <-benthos.avail$Copepoda + benthos.avail$Cyclopoida + 
  benthos.avail$Calanoida + benthos.avail$Harpacticoida #makes a column that sums up all copepods
benthos.avail <-benthos.avail[ , -which(names(benthos.avail) %in% c("Cyclopoida", "Harpacticoida", "Calanoida", "Copepoda"))] #gets rid of all copepods categories
names(benthos.avail)[names(benthos.avail)=="c"]<-"Copepoda" #makes c column copepoda


#alphabetize the benthos items...probs a better way to do this but im too lazy right now.
benthos.labels<-subset(benthos.avail[,(1:3)])
benthos.critters<-subset(benthos.avail[,(4:23)])
benthos.critters<-benthos.critters[,order(names(benthos.critters))]

benthos<-cbind(benthos.labels,benthos.critters)

#check to make sure that all the column names are the same for both diet and benthos DFs
colnames(diet)
colnames(benthos)


########
###DO THIS ONLY WHEN YOU HAVE MORE BENTHOS SAMPLES
###AND NEED AN UPDATED DATASET
##get just site, year, event data from diets and benthos
diet.labels <-subset(diet[,1:3])
benthos.labels <-subset(benthos[,1:3])

colnames(diet.labels)
colnames(benthos.labels)

#This join gives you all the diets that have a matching benthos
bd.match <-join(benthos.labels,diet.labels, by=c("Event", "Year", "Site.Condition.For.Event"), type="inner", match="all")

#make final diet and benthos df's
final.diets<-join(bd.match,diet, by=c("Event", "Year", "Site.Condition.For.Event"), type="left", match="all")
final.benthos<-benthos


####diets & benthos from Live vs. Bare vs. Sloughed
dietsLIVE<-subset(final.diets,Site.Condition.For.Event=="LIVE")
benthosLIVE <- subset(final.benthos, Site.Condition.For.Event=="LIVE")

dietsBARE <-subset(final.diets, Site.Condition.For.Event=="BARE")
benthosBARE<-subset(final.benthos, Site.Condition.For.Event=="BARE")

dietsSLOUGHED <-subset(final.diets, Site.Condition.For.Event=="SLOUGHED")
benthosSLOUGHED<-subset(final.benthos, Site.Condition.For.Event=="SLOUGHED")


##get percentages of both diets and benthos
library(paleoMAS)
###
##from LIVE
###
#diets
justdiets.LIVE<-dietsLIVE[,4:23]
diet.percent.LIVE <-percenta(justdiets.LIVE, first=1, last=20) #dataframe of percentages in stomach

#benthos:
justbenthos.LIVE<-benthosLIVE[,4:23]
benthos.percent.LIVE <-percenta(justbenthos.LIVE, first=1, last=20) #dataframe of percentages in benthos


###
##from BARE
###
#diets:
justdiets.BARE<-dietsBARE[,4:23]
diet.percent.BARE <-percenta(justdiets.BARE, first=1, last=20) #dataframe of percentages in stomach

#benthos
justbenthos.BARE<-benthosBARE[,4:23]
benthos.percent.BARE <-percenta(justbenthos.BARE, first=1, last=20) #dataframe of percentages in benthos

###
##from SLOUGHED
###
#diets:
justdiets.SLOUGHED<-dietsSLOUGHED[,4:23]
diet.percent.SLOUGHED <-percenta(justdiets.SLOUGHED, first=1, last=20) #dataframe of percentages in stomach

#benthos
justbenthos.SLOUGHED<-benthosSLOUGHED[,4:23]
benthos.percent.SLOUGHED <-percenta(justbenthos.SLOUGHED, first=1, last=20) #dataframe of percentages in benthos


colnames(diet.percent.LIVE)
colnames(benthos.percent.LIVE)

colnames(diet.percent.BARE)
colnames(benthos.percent.BARE)

colnames(diet.percent.SLOUGHED)
colnames(benthos.percent.SLOUGHED)

#####################
####calculate E...finally
#E= ri-pi/ri+pi
##ri= % of each critter in the gut
##pi= % of each critter in the benthos


##LIVE VS. BARE VS. SLOUGHED
E.LIVE<-((diet.percent.LIVE)-(benthos.percent.LIVE))/((diet.percent.LIVE)+(benthos.percent.LIVE))
boxplot(E.LIVE, use.cols = TRUE, main="Goby food selection: LIVE", ylab="Ivlev's Electivity Index (E)", las=2, par(mar =c(7, 4, 4, 2) + 0.1))

E.BARE<-((diet.percent.BARE)-(benthos.percent.BARE))/((diet.percent.BARE)+(benthos.percent.BARE))
boxplot(E.BARE, use.cols = TRUE, main="Goby food selection: BARE", ylab="Ivlev's Electivity Index (E)", las=2, par(mar =c(7, 4, 4, 2) + 0.1))

E.SLOUGHED<-((diet.percent.SLOUGHED)-(benthos.percent.SLOUGHED))/((diet.percent.SLOUGHED)+(benthos.percent.SLOUGHED))
boxplot(E.SLOUGHED, use.cols = TRUE, main="Goby food selection: SLOUGHED", ylab="Ivlev's Electivity Index (E)", las=2, par(mar =c(7, 4, 4, 2) + 0.1))


#########################
#rename columns for graph--for presentations, etc...
colnames(diet.percent.NONDEP2013)[colnames(diet.percent.NONDEP2013)=="Trombidiformes"]<-"Acari"
colnames(diet.percent.NONDEP2013)[colnames(diet.percent.NONDEP2013)=="Veneroida"]<-"Dreissinidae"
colnames(diet.percent.DEP2013)[colnames(diet.percent.DEP2013)=="Trombidiformes"]<-"Acari"
colnames(diet.percent.DEP2013)[colnames(diet.percent.DEP2013)=="Veneroida"]<-"Dreissinidae"
colnames(diet.percent.NONDEP2013)[colnames(diet.percent.NONDEP2013)=="Trombidiformes"]<-"Acari"
colnames(diet.percent.NONDEP2013)[colnames(diet.percent.NONDEP2013)=="Veneroida"]<-"Dreissinidae"
colnames(benthos.percent.NONDEP2013)[colnames(benthos.percent.NONDEP2013)=="Trombidiformes"]<-"Acari"
colnames(benthos.percent.NONDEP2013)[colnames(benthos.percent.NONDEP2013)=="Veneroida"]<-"Dreissinidae"
colnames(benthos.percent.DEP2013)[colnames(benthos.percent.DEP2013)=="Trombidiformes"]<-"Acari"
colnames(benthos.percent.DEP2013)[colnames(benthos.percent.DEP2013)=="Veneroida"]<-"Dreissinidae"
