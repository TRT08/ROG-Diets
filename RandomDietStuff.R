############################################################
#####BEHOLD: RANDOMDIETSTUFF.  ALL THE RANDOM ANALYSES######
#####YOU COULD WANT TO DO WITH THE ROUND GOBY DIET DATA#####
############################################################

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
source("F:/DATA/SLBE/R scripts/ROG CPUE/compiledataforCPUE.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.diets<- NA.omit.biomass
rm(NA.omit.biomass)
setwd("F:/DATA/SLBE/R scripts/Round Goby Diet Analysis/")#set directory

logfile <- file("output.log") #creates a backlog of the script activity (look in folder)
sink(logfile, append=TRUE) 
sink(logfile, append=TRUE, type="message")
source("dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000) #Load the data and run calculations
sink()  # Restore output to console
sink(type="message")
rm(fishdiet2,NA.omit.biomass)


#####Freq determination by Year and general location
freq.year <- cast(CompiledFishLogandDiets, YearSERUnique ~ Family, value='Total.Organisms.sum', sum)
freq.year<-freq.year[rowSums(freq.year) > 0, ] #gets rid of all the empty stomachs
row.names(freq.year)<-freq.year$YearSERUnique #makes rownames out of the YearSERUniqe col
freq.year<-freq.year[,-1]
freq.yearm <-data.matrix(freq.year, rownames.force=TRUE) 


################################################################################
###############################PROPORTIONS N STUFF##############################
######Stacked bar graphs for diets by nearshore/offshore, dep, year, size
#####Freq determination by Year and general location
###adapted from Diet analysis graphs scripts.R by TRT

###RUN FIRST####
names(CompiledFishLogandDiets)
d <- CompiledFishLogandDiets
d <- subset(CompiledFishLogandDiets, Cheese.=="N") #remove cheesiness
d <-d[!(d$Diet.Item %in% c("Fish", "Fish Egg", "Empty", "Fish Spp (whole or mostly)", "Fish scales" ,"???")),]
d <-d[!(d$Order %in% c("Fish", "???", "Empty")),]
d$Month <- month(d$Date)
d<-d[!is.na(d$Site.Condition.For.Event),]
d<-d[!is.na(d$SizeClass),]
FinalFish$DayNum<- yday(FinalFish$Date.In)
effort <-subset(FinalFish[,c("YearSER","Effort..hrs.", "DayNum", "Standardized.Site.ID")])
d<-join(d,effort, by="YearSER") #add effort hours to d dataframe
colnames(d)

f<- CompiledFishLog
f$Month <- month(f$Date)
#Extract processed fish only
levels(f$Status)
f <- f[which(f$Status %in% c('Frozen - Stomach Removed - Stomach Processed','Processed')), ]
######

#####SET UP THEME FOR YOUR GRAPHS~!!! GGPLOT!!! ##########
#font_import(pattern='arial') #Get arial
#loadfonts(device = "win")

Goby_theme <- theme_bw()+theme(
  text = element_text(family="Arial", colour="black", size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.key = element_blank())+
  theme(axis.title.y = element_text(vjust=1))+ 
  theme(strip.background = element_rect(fill = 'gray97'))

Goby_pres_theme <- theme_bw()+theme(
  text = element_text(colour="black", size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.key = element_blank())+
  theme(axis.title.y = element_text(vjust=1))+ 
  theme(strip.background = element_rect(fill = 'gray97'))

cbPalette <- c("#000000","#999999", "#c0c0c0", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Month_scale <-scale_x_discrete("Month", labels = c("6" = "Jun","7" = "Jul","8" = "Aug","9" = "Sep","10" = "Oct", "11"="Nov"))

##########################################################  

Grab<- d[ , c("Month","Order","Year","Total.Organisms.sum","SizeClass","GeneralLoc","Depth.m.standard","DepNonDep","Site.Condition.For.Event","Family", "YearSERUnique")]
agg <- aggregate(data=Grab, Total.Organisms.sum~ Year+Month+Order, function(x) sum(x))

SampleSize <- aggregate(data=d, YearSERUnique~Year+Date, function(x) length(unique(x)))
plotovertime <- join(agg,SampleSize,by=c("Year","Month"))

ggplot(plotovertime, aes(x=Month, y=Total.Organisms.sum, fill=Order)) +
  geom_bar(stat="identity", position = "fill") +
  Goby_theme + scale_x_continuous(breaks=pretty_breaks(n=10))+
  labs(x="Month", y="Proportion of diet", title="Goby diet items over time")+
  facet_grid(Year ~ .)
###Or to export to excel to make easier graphs....
#write.csv(plotovertime,"plotovertime.csv")

###By size class ### THIS IS A REALLY NICE GRAPH!
###run the agg2 for analyses below
##unhash the first two in order to make changes
#agg2 <- aggregate(data=Grab, Total.Organisms.sum~ Year +Month+ DepNonDep+Site.Condition.For.Event +SizeClass+Order, function(x) sum(x))
#agg2<-subset(agg2, subset= Order %in% c("Amphipoda" , "Cladocera" ,"Copepoda" ,"Diptera",
#                                        "Gastropoda","Isopoda","Oligochaeta","Ostracoda",
#                                        "Trombidiformes","Veneroida"))
#go into excel and change the names of some of the orders 
#to family to make the figure easier to interpret
#write.csv(agg2,"agg2.csv")
agg2<-read.csv(file="F:\\DATA\\SLBE\\R scripts\\Round Goby Diet Analysis\\csv to use\\agg2.csv", header=TRUE, sep=",")
#agg2$Date<-as.Date(agg2$Date, format="%m/%d/%Y")

agg2$Month<-as.factor(agg2$Month)

ggplot(agg2, aes(x=Month, y=Total.Organisms.sum, fill=Order)) +
  geom_bar(stat="identity", position = "fill") +
  Goby_theme + Month_scale+
  scale_fill_manual(guide = guide_legend(title = NULL),values=cbPalette, breaks=c("Dreissenidae", "Acari", "Ostracoda", "Oligochaeta","Isopoda","Gastropoda","Chironomidae", "Copepoda", "Cladocera", "Amphipoda"))+
  labs(x="Month", y="% Frequency", title="Goby diet by habitat")+
  facet_grid(Year ~Site.Condition.For.Event +.) 

agg3 <- aggregate(data=Grab[Grab$Year=="2012",], Total.Organisms.sum~DepNonDep+Depth.m.standard+Month+Order, function(x) sum(x))
ggplot(agg3, aes(x=Month, y=Total.Organisms.sum, fill=Order)) +
  geom_bar(stat="identity", position = "fill") +
  Goby_theme + scale_x_continuous(breaks=pretty_breaks(n=10))+
  labs(x="Month", y="Proportion of diet", title="Goby diet items over time")+
  facet_grid(DepNonDep ~ Depth.m.standard +.)

###get proportions of each diet item per size class, month, year-- use to calculate %F
diet.prop<-ddply(agg2,.(Year, Month, SizeClass),transform,prop=Total.Organisms.sum/sum(Total.Organisms.sum))
#write.csv(diet.prop,"dietprop.csv")

###get proportions of each diet item per size class, year only
###Basically a table version of the REALLY NICE GRAPH above
dietBySize <- summaryBy(Total.Organisms.sum~ Year+SizeClass+Order, data=agg2, FUN=(sum))
colnames(dietBySize)[4] <-"t"
dietBySize<-ddply(dietBySize,.(Year,SizeClass),transform,prop=t/sum(t))
#write.csv(dietBySize,"dietbysize.csv")


####How many fish per size class in each habitat type by year
D <-summaryBy(SizeClass ~ Year.1 + SizeClass +DepNonDep, data = f, FUN=length)
D<-na.omit(D)
D

D2012 <-subset(D, Year.1=="2012")
D2012$Year.1=NULL
D2012 <-na.omit(D2012)

D2013 <-subset(D, Year.1=="2013")
D2013$Year.1=NULL
D2013 <-na.omit(D2013)


ggplot(D,aes(x=SizeClass,y=SizeClass.length,fill=factor(DepNonDep)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Habitat Type",
                      breaks=c("DEP", "NONDEP"),
                      labels=c("DEP", "NONDEP"))+
  xlab("Size Class")+ylab("Number of Processed Fish") + ggtitle('Processed Gobies per Size Class') +
  theme_bw() + theme(axis.line = element_line(colour = "black"))+
  facet_grid(.~Year.1)

#Total Number of fish diets / year
table(f$Year.1)

summaryBy(SizeClass ~ Year.1 + SizeClass + DepNonDep, data = f, FUN=length)

###taxonomic richness by size class & year
specnumber(which(dietBySize$SizeClass=="1" & dietBySize$Year=="2011"))
specnumber(which(dietBySize$SizeClass=="2" & dietBySize$Year=="2011"))
specnumber(which(dietBySize$SizeClass=="3" & dietBySize$Year=="2011"))

specnumber(which(dietBySize$SizeClass=="1" & dietBySize$Year=="2012"))
specnumber(which(dietBySize$SizeClass=="2" & dietBySize$Year=="2012"))
specnumber(which(dietBySize$SizeClass=="3" & dietBySize$Year=="2012"))

specnumber(which(dietBySize$SizeClass=="1" & dietBySize$Year=="2013"))
specnumber(which(dietBySize$SizeClass=="2" & dietBySize$Year=="2013"))
specnumber(which(dietBySize$SizeClass=="3" & dietBySize$Year=="2013"))

#taxonomic richness by dep vs nondep & year
agg4 <- aggregate(data=Grab, Total.Organisms.sum~ Year+DepNonDep+ Order, function(x) sum(x))
dietByDep <- summaryBy(Total.Organisms.sum~ Year+DepNonDep+Order, data=agg4, FUN=(sum))

specnumber(which(dietByDep$DepNonDep=="DEP" & dietByDep$Year=="2012"))
specnumber(which(dietByDep$DepNonDep=="NONDEP" & dietByDep$Year=="2012"))
specnumber(which(dietByDep$DepNonDep=="DEP" & dietByDep$Year=="2013"))
specnumber(which(dietByDep$DepNonDep=="NONDEP" & dietByDep$Year=="2013"))

write.csv(dietByDep, "dietByDep.csv")

########################################################################################
#######selectivity indices.  RUN benthic biomass freq script thing!!#############
#################################################################################
##pool all diets by Event, Year, DepNonDep
diet.order <- cast(CompiledFishLogandDiets, Event + Year + DepNonDep + Empty.Stomach. ~ Order, value='Total.Organisms.sum', sum)
diet.order <- subset(diet.order, Empty.Stomach.=="N")
diet.order$Empty.Stomach. <- NULL
diet.order <-diet.order[ , -which(names(diet.order) %in% c("???", "Acanthocephala", "Cestoda", "Decapoda","Empty"
                                                           ,"Fish","Hemiptera","Odonata", "Thysanoptera" ))] #gets rid of all the guys in diets but not in benthos

##benthos avail--Freq determination by Event, Year, DepNonDep
benthos.avail <- cast(CompiledBenthosLogandFreq, Event + Year + DepNonDep ~ Order, value='Total.Organisms.sum', sum)
benthos.avail$c <-benthos.avail$Copepoda + benthos.avail$Cyclopoida + 
  benthos.avail$Calanoida + benthos.avail$Harpacticoida #makes a column that sums up all copepods
benthos.avail <-benthos.avail[ , -which(names(benthos.avail) %in% c("Cyclopoida", "Harpacticoida", "Calanoida", "Copepoda"))] #gets rid of all copepods categories
colnames(benthos.avail)[23]<-"Copepoda" #makes c column copepoda
benthos.avail <-benthos.avail[ , -which(names(benthos.avail) %in% c( "Arhynchobdellida", "Diplostraca", "Ephemeroptera","Nemertea"
                                                                     ,"Rhabdocoela", "Trichoptera","Plecoptera"))] #gets rid of all the things in benthos that arent in diets

benthos.avail<-benthos.avail[,c(1,2,3,4,5,16,6,7,8,9,10,11,12,13,14,15)]
colnames(diet.order)
colnames(benthos.avail)

##get just 2012 & 2013 dater
diet <-subset(diet.order, Year >=2012)
diet <-na.omit(diet)
benthos <-subset(benthos.avail, Year >=2012)

########
###DO THIS ONLY WHEN YOU HAVE MORE BENTHOS SAMPLES
###AND NEED AN UPDATED DATASET
##get just site, year, event data from diets and benthos
#diet.labels <-subset(diet[,1:3])
#benthos.labels <-subset(benthos[,1:3])

#colnames(diet.labels)
#colnames(benthos.labels)

#bd.match <-join(diet.labels, benthos.labels, by=c("Event", "Year", "DepNonDep"), type="inner", match="all")
#write.csv(bd.match,"bd.csv")
#write.csv(diet.labels,"dlabels1.csv")
#write.csv(benthos.labels,"blabels1.csv")
#now go and find matches for all diets that have matching benthos,
#make a new csv file of the ones to use and and call it diets2use.csv
###############

#get final diets by using your diets2use list
diets2use <-read.csv("F:\\DATA\\SLBE\\R scripts\\Round Goby Diet Analysis\\csv to use\\diets2use.csv", header=TRUE, sep=",")
final.diets<-join(diets2use,diet, by=c("Event", "Year", "DepNonDep"), type="left", match="all")

final.benthos<-benthos


####diets & benthos from dep vs nondep
dietsDEP<-subset(final.diets,DepNonDep=="DEP")
benthosDEP <- subset(final.benthos, DepNonDep=="DEP")

dietsNONDEP <-subset(final.diets, DepNonDep=="NONDEP")
benthosNONDEP<-subset(final.benthos, DepNonDep=="NONDEP")

##get percentages of both diets and benthos
###
##from DEP
###
#diets
justdiets.DEP<-dietsDEP[,4:16]
#justdiets<-(justdiets^(1/2))
diet.percent.DEP <-percenta(justdiets.DEP, first=1, last=13) #dataframe of percentages in stomach
diet.percent.DEP[is.na(diet.percent.DEP)]<-0
diet.percent.DEP<-diet.percent.DEP[ , which(names(diet.percent.DEP) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                               ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                               ,"Trombidiformes","Veneroida"))]


#benthos:
justbenthos.DEP<-benthosDEP[,4:16]
#justbenthos<-(justbenthos^(1/2))
benthos.percent.DEP <-percenta(justbenthos.DEP, first=1, last=13) #dataframe of percentages in benthos
benthos.percent.DEP<-benthos.percent.DEP[ , which(names(benthos.percent.DEP) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                               ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                               ,"Trombidiformes","Veneroida"))]


###
##from NONDEP
###
#diets:
justdiets.NONDEP<-dietsNONDEP[,4:16]
#justdiets<-(justdiets^(1/2))
diet.percent.NONDEP <-percenta(justdiets.NONDEP, first=1, last=13) #dataframe of percentages in stomach
diet.percent.NONDEP[is.na(diet.percent.NONDEP)]<-0
diet.percent.NONDEP<-diet.percent.NONDEP[ , which(names(diet.percent.NONDEP) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                               ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                               ,"Trombidiformes","Veneroida"))]

#benthos
justbenthos.NONDEP<-benthosNONDEP[,4:16]
#justbenthos<-(justbenthos^(1/2))
benthos.percent.NONDEP <-percenta(justbenthos.NONDEP, first=1, last=13) #dataframe of percentages in benthos
benthos.percent.NONDEP<-benthos.percent.NONDEP[ , which(names(benthos.percent.NONDEP) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                               ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                               ,"Trombidiformes","Veneroida"))]




#rename columns for graph
colnames(diet.percent.NONDEP)[colnames(diet.percent.NONDEP)=="Trombidiformes"]<-"Acari"
colnames(diet.percent.NONDEP)[colnames(diet.percent.NONDEP)=="Veneroida"]<-"Dreissinidae"
colnames(diet.percent.NONDEP)[colnames(diet.percent.NONDEP)=="Diptera"]<-"Chironomidae"
colnames(diet.percent.DEP)[colnames(diet.percent.DEP)=="Trombidiformes"]<-"Acari"
colnames(diet.percent.DEP)[colnames(diet.percent.DEP)=="Veneroida"]<-"Dreissinidae"
colnames(diet.percent.DEP)[colnames(diet.percent.DEP)=="Diptera"]<-"Chironomidae"
colnames(diet.percent.NONDEP)[colnames(diet.percent.NONDEP)=="Trombidiformes"]<-"Acari"
colnames(diet.percent.NONDEP)[colnames(diet.percent.NONDEP)=="Veneroida"]<-"Dreissinidae"
colnames(diet.percent.NONDEP)[colnames(diet.percent.NONDEP)=="Diptera"]<-"Chironomidae"
colnames(benthos.percent.NONDEP)[colnames(benthos.percent.NONDEP)=="Trombidiformes"]<-"Acari"
colnames(benthos.percent.NONDEP)[colnames(benthos.percent.NONDEP)=="Veneroida"]<-"Dreissinidae"
colnames(benthos.percent.NONDEP)[colnames(benthos.percent.NONDEP)=="Diptera"]<-"Chironomidae"
colnames(benthos.percent.DEP)[colnames(benthos.percent.DEP)=="Trombidiformes"]<-"Acari"
colnames(benthos.percent.DEP)[colnames(benthos.percent.DEP)=="Veneroida"]<-"Dreissinidae"
colnames(benthos.percent.DEP)[colnames(benthos.percent.DEP)=="Diptera"]<-"Chironomidae"
#order columns alphabetically
diet.percent.NONDEP<-diet.percent.NONDEP[,order(names(diet.percent.NONDEP))]
diet.percent.DEP<-diet.percent.DEP[,order(names(diet.percent.DEP))]
benthos.percent.NONDEP<-benthos.percent.NONDEP[,order(names(benthos.percent.NONDEP))]
benthos.percent.DEP<-benthos.percent.DEP[,order(names(benthos.percent.DEP))]

colnames(diet.percent.DEP)
colnames(benthos.percent.DEP)
colnames(diet.percent.NONDEP)
colnames(benthos.percent.NONDEP)

#####################
####calculate E...finally
#E= ri-pi/ri+pi
##ri= % of each critter in the gut
##pi= % of each critter in the benthos


##DEP VS NONDEP
E.DEP<-((diet.percent.DEP)-(benthos.percent.DEP))/((diet.percent.DEP)+(benthos.percent.DEP))

boxplot(E.DEP, use.cols = TRUE, main="Goby prey selection: DEP", ylab="Ivlev's Electivity Index (E)", las=2, par(mar =c(7, 4, 4, 2) + 0.1))

E.NONDEP<-((diet.percent.NONDEP)-(benthos.percent.NONDEP))/((diet.percent.NONDEP)+(benthos.percent.NONDEP))
boxplot(E.NONDEP, use.cols = TRUE, main="Goby prey selection: NONDEP", ylab="Ivlev's Electivity Index (E)", las=2, par(mar =c(7, 4, 4, 2) + 0.1))



#######################
###Schoener's index--compares overlap in diets of ROGs in dep vs nondep habitats####
#######################

all.diets <- cast(CompiledFishLogandDiets, YearSERUnique + Year + DepNonDep + SizeClass + Empty.Stomach. + Cheese.~ Family, value='Total.Organisms.sum', sum)
diets <- subset(all.diets,Empty.Stomach.=="N")  #deletes empties
diets <- diets[!is.na(diets$DepNonDep),] #deletes DepNonDep=NA
diets <- diets[!is.na(diets$SizeClass),] #deletes SizeClass=NA
diets <- subset(diets, Cheese.== "N") #deletes cheese fish
diets<- diets[, !apply(diets==0,2,all)] #deletes all columns = 0
diets$"Fish bones" <-NULL
diets$"Fish Egg" <-NULL
diets$"Fish Spp" <-NULL


##to get Schoener's index by year
diets11 <-subset(diets, Year=="2011") 
diets12 <-subset(diets, Year=="2012")
diets13 <-subset(diets, Year=="2013") 



###Choose which columns to  keep in your analysis (all the numeric cols of species)
cols.keep<-c(8:31) ##remember to change this!!
cnames <- colnames(diets13)[cols.keep]

library(spaa)

propns<-decostand(as.matrix(diets13[,cols.keep]), MARGIN=1, method="total")#gets the proportions per row
propns <- as.data.frame(propns)
colnames(propns)<- cnames
colnames(propns) <- paste("Propn.", colnames(propns), sep="") #change all the colnames so we know they are propns now
                         
                         #now we need to combine this shit back together with the original data
                         propns$YearSERUnique<-diets13$YearSERUnique 
                         diets13<- join(diets13[,c(1,3,4)], propns, by="YearSERUnique")
                         diets13 <- diets13[,-1] #deletes YearSERUnique
                         library(reshape)
                         md <- melt(diets13, id=(c("DepNonDep", "SizeClass")))
                         mndiet <- cast(md,  variable + SizeClass ~ DepNonDep, mean)
                         
                         # Now compute diet overlap between same sized fishes of the two species
                         dietOverlap(mndiet[mndiet$SizeClass=="1",3:4],prey=levels(mndiet$variable),type="Schoener")
                         dietOverlap(mndiet[mndiet$SizeClass=="2",3:4],prey=levels(mndiet$variable),type="Schoener")
                         dietOverlap(mndiet[mndiet$SizeClass=="3",3:4],prey=levels(mndiet$variable),type="Schoener")
                         

#################################################################################
####CANONICAL CORRESPONDENCE ANALYSIS####
####see http://ecology.msu.montana.edu/labdsv/R/labs/lab12/lab12.html for tutorial
##################################################################################
##########################
####data from examples online:
brycesite <- read.table(file="F:\\DATA\\SLBE\\Erica's crap\\Data\\brycesite.r", header=TRUE)
bryceveg <- read.table(file="F:\\DATA\\SLBE\\Erica's crap\\Data\\bryceveg.r", header=TRUE)

attach(brycesite)
cca.1 <- cca(bryceveg~elev+east+pos)

attach(brycesite)
bryce.cca<-cca(bryceveg~elev+slope+av)

plot(bryce.cca)
############################
##get environmental variables for ALL diets


d<-subset(d, Year>="2012")
d<-d[!(d$YearSERUnique=="2012-445-2"),]
d$poop<-1


##make df for environmental variables--you can change these
diet.env<- summaryBy(poop ~ YearSERUnique+Year + Depth.m.standard + Water.Temp.C +  
                      SizeClass+ GeneralLoc+ Site.Condition.For.Event +DayNum + Total.Time..decimal.+Avg.trap.depth.m.1 , FUN=mean, data=d)
#row.names(diet.env)<-diet.env$YearSERUnique
diet.env$YearSERUnique<-NULL


write.csv(diet.env,"diet_env.csv")

##make species df
diet.items<- cast(d, YearSERUnique ~ Family, value='Total.Organisms.sum', sum)
#row.names(diet.items)<-diet.items$YearSERUnique #makes rownames out of the YearSERUniqe col
diet.items$YearSERUnique<-NULL
diet_mat<-data.matrix(diet.items)

#diet.items$totes<- rowSums(diet.items[,1:25])
#diet.items$totes<-NULL

attach(diet.env)
cca(diet.items ~ SizeClass+ GeneralLoc+ Site.Condition.For.Event +DayNum + Total.Time..decimal.+Avg.trap.depth.m.1 )


plot(gobydiet.cca)






diet2013.cca <-cca(dietfreq2013~SizeClass
                   + DepNonDep)

diet2013.cca.plot <- ordiplot(diet2013.cca)

plot(diet2013.cca)








###just try with continuous variables from 2013
#data2013 <-subset(env.diet.join, Year.1=="2013")

#e2013 <-subset(data2013[,1:10])
#row.names(e2013)<-e2013$YearSERUnique
#e2013<-e2013[,-1]


#dietfreq2013 <-subset(data2013[,c(1,12:45)])
#row.names(dietfreq2013)<-dietfreq2013$YearSERUnique
#dietfreq2013<-dietfreq2013[,-1]


attach(e2013)
cca(dietfreq2013 ~ Water.Temp.C + Oxygen..mg.L. +pH)
diet2013.cca <-cca(dietfreq2013~SizeClass
                   + DepNonDep)

diet2013.cca.plot <- ordiplot(diet2013.cca)

plot(diet2013.cca)

###note 4March2015:  got analysis to work but dont know what it means. so im an idiot.
####################################################################
################COMMUNITY ANALYSIS ON DIETS#########################
####################################################################

#run first
######################
d <-CompiledFishLogandDiets
d <-subset(CompiledFishLogandDiets, Cheese.=="N") #remove cheesiness
d$Month<- month(d$Date)
d<-d[!(d$GeneralLoc %in% "SleepingBearShoals"),]
d<-d[!(d$Diet.Item %in% c("Fish", "Fish Egg", "Empty", "Fish Spp (whole or mostly)", "Fish scales" ,"???")),]
d<-d[!(d$Order %in% c("Fish", "???", "Empty")),]


diet.w.group<-cast(d, YearSERUnique + Year + DepNonDep +Site.Condition.For.Event
                    +Depth.m.standard+SizeClass + GeneralLoc+ Empty.Stomach. + Cheese.~ Family, value='Total.Organisms.sum', sum)
diet.w.group <-subset(diet.w.group, Year >= "2012") #get everything from 2012-2013
diet.w.group <-na.omit(diet.w.group)
diet.w.group$totes<- rowSums(diet.w.group[,10:36]) #gets rowsums of everything
diet.w.group <-subset(diet.w.group,totes>0) #deletes everyting w/ rowsums of 0 (still some in there from fish scales in database :\)
diet.w.group$totes<- NULL
diet.w.group$Cheese. <-NULL
diet.w.group$Empty.Stomach. <-NULL


groups <- diet.w.group[,1:7] #make yer groups
sitecond<-diet.w.group[,c(1,4)]
#groups$YearSERUnique<-NULL
food<-diet.w.group[,-c(2:7)] #get the foods

food.dist<-vegdist(diet.w.group[,(8:34)], method="bray") #get bray-curtis distances

##########
##anosim##
##########

colnames(groups)  

attach(groups)
anosim(food.dist, Year, permutations=999) ##you can change out the grouping factor

#################
##NMDS on diets##
#################


food.transform <-(food^(1/4)) #fourth root transform the dater
food.transform<-as.data.frame(food.transform)

diets<-cbind(sample,food.transform)
row.names(diets)<-diets$sample


diets<-diets[,-1]


diets<-diet.w.group[,-c(2:7)] #get the foods
diet.site<-join(sitecond,diets, by=("YearSERUnique"), match="all")
diet.site<-diet.site[,-1]
diet_mat <-data.matrix(diet.site)
diet.nmds<-metaMDS(diet_mat, distance="bray", k=2, wascores=FALSE)
diet.nmds

orditorp(diet.nmds, display="sites", main="what do fish eat???????????????", pch=19)

write.csv(diet.nmds$points, "dietpoints.csv")




###Trying to make all the points colors of corresponding dep/year/size/etc...
###kind of working
scl <-3 ##scaling ==3
colvec <- c( "red2", "green4", "mediumblue")

plot(diet.nmds,scaling= scl,xlim=c(-.01,.05),ylim=c(-0.02,0.02), main="what do fish eat???????????????")

with(groups, points(diet.nmds, display = "sites", col = colvec[SizeClass],
                    scaling = scl, pch = 21, bg = colvec[SizeClass])) #works w/ scl=3

with(groups, legend("topright", legend = levels(SizeClass), bty = "n",
                    col = colvec, pch = 21, pt.bg = colvec))


scl <-2 ##scaling ==2
colvec <- c("green4", "mediumblue")

plot(diet.nmds,scaling= scl,xlim=c(-.015,.005),ylim=c(-0.005,0.006), main="what do fish eat???????????????")

with(groups, points(diet.nmds, display = "sites", col = colvec[DepNonDep],
                    scaling = scl, pch = 21, bg = colvec[DepNonDep])) #DOESNT work with scl=2

with(groups, legend("topright", legend = levels(DepNonDep), bty = "n",
                    col = colvec, pch = 21, pt.bg = colvec))


#######################################################

#Yeah so i want to make a table that has some descriptive stuff about the sampling
#things like date, gear type, site name, condition, coordinates then size ranges
#of fish, mean SL things, number of empties..things like that


descript <-f[,c("Event", "Year.1","YearSER","Date", "Month","Site.Condition.For.Event", "Depth.m.standard",
                "GeneralLoc","Gear","SampleType", "Lat", "Long", "Cheese.")]
descript <- subset(descript, Cheese.=="N") #remove cheesiness

#how many NAs for site.cond.for.event
sum(is.na(descript$Site.Condition.For.Event))


#OOOOOOOOOOOK.  is there any relationship between empty stomachs and time in trap?

d <- CompiledFishLogandDiets
d <- subset(CompiledFishLogandDiets, Cheese.=="N") #remove cheesiness

#get number of empties per SER
empties <- subset(d,Empty.Stomach.=="Y")
num.empties<-summaryBy(Empty.Stomach. ~ YearSER + Total.Time..decimal., data=d, FUN=length)
write.csv(num.empties, "empties.csv")

###fill in NAS for lat long to make a table
LL<-read.csv("F:\\DATA\\SLBE\\R scripts\\Round Goby Diet Analysis\\csv to use\\YearSERLatLong.csv", header=TRUE, sep=",")
LL<- LL[!is.na(LL$Lat & LL$Long),]
names(LL)[names(LL)=="Lat"] <-"Lat.general"
names(LL)[names(LL)=="Long"] <-"Long.general"

descript<-join(descript,LL, by=c("YearSER"))
##gillnets dont have lat/long, we will need to fix this at some point

descript.final<-summaryBy(Event~Date+Year.1+Month+GeneralLoc+Gear+Lat+Long, data=descript, FUN=mean)
write.csv(descript.final,"SamplingDescriptions.csv")


