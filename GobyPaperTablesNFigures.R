#figure and table scripts for round goby diet paper

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
library(nlme)

#  RUN THESE FIRST TO GET THE NECESSARY FILES FOR THIS SCRIPT TO RUN ########
source("F:/DATA/SLBE/R scripts/Diet biomass and frequency/dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
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

#font_import(pattern='times') #Get times new roman
#y #you have to say y to it to get a new font
#loadfonts(device = "win")

Goby_theme <- theme_bw()+theme(
  text = element_text(family="Times New Roman", colour="black", size=12))+
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

Month_scale <-scale_x_discrete("Month", labels = c("5"= "May","6" = "Jun","7" = "Jul","8" = "Aug","9" = "Sep","10" = "Oct", "11"="Nov"))
########################
###run second
d <- CompiledFishLogandDiets
d <- subset(CompiledFishLogandDiets, Cheese.=="N") #remove cheesiness
d$Month <- month(d$Date)
d <-d[-which(d$GeneralLoc %in% "SleepingBearShoals"),]
d <-d[-which(d$Diet.Item %in% c("Fish", "Fish Egg", "Empty", "Fish Spp (whole or mostly)", "Fish scales" ,"???")),]
d <-d[-which(d$Order %in% c("Fish", "???", "Empty")),]
d <-subset(d, Year>="2012")
d<-d[!is.na(d$Site.Condition.For.Event),]
d<-d[!is.na(d$SizeClass),]

f<- CompiledFishLog
f$Month <- month(f$Date)
f$Year<-year(f$Date)
#Extract processed fish only
levels(f$Status)
f <- f[which(f$Status %in% c('Frozen - Stomach Removed - Stomach Processed','Processed')), ]
f <-subset(f, Year>="2012")
f<-f[!is.na(f$Site.Condition.For.Event),]
f<-f[!is.na(f$SizeClass),]

####script to make a table that has the %F (the number of times a diet item shows up in the diets)
#make a dataframe that has just the things that you need
df<-subset(d[,c("YearSERUnique", "Year","SizeClass", "Site.Condition.For.Event","Diet.Item","Family", "Total.Organisms.sum")])
df<-df[!is.na(df$Site.Condition.For.Event),]
df <-df[!(df$Diet.Item %in% c("Fish", "Fish Egg", "Empty", "Fish Spp (whole or mostly)", "Fish scales" ,"???")),]

df<-summaryBy(Total.Organisms.sum~YearSERUnique+Year+SizeClass+Site.Condition.For.Event+Family, data=df, FUN=c(sum))

#make a df that has the yearsers w/ sizeclass and site condition in a good format
df1<- cast(d, YearSERUnique+Year+SizeClass+Site.Condition.For.Event ~ Diet.Item, value='Total.Organisms.sum', sum)
df1<-df1[!is.na(df1$Site.Condition.For.Event),]

#count the number of times a particular diet item appears in a stomach per year, size class, and site condition
diet.item.count<-!is.na(df$SizeClass)
diet.item.count<-count(df,c( "Year", "Site.Condition.For.Event", "SizeClass", "Family" ))

#count the number of fish per year, site condition, and size class
yearser.count<-count(f,c("Year.1","Site.Condition.For.Event", "SizeClass"))
names(yearser.count)[names(yearser.count)=="Year.1"]<-"Year"
names(yearser.count)[names(yearser.count)=="freq"]<-"Tot.Fish.for.SizeClass"

#find the percent of times that a diet item appears in a stomach per year, site condition and size class
diet.tally<-join(diet.item.count, yearser.count, by=c("Year","Site.Condition.For.Event","SizeClass"), match="all")
diet.tally$perc.F<-((diet.tally$freq/diet.tally$Tot.Fish.for.SizeClass)*100)

#this csv file will have everything that you need in it to go into excel and make tables.
#I used a pivot table to make the one that is currently in the paper
#write.csv(diet.tally,"diet tally.csv")

#################################################
####find the mean number of organisms per stomach:
DF<-subset(d[,c("YearSERUnique", "Year","SizeClass", "Site.Condition.For.Event","Order","Family", "Total.Organisms.sum")])
DF<-DF[!is.na(DF$Site.Condition.For.Event),]
write.csv(DF, "DF with orders changed")
mean.num.perstomach<- summaryBy(Total.Organisms.sum ~ Year+SizeClass+Site.Condition.For.Event, data=DF, FUN= c(mean, sd, se))
names(mean.num.perstomach)[names(mean.num.perstomach)=="Total.Organisms.sum.mean"]<-"avg"
#this csv has been made into an easy graph in excel.
#write.csv(mean.num.perstomach, "mean prey items per stomach.csv")


###trying to make a nicer grpah, but FAILING.
# Define the top and bottom of the errorbars
attach(mean.num.perstomach)
limits <- aes(ymax = avg + Total.Organisms.sum.se , ymin=avg - Total.Organisms.sum.se )

ggplot(mean.num.perstomach, aes(x=factor(Site.Condition.For.Event),y=avg, fill=SizeClass))+
  geom_bar(stat="identity", position="dodge")+Goby_theme +Month_scale +facet_grid(.~Year)+
  geom_errorbar(limits, width=0.25) +ylab("Mean number of prey items per fish")


##################################################
####Find the number of chironomids eaten by year and site condition
DF<-subset(d[,c("YearSERUnique", "Year","SizeClass", "Site.Condition.For.Event","Family", "Month","Total.Organisms.sum")])
DF<-DF[!is.na(DF$Site.Condition.For.Event),]
num.chiron<-subset(DF, Family=="Chironomidae") 
chiron.summary<-summaryBy(Total.Organisms.sum~ Year+Site.Condition.For.Event, data=num.chiron, FUN=c(mean, sd, se))
names(chiron.summary)[names(chiron.summary)=="Total.Organisms.sum.mean"]<-"avg"
#num.chiron$logchiron<-log10(num.chiron$Total.Organisms.sum)

###
#barplot of number of chironomids in Site condition for both years
ggplot(chiron.summary, aes(x=factor(Site.Condition.For.Event), avg))+
  geom_bar(stat="identity")+Goby_theme +facet_grid(.~Year)

###
#num chirons by month
chiron.month<-summaryBy(Total.Organisms.sum~ Year+Month+Site.Condition.For.Event, data=num.chiron, FUN=c(mean, sd, se))
names(chiron.month)[names(chiron.month)=="Total.Organisms.sum.mean"]<-"avg"

# Define the top and bottom of the errorbars
limits <- aes(ymax = avg + Total.Organisms.sum.se , ymin=avg - Total.Organisms.sum.se )

###
#barplot of number of chironomids in Site condition for both years
ggplot(chiron.month, aes(x=factor(Month), avg))+
  geom_bar(stat="identity", fill="#999999")+Goby_theme +Month_scale +facet_grid(Site.Condition.For.Event~Year)+
  geom_errorbar(limits, width=0.25) +ylab("Mean number of chironomid larvae in diet")

##################################################
###dreissenids per site condition and year by mo
DF<-subset(d[,c("YearSERUnique", "Year","SizeClass", "Site.Condition.For.Event","Family", "Month","Total.Organisms.sum")])
DF<-DF[!is.na(DF$Site.Condition.For.Event),]
num.muss<-subset(DF, Family=="Dreissenidae") 
muss.summary<-summaryBy(Total.Organisms.sum~ Year+Site.Condition.For.Event, data=num.muss, FUN=c(mean, sd, se))
names(muss.summary)[names(muss.summary)=="Total.Organisms.sum.mean"]<-"avg"


#barplot of number of mussels in Site condition for both years
ggplot(muss.summary, aes(x=factor(Site.Condition.For.Event), avg))+
  geom_bar(stat="identity")+Goby_theme +facet_grid(.~Year)

#num mussels by month
muss.month<-summaryBy(Total.Organisms.sum~ Year+Month+Site.Condition.For.Event, data=num.muss, FUN=c(mean, sd, se))
names(muss.month)[names(muss.month)=="Total.Organisms.sum.mean"]<-"avg"

# Define the top and bottom of the errorbars
attach(muss.month)
limits <- aes(ymax = avg + Total.Organisms.sum.se , ymin=avg - Total.Organisms.sum.se )

#barplot of number of mussels in Site condition for both years
ggplot(muss.month, aes(x=factor(Month), avg))+
  geom_bar(stat="identity", fill="#999999")+Goby_theme +Month_scale +facet_grid(Site.Condition.For.Event~Year)+
  geom_errorbar(limits, width=0.25) +ylab("Mean number of dreissenids in diet")

##################################################
###make faceted bar plot with the general groupings of diet items

###first, get all the stuff you need
Grab<- d[ , c("Month","Order","Year","Total.Organisms.sum","SizeClass","GeneralLoc","Site.Condition.For.Event","Family", "YearSERUnique", "Habitat")]

###then make a column that is for the general grouping w/ this stupid nested ifelse statement
Grab$Grouping<-ifelse(Grab$Family=="Chironomidae", "Chironomid",
                      ifelse(Grab$Family=="Dreissenidae", "Dreissenid", 
                             ifelse(Grab$Family=="Ostracoda", "Ostracod", 
                                    ifelse(Grab$Family=="Asellidae", "Isopod", 
                                           ifelse(Grab$Family=="Sphaeriidae", "Sphaeriid", 
                                                  ifelse(Grab$Family %in% c("Amphipoda","Pontoporeiidae", "Gammaridae"), "Amphipod", 
                                                         ifelse(Grab$Family %in% c("Chydoridae", "Harpacticoida"),"Benthic Zooplankton",
                                                                ifelse(Grab$Family %in% c("Bosminidae", "Calanoida", "Cercopagididae", "Cyclopoida", "Daphniidae"), "Pelagic Zooplankton",
                                                                       ifelse(Grab$Family %in% c("Cladocera sp", "Copepoda"), "Other Zooplankton", "Other")))))))))





DIETS <- summaryBy(Total.Organisms.sum~ Year +Site.Condition.For.Event +Grouping, data=Grab, FUN=c(mean, sd, se)) 
names(DIETS)[names(DIETS)=="Total.Organisms.sum.mean"]<-"avg"


DIETS$Grouping<- factor(DIETS$Grouping, levels = c("Pelagic Zooplankton","Benthic Zooplankton","Other Zooplankton",
                                                   "Chironomid","Dreissenid", "Ostracod", "Sphaeriid", "Isopod",
                                                   "Amphipod", "Other"))

attach(DIETS)
limits <- aes(ymax = avg + Total.Organisms.sum.se , ymin=avg - Total.Organisms.sum.se )

ggplot(DIETS, aes(x=factor(Grouping),avg))+
  geom_bar(stat="identity", position="dodge",  fill="#999999")+Goby_theme +facet_grid(Site.Condition.For.Event~Year)+
  labs(x="Prey item", y="Average number per diet")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_errorbar(limits, width=0.25)

###
#stacked bar graph of number of diet item groupings in Site condition for both years
#grouped by size class. ugly AF, but it is easy to visualize.


#have to rerun this to get size classes back in there
DIETS <- summaryBy(Total.Organisms.sum~ Year +Site.Condition.For.Event +SizeClass+Grouping, data=Grab, FUN=c(mean, sd, se)) 
names(DIETS)[names(DIETS)=="Total.Organisms.sum.mean"]<-"avg"

ggplot(DIETS, aes(x=factor(SizeClass), y=avg, fill=Grouping))+
  geom_bar(stat="identity", position="fill")+Goby_theme +facet_grid(Site.Condition.For.Event~Year)+
  ylab("Mean number of prey items per diet")+ scale_fill_manual(values=cbPalette)


##find out how many sites there are that are live/bare/dep per month & year
bare.sites<-subset(d, Site.Condition.For.Event=="BARE")
bare.sites<-summaryBy(Event~Site+GeneralLoc+Site.Condition.For.Event+Month+Year, data=bare.sites, FUN=mean)
bare.sum<-summaryBy(Site.Condition.For.Event~Month+Year, data=bare.sites, FUN=length)
write.csv(bare.sum, "baresites.csv")

live.sites<-subset(d, Site.Condition.For.Event=="LIVE")
live.sites<-summaryBy(Event~Site+GeneralLoc+Site.Condition.For.Event+Month+Year, data=live.sites, FUN=mean)
live.sum<-summaryBy(Site.Condition.For.Event~Month+Year, data=live.sites, FUN=length)
write.csv(live.sum, "livesites.csv")

sloughed.sites<-subset(d, Site.Condition.For.Event=="SLOUGHED")
sloughed.sites<-summaryBy(Event~Site+GeneralLoc+Site.Condition.For.Event+Month+Year, data=sloughed.sites, FUN=mean)
sloughed.sum<-summaryBy(Site.Condition.For.Event~Month+Year, data=sloughed.sites, FUN=length)
write.csv(sloughed.sum, "sloughedsites.csv")

###make a stacked bargraph for the percentages of each group in the diets.

###then make a column that is for the general grouping w/ this stupid nested ifelse statement
Grab$Grouping<-ifelse(Grab$Family=="Chironomidae", "Chironomid",
                      ifelse(Grab$Family=="Dreissenidae", "Dreissenid", 
                             ifelse(Grab$Family=="Ostracoda", "Ostracod", 
                                    ifelse(Grab$Family=="Asellidae", "Isopod", 
                                           ifelse(Grab$Family %in% c("Chydoridae", "Harpacticoida"),"Benthic Zooplankton",
                                                  ifelse(Grab$Family %in% c("Bosminidae", "Calanoida", "Cercopagididae", "Cyclopoida", "Daphniidae"), "Pelagic Zooplankton",
                                                         ifelse(Grab$Family %in% c("Cladocera sp", "Copepoda"), "Other Zooplankton", "Other")))))))



agg2 <- aggregate(data=Grab, Total.Organisms.sum~ Year+Site.Condition.For.Event+Grouping, function(x) sum(x))

#write.csv(agg2, "percent bug table.csv")

###get proportions of each diet item per size class, year only
###Basically a table that has all the percentages that the figure above does
proptable<- summaryBy(Total.Organisms.sum~ Year+Site.Condition.For.Event+Grouping, data=agg2, FUN=(sum))
colnames(proptable)[4] <-"t"
sitecondprop<-ddply(proptable,.(Year,Site.Condition.For.Event),transform,prop=t/sum(t))



######
#################################################
####find the mean number of organisms per stomach:
DF<-subset(d[,c("YearSERUnique", "Year", "Site.Condition.For.Event","Order","Family", "Total.Organisms.sum")])
DF<-DF[!is.na(DF$Site.Condition.For.Event),]
#write.csv(DF, "DF with orders changed")
sum.perstomach<-summaryBy(Total.Organisms.sum~Year+Site.Condition.For.Event+YearSERUnique, data=DF, FUN=sum)
names(sum.perstomach)[names(sum.perstomach)=="Total.Organisms.sum.sum"]<-"t"
mean.num.perstomach<- summaryBy(t ~ Year+Site.Condition.For.Event, data=sum.perstomach, FUN= c(mean, sd, se))
names(mean.num.perstomach)[names(mean.num.perstomach)=="t.mean"]<-"avg"

attach(mean.num.perstomach)
limits <- aes(ymax = avg + t.se , ymin=avg - t.se )

ggplot(mean.num.perstomach, aes(x=factor(Site.Condition.For.Event),avg))+
  geom_bar(stat="identity", position="dodge", fill="#999999")+Goby_theme +facet_grid(.~Year)+
  geom_errorbar(limits, width=0.25) +ylab("Average number of prey items per fish")+
  xlab("Site condition")



