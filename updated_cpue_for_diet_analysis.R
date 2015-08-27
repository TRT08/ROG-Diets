setwd("F:/DATA/SLBE/R scripts/Round Goby Diet Analysis/")#set directory
source("F:/DATA/SLBE/R scripts/ROG CPUE/compiledataforCPUE.R",echo=TRUE, max.deparse.length=10000)

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

########################################################## 
#####SET UP THEME FOR YOUR GRAPHS~!!! GGPLOT!!! ##########
#font_import(pattern='times') #Get Times New Roman
#loadfonts(device = "win")

Goby_theme <- theme_bw()+theme(
  text = element_text(family="Arial", colour="black", size=14))+
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

Month_scale <-scale_x_discrete("Month", labels = c("5"= "May","6" = "Jun","7" = "Jul","8" = "Aug","9" = "Sep","10" = "Oct", "11"="Nov"))

######################################################################################################




f<- CompiledFishLog
f$Month <- month(f$Date)
#Extract processed fish only
levels(f$Status)
f <- f[which(f$Status %in% c('Frozen - Stomach Removed - Stomach Processed','Processed')), ]
f<-subset(f,Year.1>="2012")

######

####How many fish per size class in each habitat type by year
D <-summaryBy(YearSERUnique ~ Year.1 + Depth.m.standard + GeneralLoc +SizeClass, data = f, FUN=length)
D<-na.omit(D)


D2012 <-subset(D, Year.1=="2012")
D2012$Year.1=NULL
D2012 <-na.omit(D2012)

D2013 <-subset(D, Year.1=="2013")
D2013$Year.1=NULL
D2013 <-na.omit(D2013)

write.csv(D, "Diets_YearSiteLoc.csv")
sum(D$YearSERUnique.length)

################CPUE STUFF###################################

Good <- FinalFish[!FinalFish$Is.this.data.OK. %in%  c("NO","NO DATA- LOST FOREVER"),]

Good$DayNum<- yday(Good$Date.In) #Add daynumber for easy plotting
MT <- Good[which(Good$GEAR =="MT"),] #Minnow Traps only
MT$ROG.CPUE.trap.adj<- as.numeric(MT$ROG.CPUE.trap.adj)
MT<-MT[!is.na(MT$Depth.m.standard),]
MT <- MT[!MT$GeneralLoc %in%  "SleepingBearShoals",]
MT <- MT[!MT$Year  %in%  c("2011", "2010"),]
MTuse<-subset(MT,Year=="2012")#get only 2012
MT2013<-subset(MT,Year=="2013")#get only 2013

GN <- Good[which(Good$GEAR =="GN"),] #Gill nets only

###################################################

###MT cpue####

#get the data you need from the FinalFish dataframe
stuff<-subset(MT[,c("Event","Year","Site","GeneralLoc","Site.Condition.For.Event","Date.Out","Effort..hrs."
                    ,"GEAR", "Number.of.Traps","Depth.m.standard","DepNonDep","ROG","ROG.CPUE","ROG.CPUE.trap.adj", "YearSER")])
stuff$Month<- month(stuff$Date.Out)
stuff$poop<-"1"
stuff <-stuff[!(stuff$YearSER %in% c("2013-121")),] #gets rid of the outlier with ~1600 gobies
#write.csv(site.cond.per.site, "stuff.csv")

#calculate number of gobies caught and the total number of traps and the total trap time.
summary<-summaryBy(Effort..hrs.~Year+Month+GeneralLoc+Site, data=stuff, FUN=(sum))
names(summary)[names(summary)=="Effort..hrs..(sum)"]<-"Effort.hours.total"
ROGS<-summaryBy(ROG~Year+Month+GeneralLoc+Site, data=stuff, FUN=(sum))
names(ROGS)[names(ROGS)=="ROG.(sum)"]<-"ROG.total"
traps<-summaryBy(Number.of.Traps~Year+Month+GeneralLoc+Site, data=stuff, FUN=(sum))

#rename some columns from the above dataframes
names(traps)[names(traps)=="Number.of.Traps.(sum)"]<-"Traps.total"
newCPUE<-cbind(summary, ROGS$ROG.total)
names(newCPUE)[names(newCPUE)=="ROGS$ROG.total"]<-"ROG.total"
newCPUE<-cbind(newCPUE, traps$Traps.total)
names(newCPUE)[names(newCPUE)=="traps$Traps.total"]<-"Traps.total"


#calculateCPUE (more steps than i need to have but i wanted to show how i calculated this for later)
newCPUE$CPUE.reg<-((newCPUE$ROG.total/newCPUE$Effort.hours.total)) #= (total gobies/total number of hours) = CPUE for one hour
newCPUE$CPUE.reg..per.day<-(newCPUE$CPUE.reg*24) # (gobies caught/total effort hours)*24 = CPUE per trap day
newCPUE$CPUE.adj.per.trap<-(newCPUE$CPUE.reg..per.day/newCPUE$Traps.total)  #CPUE per trap day/number of traps at that site
                                                                            #=CPUE per trap day per trap

#summaries based on year, month and general loc
#first- cpue
ugh<-summaryBy(CPUE.adj.per.trap~Year+Month+GeneralLoc, data=newCPUE, FUN=c(se, mean))
names(ugh)[names(ugh)=="CPUE.adj.per.trap.(se)"]<-"CPUE.adj.per.trap.se"
names(ugh)[names(ugh)=="CPUE.adj.per.trap.(mean)"]<-"CPUE.adj.per.trap.mean"

#total time
effort.time.total<-summaryBy(Effort.hours.total~Year+Month+GeneralLoc, data=newCPUE, FUN=c(sum))
final.trap.total<-summaryBy(Traps.total~Year+Month+GeneralLoc, data=newCPUE, FUN=c(sum))
ugh<-join(ugh, effort.time.total, by=c("Year","GeneralLoc","Month"), match="all")

#final datafrmae to plot.
ugh<-join(ugh, final.trap.total, by=c("Year","GeneralLoc","Month"), match="all")
names(ugh)[names(ugh)=="Traps.total.(sum)"]<-"Traps.total"
names(ugh)[names(ugh)=="effort.time.total.(sum)"]<-"Effort.time.total"


#CPUEtable<-ugh
#CPUEtable$meanCPUE.w.se<-paste(ugh$CPUE.adj.per.trap.mean, "(", ugh$CPUE.adj.per.trap.se,")")
#write.csv(CPUEtable, "CPUEtable.csv")

#make errorbars for figure
attach(ugh)
limits <- aes(ymax = CPUE.adj.per.trap.mean + CPUE.adj.per.trap.se , ymin=CPUE.adj.per.trap.mean - CPUE.adj.per.trap.se )

####graph MT cpue####
ggplot(data=ugh, aes(x=factor(Month), y=CPUE.adj.per.trap.mean, colour=GeneralLoc)) + geom_point(size=3)+
  Goby_theme +
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0,16))+ geom_errorbar(limits, width=0.25)+
  labs(x="Month", y="mean round goby CPUE", title="Minnow trap cpue by year") +facet_grid(Year~. )+
  Month_scale




###calculate cpue for depths#####
#calculate number of gobies caught and the total number of traps and the total trap time.
stuff1<-subset(stuff,Depth.m.standard<="20")
summary1<-summaryBy(Effort..hrs.~Year+Month+GeneralLoc+Site+Depth.m.standard, data=stuff1, FUN=(sum))
names(summary1)[names(summary1)=="Effort..hrs..(sum)"]<-"Effort.hours.total"
ROGS<-summaryBy(ROG~Year+Month+GeneralLoc+Site+Depth.m.standard, data=stuff1, FUN=(sum))
names(ROGS)[names(ROGS)=="ROG.(sum)"]<-"ROG.total"
traps1<-summaryBy(Number.of.Traps~Year+Month+GeneralLoc+Site+Depth.m.standard, data=stuff1, FUN=(sum))

#rename some columns from the above dataframes
names(traps1)[names(traps1)=="Number.of.Traps.(sum)"]<-"Traps.total"
newCPUE1<-cbind(summary1, ROGS$ROG.total)
names(newCPUE1)[names(newCPUE1)=="ROGS$ROG.total"]<-"ROG.total"
newCPUE1<-cbind(newCPUE1, traps1$Traps.total)
names(newCPUE1)[names(newCPUE1)=="traps1$Traps.total"]<-"Traps.total"


#calculateCPUE (more steps than i need to have but i wanted to show how i calculated this for later)
newCPUE1$CPUE.reg<-((newCPUE1$ROG.total/newCPUE1$Effort.hours.total)) #= (total gobies/total number of hours) = CPUE for one hour
newCPUE1$CPUE.reg..per.day<-(newCPUE1$CPUE.reg*24) # (gobies caught/total effort hours)*24 = CPUE per trap day
newCPUE1$CPUE.adj.per.trap<-(newCPUE1$CPUE.reg..per.day/newCPUE1$Traps.total)  #CPUE per trap day/number of traps at that site
#=CPUE per trap day per trap

#summaries based on year, month and general loc
#first- cpue
ugh1<-summaryBy(CPUE.adj.per.trap~Year+Month+GeneralLoc+Depth.m.standard, data=newCPUE1, FUN=c(se, mean))
names(ugh1)[names(ugh1)=="CPUE.adj.per.trap.(se)"]<-"CPUE.adj.per.trap.se"
names(ugh1)[names(ugh1)=="CPUE.adj.per.trap.(mean)"]<-"CPUE.adj.per.trap.mean"

#total time
effort.time.total1<-summaryBy(Traps.total~Year+Month+GeneralLoc+Depth.m.standard, data=newCPUE1, FUN=c(sum))
final.trap.total1<-summaryBy(Effort.hours.total~Year+Month+GeneralLoc+Depth.m.standard, data=newCPUE1, FUN=c(sum))
ugh1<-join(ugh1, effort.time.total1, by=c("Year","GeneralLoc","Month", "Depth.m.standard"), match="all")

#final datafrmae to plot.
ugh1<-join(ugh1, final.trap.total1, by=c("Year","GeneralLoc","Month", "Depth.m.standard"), match="all")
names(ugh1)[names(ugh1)=="Traps.total.(sum)"]<-"Traps.total"
names(ugh1)[names(ugh1)=="effort.time.total.(sum)"]<-"Effort.time.total"

#write.csv(newCPUE, "newCPUE.csv")

#make errorbars for figure
attach(ugh1)
limits <- aes(ymax = CPUE.adj.per.trap.mean + CPUE.adj.per.trap.se , ymin=CPUE.adj.per.trap.mean - CPUE.adj.per.trap.se )

#graph cpue
ggplot(data=ugh1, aes(x=factor(Month), y=CPUE.adj.per.trap.mean, colour=GeneralLoc)) + geom_point(size=3)+
  Goby_theme +
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0,25))+ geom_errorbar(limits, width=0.25)+
  labs(x="Sampling event", y="mean round goby CPUE", title="Minnow trap cpue by depth and year") +facet_grid(Depth.m.standard~Year )+
  Month_scale


#######calculate GN cpue######
GN <- Good[which(Good$GEAR =="GN"),] #Gill nets only
GN<-GN[!is.na(GN$Depth.m.standard),]
GN <- GN[!GN$GeneralLoc %in%  "SleepingBearShoals",]
GN <- GN[!GN$Year  %in%  c("2011", "2010"),]

#get the data you need from the FinalFish dataframe
GN.stuff<-subset(GN[,c("Event","Year","Site","GeneralLoc","Site.Condition.For.Event","Date.Out","Effort..hrs."
                       ,"GEAR", "Number.of.Traps","Depth.m.standard","DepNonDep","ROG","ROG.CPUE","ROG.CPUE.trap.adj", "YearSER")])
GN.stuff$Month<- month(GN.stuff$Date.Out)
GN.stuff$poop<-"1"
#write.csv(site.cond.per.site, "stuff.csv")

#calculate number of gobies caught and the total number of traps and the total trap time.
GN.summary<-summaryBy(Effort..hrs.~Year+Month+GeneralLoc+Site, data=GN.stuff, FUN=(sum))
names(GN.summary)[names(GN.summary)=="Effort..hrs..(sum)"]<-"Effort.hours.total"
GN.ROGS<-summaryBy(ROG~Year+Month+GeneralLoc+Site, data=GN.stuff, FUN=(sum))
names(GN.ROGS)[names(GN.ROGS)=="ROG.(sum)"]<-"ROG.total"
#traps<-summaryBy(Number.of.Traps~Year+Month+GeneralLoc+Site, data=stuff, FUN=(sum))

GN.newCPUE<-cbind(GN.summary, GN.ROGS$ROG.total)
names(GN.newCPUE)[names(GN.newCPUE)=="GN.ROGS$ROG.total"]<-"ROG.total"

#calculateCPUE (more steps than i need to have but i wanted to show how i calculated this for later)
GN.newCPUE$CPUE.reg<-((GN.newCPUE$ROG.total/GN.newCPUE$Effort.hours.total)) #= (total gobies/total number of hours) = CPUE for one hour
GN.newCPUE$CPUE.per.day<-(GN.newCPUE$CPUE.reg*24) # (gobies caught/total effort hours)*24 = CPUE per trap day

#summaries based on year, month and general loc
#first- cpue
GN.ugh<-summaryBy(CPUE.per.day~Year+Month+GeneralLoc, data=GN.newCPUE, FUN=c(se, mean))
names(GN.ugh)[names(GN.ugh)=="CPUE.per.day.(se)"]<-"CPUE.per.day.se"
names(GN.ugh)[names(GN.ugh)=="CPUE.per.day.(mean)"]<-"CPUE.per.day."

#total time
GN.effort.time.total<-summaryBy(Effort.hours.total~Year+Month+GeneralLoc, data=GN.newCPUE, FUN=c(sum))
GN.ugh<-join(GN.ugh, GN.effort.time.total, by=c("Year","GeneralLoc","Month"), match="all")
names(GN.ugh)[names(GN.ugh)=="effort.time.total.sum"]<-"Effort.time.total"


#make errorbars for figure
attach(GN.ugh)
limits <- aes(ymax = CPUE.per.day.mean + CPUE.per.day.se , ymin=CPUE.per.day.mean - CPUE.per.day.se )

###graph GN cpue#####
ggplot(data=GN.ugh, aes(x=factor(Month), y=CPUE.per.day.mean, colour=GeneralLoc)) + geom_point(size=3)+
  Goby_theme +
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0,40))+ geom_errorbar(limits, width=0.25)+
  labs(x="Sampling event", y="mean round goby CPUE", title="Gill net cpue") +facet_grid(Year~. )+
  Month_scale






#############crap

CPUEtable<-newCPUE
CPUEtable$CPUE.adj.final<-paste(ugh$CPUE.adj.per.trap.mean, "(", ugh$CPUE.adj.per.trap.se,")")





newCPUE<-ddply(stuff,.(Year, Event, Month,GeneralLoc),summarise,ROG.CPUE.new=((sum(ROG)/sum(Effort..hrs.))/(sum(Number.of.Traps))))
