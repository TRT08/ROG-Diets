#count table stuff for diet analysis paper

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

######################
d <- CompiledFishLogandDiets
d <- subset(CompiledFishLogandDiets, Cheese.=="N") #remove cheesiness
d$Month <- month(d$Date)
d <-d[!(d$GeneralLoc %in% "SleepingBearShoals"),]
d <-d[!(d$Diet.Item %in% c("Fish", "Fish Egg", "Empty", "Fish Spp (whole or mostly)", "Fish scales" ,"???")),]
d <-d[!(d$Order %in% c("Fish", "???", "Empty")),]

f<- CompiledFishLog
f$Month <- month(f$Date)
#Extract processed fish only
levels(f$Status)
f <- f[which(f$Status %in% c('Frozen - Stomach Removed - Stomach Processed','Processed')), ]
f<-f[!is.na(f$Site.Condition.For.Event),]
f<-f[!is.na(f$SizeClass),]


levels(d$Diet.Item)
levels(d$GeneralLoc)
colnames(d)


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

#write.csv(diet.tally,"diet tally.csv")


#find the mean number of organisms per stomach:
DF<-subset(d[,c("YearSERUnique", "Year","SizeClass", "Site.Condition.For.Event","Order","Family", "Total.Organisms.sum")])
DF<-DF[!is.na(DF$Site.Condition.For.Event),]
write.csv(DF, "DF with orders changed")
mean.num.perstomach<- summaryBy(Total.Organisms.sum ~ Year+SizeClass+Site.Condition.For.Event, data=DF, FUN= c(mean, sd, se))
#write.csv(mean.num.perstomach, "mean prey items per stomach.csv")

#are there sig differences between mean number of prey items in a stomach between habitats & size classes in each year?
##get just 2012 data
DF2012<-subset(DF, Year=="2012")
DF2012<-summaryBy(Total.Organisms.sum~Year+YearSERUnique+SizeClass+Site.Condition.For.Event, data=DF2012, FUN=(sum))
names(DF2012)[names(DF2012)=="Total.Organisms.sum.(sum)"]<-"total"
DF2012$logtotal<-log10(DF2012$total) #log transform

#interaction plots for each variable
interaction.plot(DF2012$SizeClass, DF2012$Site.Condition.For.Event, DF2012$logtotal)
interaction.plot(DF2012$Site.Condition.For.Event,DF2012$SizeClass, DF2012$logtotal)


#ancova
results <- lm(logtotal ~ SizeClass + Site.Condition.For.Event + SizeClass*Site.Condition.For.Event, data=DF2012)
anova(results)

qqnorm(results$res)
plot(results$fitted,results$res,xlab="Fitted",ylab="Residuals")

#check normality on residuals
shapiro.test(results$res) 
#check equal variance
leveneTest(logtotal~SizeClass*Site.Condition.For.Event, data=DF2012)

##get just 2013 data
DF2013<-subset(DF, Year=="2013")
DF2013<-summaryBy(Total.Organisms.sum~Year+YearSERUnique+SizeClass+Site.Condition.For.Event, data=DF2013, FUN=(sum))
names(DF2013)[names(DF2013)=="Total.Organisms.sum.(sum)"]<-"total"
DF2013$logtotal<-log10(DF2013$total) #log transform

#interaction plots for each variable
interaction.plot(DF2013$SizeClass, DF2013$Site.Condition.For.Event, DF2013$logtotal)
interaction.plot(DF2013$Site.Condition.For.Event,DF2013$SizeClass, DF2013$logtotal)


#ancova
results <- lm(logtotal ~ SizeClass + Site.Condition.For.Event + SizeClass*Site.Condition.For.Event, data=DF2012)
anova(results)

qqnorm(results$res)
plot(results$fitted,results$res,xlab="Fitted",ylab="Residuals")

#check normality on residuals
shapiro.test(results$res) 
#check equal variance
leveneTest(logtotal~SizeClass*Site.Condition.For.Event, data=DF201)

##is there a significant difference of chironomids eaten in each habitat
##paired t test.  number of chironomids in each type of site condition between 2 years
DF<-subset(d[,c("YearSERUnique", "Year","SizeClass", "Site.Condition.For.Event","Family", "Month","Total.Organisms.sum")])
DF<-DF[!is.na(DF$Site.Condition.For.Event),]
num.chiron<-subset(DF, Family=="Chironomidae") 
chiron.summary<-summaryBy(Total.Organisms.sum~ Year+Site.Condition.For.Event, data=num.chiron, FUN=c(mean, sd, se))
names(chiron.summary)[names(chiron.summary)=="Total.Organisms.sum.mean"]<-"avg"
#num.chiron$logchiron<-log10(num.chiron$Total.Organisms.sum)

#barplot of number of chironomids in Site condition for both years
ggplot(chiron.summary, aes(x=factor(Site.Condition.For.Event), avg))+
  geom_bar(stat="identity")+Goby_theme +facet_grid(.~Year)


#num chirons by month
chiron.month<-summaryBy(Total.Organisms.sum~ Year+Month+Site.Condition.For.Event, data=num.chiron, FUN=c(mean, sd, se))
names(chiron.month)[names(chiron.month)=="Total.Organisms.sum.mean"]<-"avg"

# Define the top and bottom of the errorbars
limits <- aes(ymax = avg + Total.Organisms.sum.se , ymin=avg - Total.Organisms.sum.se )

#barplot of number of chironomids in Site condition for both years
ggplot(chiron.month, aes(x=factor(Month), avg))+
  geom_bar(stat="identity", fill="#999999")+Goby_theme +Month_scale +facet_grid(Site.Condition.For.Event~Year)+
  geom_errorbar(limits, width=0.25) +ylab("Mean number of chironomid larvae in diet")


##dreissenids per site condition and year by mo
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
