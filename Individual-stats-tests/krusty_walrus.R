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



#Krusty walrus tests on number of chironomids vs site type per year...
#will probs need to ask jean about a nonparametric ancova to compare all the sites and the years

DF<-subset(d[,c("YearSERUnique", "Year","SizeClass", "Site.Condition.For.Event","Family", "Month","Total.Organisms.sum")])
DF<-DF[!is.na(DF$Site.Condition.For.Event),]
num.chiron<-subset(DF, Family=="Chironomidae")
c2012<-subset(num.chiron, Year=="2012")
c2013<-subset(num.chiron, Year=="2013")

kruskal.test(Total.Organisms.sum~Site.Condition.For.Event, data=c2012)
kruskal.test(Total.Organisms.sum~Site.Condition.For.Event, data=c2013)


#test number of mussels vs site type per year
num.muss<-subset(DF, Family=="Dreissenidae")
d2012<-subset(num.muss, Year=="2012")
d2013<-subset(num.muss, Year=="2013")

kruskal.test(Total.Organisms.sum~Site.Condition.For.Event, data=d2012)
kruskal.test(Total.Organisms.sum~Site.Condition.For.Event, data=d2013)



#difference between avg number of things eaten per hab?
###first, get all the stuff you need
Grab<- d[ , c("Month","Order","Year","Total.Organisms.sum","SizeClass","GeneralLoc","Site.Condition.For.Event","Family", "YearSERUnique")]


sums<-summaryBy<-summaryBy(Total.Organisms.sum~Year+Site.Condition.For.Event+YearSERUnique, data=Grab, FUN=sum)
names(sums)[names(sums)=="Total.Organisms.sum.sum"]<-"total"

sums2012<-subset(sums, Year=="2012")
sums2013<-subset(sums, Year=="2013")

kruskal.test(total~Site.Condition.For.Event, data=sums2012)
kruskal.test(total~Site.Condition.For.Event, data=sums2013)





###extra stuff for testing equal variance and normality using residuals
names(newCPUE)


#log transform CPUE
newCPUE$logCPUE<-log10(newCPUE$CPUE.adj.per.trap)
newCPUE<-subset(newCPUE, ROG.total>"0")
CPUE2012<-subset(newCPUE, Year=="2012")
CPUE2012.lm = lm(logCPUE ~ Month, data=CPUE2012)
CPUE2012.res = resid(CPUE2012.lm)

plot(CPUE2012.res)