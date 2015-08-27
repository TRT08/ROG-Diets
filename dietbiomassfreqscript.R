
####################################
####GETTING STARTED - RUN FIRST ####
####################################

setwd("F:/DATA/SLBE/R scripts/Diet biomass and frequency/")

require(reshape) || install.packages("reshape") 
require(plyr) || install.packages("plyr") 
require(RODBC) || install.packages("RODBC") 
require(splitstackshape) || install.packages("splitstackshape") 
require(data.table) || install.packages("data.table") 
require(car) || install.packages("car") 
require(doBy) || install.packages("doBy") 
require(chron) || install.packages("chron") 


options(scipen=999) #Keeps scientific notation away

##########################################
#######GET DATA FROM DATABASE       ######
##########################################

# Get benthos/scope data from Database
db <- "F:/DATA/SLBE/AVBOT Database.accdb"
con2 <- odbcConnectAccess2007(db)
sqlTables(con2, tableType = "TABLE")$TABLE_NAME   #list table names
RawFishLog <- sqlFetch(con2, "Inventory Control Log - Fish")
RawDietData <- sqlFetch(con2, "Fish Diets")
RawGillNets <- sqlFetch(con2, "Serial Log - Gill Nets")
RawMinnowTraps <- sqlFetch(con2, "Serial Log - Minnow Trap")
RawDietTaxa <- sqlFetch(con2, "Potential Diet Item List")
Rawscopes <- sqlFetch(con2, "Microscope Magnifications")
Video <- sqlFetch(con2, "Serial Log - Video")
BT <- sqlFetch(con2, "BT Environmental Data")
HOBO <- sqlFetch(con2, "Combined")


close(con2)

#Make database colnames appropriate for R to use
colnames(RawFishLog) <- make.names(colnames(RawFishLog ), unique = TRUE)
colnames(RawDietData) <- make.names(colnames(RawDietData), unique = TRUE)
colnames(RawGillNets) <- make.names(colnames(RawGillNets), unique = TRUE)
colnames(RawMinnowTraps) <- make.names(colnames(RawMinnowTraps), unique = TRUE)
colnames(RawDietTaxa ) <- make.names(colnames(RawDietTaxa), unique = TRUE)
colnames(Rawscopes) <- make.names(colnames(Rawscopes), unique = TRUE)
colnames(Video) <- make.names(colnames(Video), unique = TRUE)
colnames(BT) <- make.names(colnames(BT), unique = TRUE)
colnames(HOBO) <- make.names(colnames(HOBO), unique = TRUE)
DownCasts <- BT[which(BT$Cast.Direction=="Down"),]

##########################################
#######   PREPARE THE FISH LOG      ######
##########################################

#Add ages for fish using Bin Huo 2014 equations to fish log
attach(RawFishLog)
RawFishLog$Est.age<-ifelse(Sex0Juvenile1Male2Female==2 & Lengthmm <=71, "0to 2",
                    ifelse(Sex0Juvenile1Male2Female==2 & Lengthmm >71 & Lengthmm <=80, "3",
                    ifelse(Sex0Juvenile1Male2Female==2 & Lengthmm >80 & Lengthmm <=91, "4",
                    ifelse(Sex0Juvenile1Male2Female==2 & Lengthmm >91 & Lengthmm <=99, "5",       
                    ifelse(Sex0Juvenile1Male2Female==2 & Lengthmm >99 & Lengthmm <=111, "6",
                    ifelse(Sex0Juvenile1Male2Female==2 & Lengthmm >111, "7+",
                    ifelse(Sex0Juvenile1Male2Female==1 & Lengthmm <=70, "0 to 2",
                    ifelse(Sex0Juvenile1Male2Female==1 & Lengthmm >70 & Lengthmm <=84, "3",
                    ifelse(Sex0Juvenile1Male2Female==1 & Lengthmm >84 & Lengthmm <=94, "4",
                    ifelse(Sex0Juvenile1Male2Female==1 & Lengthmm >94 & Lengthmm <=108, "5",       
                    ifelse(Sex0Juvenile1Male2Female==1 & Lengthmm >108 & Lengthmm <=131, "6 to 7",
                    ifelse(Sex0Juvenile1Male2Female==1 & Lengthmm >131, ">7",     
                    ifelse(Sex0Juvenile1Male2Female==0, "0", NA)))))))))))))
detach(RawFishLog)                                                 

#Join the gill nets and minnow traps to the fish log
nms <- colnames(RawMinnowTraps)  
Missing <- setdiff(nms, names(RawGillNets))  # Find names of missing columns
RawGillNets[Missing] <- "NA"                    # Add them, filled with '0's
RawGillNets <- RawGillNets[nms]  
class(RawGillNets$Total.Time) = c('POSIXt','POSIXct') 
test <- rbind(RawMinnowTraps, RawGillNets)
fish1 <- join(RawFishLog, test, by ="YearSER", type = "left", match = "all")

#convert feet to meters and add standard depths if true depths not available!
fish1$Depth.m.standard[fish1$Site %in% c("GH1","GHA1","GHB1","GHC1","GHD1","GHN1","SBD1","SBN1","SM1","SMA1","SMB1","SMC1","SMD1","SMN1","C2","A1","B1","C1","D1","E1")] <- 10
fish1$Depth.m.standard[fish1$Site %in% c("GH2","GHA2","GHB2","GHC2","GHD2","GHN2","SBD2","SBN2","SM2","SMA2","SMB2","SMC2","SMD2","SMN2","A2","B2","C3","C3 (569)","D2","E2")] <- 20
fish1$Depth.m.standard[fish1$Site %in% c("GHD3","GHN3","SBD3","SBN3","SMD3","SMN3")]<- 30
fish1$Start.Depth.m <- fish1$Start.Depth * 0.3048 #convert feet to meters
fish1$End.Depth.m <- fish1$End.Depth * 0.3048 #convert feet to meters
fish1 <- fish1[ ,-which(names(fish1) %in% c("Start.Depth","End.Depth"))] #remove old depth columns

#Add general location names (e.g. South Manitou)
fish1$GeneralLoc[fish1$Site %in% c("GH","GH1","GH2","GHA1" ,"GHA2","GHB1","GHB2","GHC1","GHC2","GHD1","GHD2","GHD3","GHHD","GHHN","GHN1","GHN2","GHN3","GH-S1","GH-S4","GH-SWLeg")] <- "GoodHarbor"
fish1$GeneralLoc[fish1$Site %in% c("SM","SM1","SM2","SMA1","SMA2","SMB1","SMB2","SMC1","SMC2","SMD1","SMD2","SMD3","SMN1","SMN2","SMN3","D1","D2","SM-WP699","CG","SM-S2","SM-S3")] <- "SouthManitou"
fish1$GeneralLoc[fish1$Site %in% c("SBD1","SBD2","SBD3","SBN1","SBN2","SBN3","B1","B2")] <- "SleepingBearShoals"
fish1$GeneralLoc[fish1$Site %in% c("E1","E2")] <- "NorthManitou"
fish1$GeneralLoc[fish1$Site %in% c("C1","C2","C3","C3 (569)")] <- "PlatteBay"
fish1$GeneralLoc[fish1$Site %in% c("A1","A2")] <- "PyramidPoint"

#####Die-off year
fish1$DieoffYear[fish1$Year==2010]<- TRUE
fish1$DieoffYear[fish1$Year==2011]<- FALSE
fish1$DieoffYear[fish1$Year==2012]<- TRUE
fish1$DieoffYear[fish1$Year==2013]<- FALSE

#####Find the difference between the standard depth (10,20,30m) and the actual mean depth of the nets/traps
fish1$Avg.trap.depth.m <- (fish1$Start.Depth.m+fish1$End.Depth.m)/2
fish1$DepthDifference.m <- abs(fish1$Depth.m.standard-fish1$Avg.trap.depth.m)

##############ADD VIDEO DATA#############
#Figure out site designation from video log by rolling up video SERs with the closest site and date
VideoGrab<- Video[ , c("YearSER","Site","Date","SedRate","MussRate", "CladRate","Site.Condition.For.Event")]
names(VideoGrab)[names(VideoGrab)=="YearSER"] <- "Video.YearSER"
DT <- data.table(fish1, key = c("Site","Date"))
tm <- data.table(VideoGrab, key = key(DT))
test <- tm[DT, roll='nearest', allow.cartesian=TRUE]
test <- test[!duplicated(test$YearSERUnique),] 
fish1 <- data.frame(test)
fish1 <- fish1[ , -which(names(fish1) %in% c("Year.1"))] #remove extra columns created by merge with video
fish1 <- fish1[ , -which(names(fish1) %in% c("SER.1"))]
fish1 <- fish1[ , -which(names(fish1) %in% c("Site.1"))]

##Add the date of the VideoSER to get difference in videograb
Videodateyear <- Video[,c("Date","YearSER")]
names(Videodateyear)[names(Videodateyear)=="Date"] <- "Video.Date"
names(Videodateyear)[names(Videodateyear)=="YearSER"] <- "Video.YearSER"
Videodateyear<- unique(Videodateyear)
test2 <- join(fish1, Videodateyear, by ="Video.YearSER", type = "left", match = "all")
withVideo <- data.frame(test2)
withVideo$Video.Time.Diff.Days<- abs(difftime(withVideo$Video.Date, withVideo$Date, units="days"))

##############ADD BT DATA FOR 2013##################
dc <- data.table(DownCasts, key=c("Site","Date"))
dc <- data.table(DownCasts)
maxdepth<- dc[, list(Depth..m.=max(Depth..m.)), by=c("SER","Site","Date")]
maxdepthBT <- join(maxdepth, DownCasts, by =c("SER","Site","Date","Depth..m."), type = "left")
maxdepthBT <- data.frame(maxdepthBT)
names(maxdepthBT)[names(maxdepthBT)=="Depth..m."] <- "Avg.trap.depth.m"

#Roll up BT data with diets using the closest site,date, and depth compared to the average trap/net depth
DT <- data.table(fish1, key = c("Site","Date"))
tm <- data.table(maxdepthBT, key = key(DT))
test <- tm[DT, roll='nearest', allow.cartesian=TRUE]
test <- test[!duplicated(test$YearSERUnique),]

##Add the date of the BT drop
BTdateyear <- BT[,c("Date","YearSer")]
names(BTdateyear)[names(BTdateyear)=="Date"] <- "BT.Date"
BTdateyear<- unique(BTdateyear)
test2 <- join(test, BTdateyear, by ="YearSer", type = "left", match = "all")
withBT <- data.frame(test2)
names(withBT)[names(withBT)=="YearSer"] <- "BT.YearSer"
names(withBT)[names(withBT)=="Avg.trap.depth.m"] <- "Max.BT.depth.m"
withBT$BT.Time.Diff.Days<- abs(difftime(withBT$BT.Date, withBT$Date, units="days"))


names(withBT)[names(withBT)=="i.Avg.trap.depth.m"] <- "Avg.trap.depth.m.1" 
#Add difference in max.bt.depth and the average trap/net depth
withBT$BT.depth.and.trap.diff.m <- abs(withBT$Max.BT.depth.m-withBT$Avg.trap.depth.m.1)

#remove extra columns created by merge with video and BT
withBT <- withBT[ , -which(names(withBT) %in% c("ID1","ID","Year","SER","Pressure..psi.","Flag","Scan.Count","Original.Fluorescence.ECO.AFL.FL..mg.m.3.","Cast.Direction","Elapsed.Time..s."))] 

names(withBT)[names(withBT)=="i.SER"] <- "SER.1" 
names(withBT)[names(withBT)=="i.Year"] <- "Year.1" 


#####DEP NONDEP DESIGNATIONS

withBT$DepNonDep[withBT$Site %in% c("GHD1","GHD2","GHD3","GHHD","SMD1","SMD2","SMD3","SBD1","SBD2","SBD3")]<- "DEP"
withBT$DepNonDep[withBT$Site %in% c("GHHN","GHN1","GHN2","GHN3","SMN1","SMN2","SMN3","SBN1","SBN2","SBN3")]<-"NONDEP"
withBT$DepNonDep[withBT$Site.Condition.For.Event=="SLOUGHED" & withBT$Year.1=="2012"]<-"DEP"
withBT$DepNonDep[withBT$Site.Condition.For.Event %in% c("BARE","LIVE") & withBT$Year.1=="2012"]<-"NONDEP"

#######RENAME FINAL FISH LOG
CompiledFishLog <- withBT
############################

#######HOBO DATA ROLLUP###########

HOBO$Time2 <- as.POSIXlt(HOBO$Time) 
HOBO$Time2 <- times(format(HOBO$Time2, "%H:%M:%S"))
HOBO <- subset(HOBO, select = -Time )
names(HOBO)[names(HOBO)=="Time2"] <- "Time" 
HOBO <-  within(HOBO, { timestamp=format(as.POSIXct(paste(HOBO$Date, HOBO$Time)), "%m/%d/%Y %H:%M:%S") }) #Make datetime

###Remove first and last HOBO readings to account for pick up and drop off times)
HOBO2 <- HOBO[(order(HOBO$YearSER, HOBO$ID)), ]

highest<-by(HOBO2, HOBO2$YearSER, tail, n=1)
lowest<-by(HOBO2, HOBO2$YearSER, head, n=1)

highestd<-do.call("rbind", as.list(highest))
lowestd<-do.call("rbind", as.list(lowest))

HOBO3 <- HOBO2[!HOBO2$ID %in% highestd$ID, ]
HOBO4 <- HOBO3[!HOBO3$ID %in% lowestd$ID, ]

###Now get temp and light summaries###

HOBOTempMean <- summaryBy(Temp_C ~ YearSER, data=HOBO4, FUN=mean, na.rm=TRUE)
HOBOTempCount <- summaryBy(Temp_C ~ YearSER, data=HOBO4, FUN=length)

Lux <- HOBO4[ -which(HOBO4$Intensity_Lux ==0),]
Lux <- Lux[!(is.na(Lux$Intensity_Lux)),] 
HOBOLuxMean <- summaryBy(Intensity_Lux ~ YearSER, data=Lux, FUN=mean, na.rm=TRUE)
HOBOLuxPosCount <- summaryBy(Intensity_Lux ~ YearSER, data=Lux, FUN=length)

require(plyr) #combine all the new HOBO measurements
HOBO5 <- join_all(list(HOBOTempMean,HOBOTempCount,HOBOLuxMean,HOBOLuxPosCount), by = 'YearSER', type = 'full')
HOBO5$LuxPrpnTimePeriods <- HOBO5$Intensity_Lux.length / HOBO5$Temp_C.length #get the proportion of time periods measured that contain Lux values > 0
names(HOBO5)[names(HOBO5)=="Temp_C.mean"] <- "HOBO_Temp_C.mean" 
names(HOBO5)[names(HOBO5)=="Temp_C.length"] <- "HOBO_Temp_C.length" 
names(HOBO5)[names(HOBO5)=="Intensity_Lux.mean"] <- "HOBO_Intensity_Lux.mean" 
names(HOBO5)[names(HOBO5)=="Intensity_Lux.length"] <- "HOBO_Intensity_Lux.length" 

CompiledFishLog <- join(CompiledFishLog, HOBO5, by ="YearSER", type = "left", match = "all")

###################

rm(Video, DT,tm,test,test2,BTdateyear,BT,dc,Videodateyear,maxdepth,maxdepthBT,
   withBT,withVideo,VideoGrab,fish1,RawMinnowTraps,RawFishLog,RawGillNets,DownCasts)

#Make backups for later use
DietData <- RawDietData
DietTaxa <-RawDietTaxa
Scopes <-Rawscopes

##########################################
#######START BIOMASS / LENGTH CALCS ######
##########################################

fishdiet <- subset(DietData, select = -1)
names(fishdiet)[names(fishdiet)=="Diet.Item"] <- "ID"  #Prep for joining
fishdiet <- join(fishdiet, DietTaxa, by ="ID", type = "left", match = "all")

#Remove taxon with ONLY counts - keep only the Total Organism counts (e.g. remove all chironomid heads if they don't count as total organisms)
fishdiet <- fishdiet[!(is.na(fishdiet$Total.Organisms)),] #Remove NA total orgs
fishdiet <-fishdiet[(fishdiet$Total.Organisms>0),] #Remove total org=0

#Melt diet data so each measured animal has its own row--make sure to include all columns except the "X1","X2",etc.

keep.cols <- names(fishdiet)[!names(fishdiet) %in% c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")]

meltedfishdiet<- melt(fishdiet, id=c(keep.cols))

#Change names to melt with microscope magnifiers
names(meltedfishdiet)[names(meltedfishdiet)=="variable"] <- "Animal.measured"
names(meltedfishdiet)[names(meltedfishdiet)=="value"] <- "Length"

#Merge scopes multiplier and data
fishdiet2 <- join(meltedfishdiet, Scopes, by = c("Scope.Name","Mag"), type = "left", match = "all")
fishdiet2 <- fishdiet2[ , -which(names(fishdiet2) %in% c("ID"))]

#Find true length of organisms (in mm)
fishdiet2$Length <- as.numeric(fishdiet2$Length, na.rm = TRUE)
fishdiet2$mm.length <- fishdiet2$Length * fishdiet2$Multiplier

####FORMAT MUSSEL DATA########
#Get mussel valve length from septa lengths first
fishdiet3<-fishdiet2
attach(fishdiet3)
fishdiet3$Septa.to.TL.mm<- ifelse(fishdiet3$Mass.Unit=="Septa", ((c*mm.length) - (d*(mm.length^2)) - e), NA)
detach(fishdiet3)
#change the name of the mass unit for the septas to get the biomass
fishdiet3$Mass.Unit[fishdiet3$Mass.Unit=="Septa"] <- "ln_mg"
#Make just one length column now
fishdiet3$mm.length <- ifelse(is.na(fishdiet3$Septa.to.TL.mm), fishdiet3$mm.length,fishdiet3$Septa.to.TL.mm)
fishdiet3 <- fishdiet3[ , -which(names(fishdiet3) %in% c("Septa.to.TL.mm"))] #Remove extra length column

######Biomass determination by animal
##Isolate complete cases
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
NA.omit.biomass <- completeFun(fishdiet3, c("a", "mm.length"))

names(NA.omit.biomass)[names(NA.omit.biomass)=="a"] <- "aX"
names(NA.omit.biomass)[names(NA.omit.biomass)=="b"] <- "bX"
names(NA.omit.biomass)[names(NA.omit.biomass)=="c"] <- "cX"

##GET BIOMASSES#####
#Note: make sure all mm.length = mm and all mass=mg!
#Note2: log in r = ln, and log10 in r=regular log

NA.omit.biomass$biomass.mg<- 
ifelse(NA.omit.biomass$Mass.Unit=="mg", with(NA.omit.biomass, (aX*(mm.length^bX)*factor)),
ifelse(NA.omit.biomass$Mass.Unit=="ug",with(NA.omit.biomass, (aX*(mm.length^bX)*factor)/1000), 
ifelse(NA.omit.biomass$Mass.Unit=="ln_mg", with(NA.omit.biomass, (exp(aX+log(mm.length)*bX))),
ifelse(NA.omit.biomass$Mass.Unit=="ln_ug", with(NA.omit.biomass, (exp(aX+log(mm.length)*bX))/1000), 
ifelse(NA.omit.biomass$Mass.Unit=="log_mg", with(NA.omit.biomass, 10^(aX+(bX*(log10(mm.length))))),
ifelse(NA.omit.biomass$Mass.Unit=="ng", with(NA.omit.biomass, ((aX-(bX*(mm.length*1000)) + cX*(mm.length*1000)^2)/1000000)), 
ifelse(NA.omit.biomass$Mass.Unit=="Cerco_mg", with(NA.omit.biomass, exp(aX*log(mm.length)-bX)), 
ifelse(NA.omit.biomass$Mass.Unit=="Diff_ug", with(NA.omit.biomass, (aX+mm.length^bX)/1000), 
ifelse(NA.omit.biomass$Mass.Unit=="Alon_ug", with(NA.omit.biomass, (aX*(mm.length/1000)^bX)/1000),
ifelse(NA.omit.biomass$Mass.Unit=="Illyo_g", with(NA.omit.biomass, ((aX*mm.length)-bX)*1000), 
ifelse(NA.omit.biomass$Mass.Unit=="Dia_ug", with(NA.omit.biomass, (aX*(cX*mm.length)^bX)/1000),
ifelse(NA.omit.biomass$Mass.Unit=="Actual_ug", with(NA.omit.biomass, aX/1000), 
ifelse(NA.omit.biomass$Mass.Unit=="Limno_ug", with(NA.omit.biomass, (10^(aX*mm.length-bX))/1000), 
ifelse(NA.omit.biomass$Mass.Unit=="Ostra_ug", with(NA.omit.biomass, (10^(aX*log10(mm.length)))/1000), 
ifelse(NA.omit.biomass$Mass.Unit=="log_ug", with(NA.omit.biomass, (10^(aX+(bX*(log10(mm.length)))))/1000),NA)))))))))))))))


#NA.omit.biomass$Mass.Unit=="Bythos_Spine", (ax*(((mm.length/2.2)-1.8)^bx)*factor)/1000, NA


########ADD CHANGE IN DIET ITEM NAMES HERE????#############

#####Renaming Freq/biomass critters
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Chironomidae Pupa HW")] <- "Chironomidae Pupa TL"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Chironomidae pupa")] <- "Chironomidae Pupa TL"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Chironomidae HW")] <- "Chironomidae"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Chironomidae HW - Head Only")] <- "Chironomidae"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Chironomidae HL (mistake)")] <- "Chironomidae" 
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Chironominae HW")] <- "Chironominae TL"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Chironominae HW - Head Only")] <- "Chironominae TL"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Tanypodinae HW")] <- "Tanypodinae TL"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Tanypodinae HW - Head Only")] <- "Tanypodinae TL"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Gastropod TH")] <- "Gastropod TL"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Orthocladiinae Larvae HW")] <- "Orthocladiinae Larvae TL"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Orthocladiinae Larvae HW - Head Only")] <- "Orthocladiinae Larvae TL"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Ostracod half shell")] <- "Ostracod whole"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Water Mite Body alone")] <- "Water Mite TL"
levels(NA.omit.biomass$Diet.Item)[levels(NA.omit.biomass$Diet.Item) %in%  c("Z septa R","Zebra half L","Zebra half R","Zebra whole","Z septa L")] <- "Zebra"
levels(NA.omit.biomass$Diet.Item)[levels(NA.omit.biomass$Diet.Item) %in%  c("Q septa","Q septa L","Q septa R","Quagga half L","Quagga half R","Quagga whole")] <- "Quagga"
levels(NA.omit.biomass$Diet.Item)[levels(NA.omit.biomass$Diet.Item) %in%  c("Bythotrephes Spine","Bythotrephes Body + Spine","Bythotrephes Body")] <- "Bythotrephes"
levels(NA.omit.biomass$Diet.Item)[levels(NA.omit.biomass$Diet.Item) %in%  c("Lirceus BW","Lirceus BW - Butt Only","Lirceus HW - Head Only", "Lirceus HW")] <- "Lirceus TL"
levels(NA.omit.biomass$Diet.Item)[levels(NA.omit.biomass$Diet.Item) %in%  c("Caecidotea BW","Caecidotea BW - Butt Only","Caecidotea HW", "Caecidotea HW - Head Only")] <- "Caecidotea TL"
NA.omit.biomass$Diet.Item[which(NA.omit.biomass$Diet.Item =="Isopod sp HW")] <- "Isopod sp TL"

NA.omit.biomass$Diet.Item <- droplevels(NA.omit.biomass$Diet.Item)

####Get averages of biomass for each taxon per SER
mean.biomass.mg <- cast(NA.omit.biomass,  Diet.Item ~ YearSERUnique, mean, value='biomass.mg')
mean.biomass.mg  <- melt(mean.biomass.mg, id=c("YearSERUnique"))
row.names(mean.biomass.mg) <- seq(nrow(mean.biomass.mg)) 
names(mean.biomass.mg)[names(mean.biomass.mg)=="value"] <- "Av.biomass.mg"

####Get number of animals measured for each taxon per SER
length.biomass.mg <- cast(NA.omit.biomass,  Diet.Item ~ YearSERUnique, length, value='biomass.mg')
length.biomass.mg  <- melt(length.biomass.mg, id=c("YearSERUnique"))
row.names(length.biomass.mg) <- seq(nrow(length.biomass.mg)) 
names(length.biomass.mg)[names(length.biomass.mg)=="value"] <- "Num.critters.averaged.4.biomass"
length.biomass.mg[length.biomass.mg == 0] <- NA

####Get biomass sum for each taxon per SER
sum.biomass.mg <- cast(NA.omit.biomass,  Diet.Item ~ YearSERUnique, sum, value='biomass.mg')
sum.biomass.mg  <- melt(sum.biomass.mg, id=c("YearSERUnique"))
row.names(sum.biomass.mg) <- seq(nrow(sum.biomass.mg)) 
names(sum.biomass.mg)[names(sum.biomass.mg)=="value"] <- "Sum.biomass.mg"
sum.biomass.mg[sum.biomass.mg == 0] <- NA


##########################################
#######     FREQUENCY DATA         ######
##########################################
fishdietf <- subset(DietData, select = -1)
names(fishdietf)[names(fishdietf)=="Diet.Item"] <- "ID"  #Prep for joining
fishdietf <- join(fishdietf, DietTaxa, by ="ID", type = "left", match = "all")

#Remove unneeded columns
fishdietf <- fishdietf[ , -which(names(fishdietf) %in% c("SampleType","Status","ID","Location", "X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","Scope.Name","Mag", "a","b","c","d","e","factor","Mass.Unit","Source","Equation"))]

#####Renaming Freq/biomass critters
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Chironomidae Pupa HW")] <- "Chironomidae Pupa TL"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Chironomidae pupa")] <- "Chironomidae Pupa TL"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Chironomidae HW")] <- "Chironomidae"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Chironomidae HW - Head Only")] <- "Chironomidae"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Chironomidae HL (mistake)")] <- "Chironomidae" 
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Chironominae HW")] <- "Chironominae TL"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Chironominae HW - Head Only")] <- "Chironominae TL"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Tanypodinae HW")] <- "Tanypodinae TL"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Tanypodinae HW - Head Only")] <- "Tanypodinae TL"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Gastropod TH")] <- "Gastropod TL"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Orthocladiinae Larvae HW")] <- "Orthocladiinae Larvae TL"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Orthocladiinae Larvae HW - Head Only")] <- "Orthocladiinae Larvae TL"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Ostracod half shell")] <- "Ostracod whole"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Water Mite Body alone")] <- "Water Mite TL"
levels(fishdietf$Diet.Item)[levels(fishdietf$Diet.Item) %in%  c("Z septa R","Zebra half L","Zebra half R","Zebra whole","Z septa L")] <- "Zebra"
levels(fishdietf$Diet.Item)[levels(fishdietf$Diet.Item) %in%  c("Q septa","Q septa L","Q septa R","Quagga half L","Quagga half R","Quagga whole")] <- "Quagga"
levels(fishdietf$Diet.Item)[levels(fishdietf$Diet.Item) %in%  c("Bythotrephes Spine","Bythotrephes Body + Spine","Bythotrephes Body")] <- "Bythotrephes"
levels(fishdietf$Diet.Item)[levels(fishdietf$Diet.Item) %in%  c("Lirceus BW","Lirceus BW - Butt Only","Lirceus HW - Head Only", "Lirceus HW")] <- "Lirceus TL"
levels(fishdietf$Diet.Item)[levels(fishdietf$Diet.Item) %in%  c("Caecidotea BW","Caecidotea BW - Butt Only","Caecidotea HW", "Caecidotea HW - Head Only")] <- "Caecidotea TL"
fishdietf$Diet.Item[which(fishdietf$Diet.Item =="Isopod sp HW")] <- "Isopod sp TL"

fishdietf$Diet.Item <- droplevels(fishdietf$Diet.Item)

Freq.sum <- summaryBy(Total.Organisms + Count ~ YearSERUnique + Diet.Item, data=fishdietf, FUN=sum, na.rm=TRUE)

Dietinfo <- fishdietf[ ,which(names(fishdietf) %in% c("YearSERUnique","Comments","Order","Family","Diet.Item","TrophicPosition","Habitat"))]

Freq.sum.plus.info <- join(Freq.sum, Dietinfo, by =c("YearSERUnique","Diet.Item"), type = "left", match="first")

#Bind the frequency and the biomass data
combo<- join(length.biomass.mg, mean.biomass.mg, by =c("Diet.Item","YearSERUnique"), type = "left", match = "all")
combo<- join(combo, sum.biomass.mg, by =c("Diet.Item","YearSERUnique"), type = "left", match = "all")
Freq.All.diets<- join(Freq.sum.plus.info, combo, by =c("Diet.Item","YearSERUnique"), type="left")

#Add column of proportion of total critters used to get biomass estimates
Freq.All.diets$Prpn.biomass.calc <- Freq.All.diets$Num.critters.averaged.4.biomass/Freq.All.diets$Total.Organisms.sum

#Make a total biomass column 
Freq.All.diets$Final.biomass.mg <- Freq.All.diets$Av.biomass.mg*Freq.All.diets$Total.Organisms.sum
Freq.All.diets[is.na(Freq.All.diets)] <- NA #Turn NANs into NAs

#remove intermediate data frames
rm(combo,length.biomass.mg,mean.biomass.mg, sum.biomass.mg, Scopes, DietData,fishdiet3,
   DietTaxa, Dietinfo,Freq.sum,Freq.sum.plus.info, RawDietData,RawDietTaxa,Rawscopes,meltedfishdiet,fishdiet,
   fishdietf, HOBO,HOBO2,HOBO3,HOBO4,HOBO5,HOBOLuxMean,HOBOLuxPosCount,HOBOTempCount,HOBOTempMean,Lux,highestd,lowestd)
##########################################
####### COMPILE FISH LOG WITH DIETS ######
##########################################

CompiledFishLogandDiets<- join(Freq.All.diets, CompiledFishLog, by =c("YearSERUnique"), type="left")
names(CompiledFishLogandDiets)[names(CompiledFishLogandDiets)=="Year.1"] <-"Year"
names(CompiledFishLogandDiets)[names(CompiledFishLogandDiets)== "SER.1"] <-"SER"

#####RENAME COMMON SITES (same throughout years) BY PROxIMITY
CompiledFishLogandDiets$RenameSite[CompiledFishLogandDiets$Site %in% c("GH2","GHA2")] <- "GH2/GHA2"
CompiledFishLogandDiets$RenameSite[CompiledFishLogandDiets$Site %in% c("GH1","GHA1","GHHN")] <- "GH1/GHA1"
CompiledFishLogandDiets$RenameSite[CompiledFishLogandDiets$Site %in% c("GHC1","GHD1")] <- "GHC1/GHD1"
CompiledFishLogandDiets$RenameSite[CompiledFishLogandDiets$Site %in% c("SMC2","SMD3")] <- "SMC2/SMD3"
CompiledFishLogandDiets$RenameSite[CompiledFishLogandDiets$YearSER=="2011-106"] <- "SMA1"
CompiledFishLogandDiets$RenameSite[CompiledFishLogandDiets$YearSER=="2012-115"] <- "SMC2"
CompiledFishLogandDiets$RenameSite2 <- as.factor(ifelse(is.na(CompiledFishLogandDiets$RenameSite), as.character(CompiledFishLogandDiets$Site), as.character(CompiledFishLogandDiets$RenameSite)))
CompiledFishLogandDiets <- CompiledFishLogandDiets[ , -which(names(CompiledFishLogandDiets) %in% c("RenameSite","Site"))]
names(CompiledFishLogandDiets)[names(CompiledFishLogandDiets)=="RenameSite2"] <- "Site"

write.csv(CompiledFishLogandDiets,"test.csv")
