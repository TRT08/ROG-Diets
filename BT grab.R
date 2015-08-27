
# Get benthos/scope data from Database
db <- "F:/DATA/SLBE/AVBOT Database.accdb"
con2 <- odbcConnectAccess2007(db)
BT <- sqlFetch(con2, "BT Environmental Data")
close(con2)

colnames(BT) <- make.names(colnames(BT), unique = TRUE)
DownCasts <- BT[which(BT$Cast.Direction=="Down"),]

dc <- data.table(DownCasts, key=c("Site","Date"))
dc <- data.table(DownCasts)
maxdepth<- dc[, list(Depth..m.=max(Depth..m.)), by=c("SER","Site","Date")]
maxdepthBT <- join(maxdepth, DownCasts, by =c("SER","Site","Date","Depth..m."), type = "left")
maxdepthBT <- data.frame(maxdepthBT)
names(maxdepthBT)[names(maxdepthBT)=="Depth..m."] <- "Avg.trap.depth.m"

#Freq.All.benthos
#Roll up BT data with diets using the closest site,date, and depth compared to the average trap/net depth
DT <- data.table(Freq.All.diets, key = c("Site","Date"))
tm <- data.table(maxdepthBT, key = key(DT))
test <- tm[DT, roll='nearest', allow.cartesian=TRUE]

##Add the date of the BT drop
BTdateyear <- BT[,c("Date","YearSer")]
names(BTdateyear)[names(BTdateyear)=="Date"] <- "BT.Date"
BTdateyear<- unique(BTdateyear)
test2 <- join(test, BTdateyear, by ="YearSer", type = "left", match = "all")
withBT <- data.frame(test2)
names(withBT)[names(withBT)=="YearSer"] <- "BT.YearSer"
names(withBT)[names(withBT)=="Avg.trap.depth.m"] <- "Max.BT.depth.m"
withBT$BT.Time.Diff.Days<- abs(difftime(withBT$BT.Date, withBT$Date, units="days"))

#Add difference in max.bt.depth and the average trap/net depth
"Max.BT.depth.m""Avg.trap.depth.m.1"
withBT$BT.depth.and.trap.diff.m <- abs(withBT$Max.BT.depth.m-withBT$Avg.trap.depth.m.1)

#remove extra columns created by merge with video
withBT <- withBT[ , -which(names(withBT) %in% c("ID1","ID","Year","SER","Pressure..psi.","Flag,Scan.Count","Original.Fluorescence.ECO.AFL.FL..mg.m.3.","Cast.Direction"))] 

rm(DT,tm,test,test2,BTdateyear,BT)

#write <- withBT[withBT$Year.1=="2013",]
#write.csv(write,"test.csv")
