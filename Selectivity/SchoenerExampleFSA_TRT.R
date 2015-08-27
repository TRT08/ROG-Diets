library(spaa)
library(FSA)
?FSA
?dietOverlap

## An extended example which requires summarization of raw data
data(TroutDietSL)
# add percentage per fish, add zeroes for empty stomachs


library(vegan)
propns<-decostand(TroutDietSL[,7:9], MARGIN=1, method="total") #gets the proportions per row
colnames(propns) <- paste("Propn.", colnames(propns), sep="") #change all the colnames so we know they are propns now

#now we need to combine this shit back together with the original data
propns$number<-TroutDietSL$number 
TroutDietSL<- join(TroutDietSL, propns, by="number")

TroutDietSL$empty <- ifelse(TroutDietSL$vol.ttl==0,"YES","NO")

# create TL from SL for Bull Trout using formula in paper -- TL=4.403+1.118SL

TroutDietSL$tl[TroutDietSL$species=="BLT"] <- 4.403+1.118*TroutDietSL$sl[TroutDietSL$species=="BLT"]

# add LCat length categories

TroutDietSL$LCat <- lencat(TroutDietSL$tl,breaks=c(0,350,500,650,950),right=TRUE)

# isolate the non-empty fish (as the authors did)
TroutDietSL1 <- Subset(TroutDietSL,empty=="NO")


#Remove columns we don't need for the analysis
TroutDietSL1<- TroutDietSL1[ , -which(names(TroutDietSL1) %in% c(   "number","sl","tl","weight",         
                                 "vol.ttl","vol.mysis","vol.oi","vol.fish","num.dipter",     
                                  "num.pisidium","empty"))]

library(reshape)
md <- melt(TroutDietSL1, id=(c("species", "LCat")))
mndiet <- cast(md,  variable + LCat ~ species, mean)

# Now compute diet overlap between same sized fishes of the two species
dietOverlap(mndiet[mndiet$LCat=="0",3:4],  prey=levels(mndiet$item),type="Schoener")
dietOverlap(mndiet[mndiet$LCat=="350",3:4],prey=levels(mndiet$item),type="Schoener")
dietOverlap(mndiet[mndiet$LCat=="500",3:4],prey=levels(mndiet$item),type="Schoener")
dietOverlap(mndiet[mndiet$LCat=="650",3:4],prey=levels(mndiet$item),type="Schoener")

