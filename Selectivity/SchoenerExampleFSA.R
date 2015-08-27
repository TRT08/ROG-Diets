library(spaa)

data(datasample)

source("http://www.rforge.net/FSA/InstallFSA.R")
library(FSA)
?FSA
?dietOverlap

# Hypothetical data -- prey categories and biomasses in diets
names <- c("bluegill","perch","minnows","bullheads","insects","zooplankton")
lmb <- c(55,35,23,7,3,1)
wae <- c(23,45,2,17,7,2)

# demonstrate using a single matrix rather than two separate vectors
diet <- cbind(lmb,wae)
rownames(diet) <- names
dietOverlap(diet,prey=rownames(diet),type="Schoener")

## An extended example which requires summarization of raw data
data(TroutDietSL)
# add percentage per fish, add zeroes for empty stomachs
TroutDietSL$pvol.mysis <- TroutDietSL$vol.mysis/TroutDietSL$vol.ttl
TroutDietSL$pvol.oi <- TroutDietSL$vol.oi/TroutDietSL$vol.ttl
TroutDietSL$pvol.fish <- TroutDietSL$vol.fish/TroutDietSL$vol.ttl
# add empty variable
TroutDietSL$empty <- ifelse(TroutDietSL$vol.ttl==0,"YES","NO")
# create TL from SL for Bull Trout using formula in paper -- TL=4.403+1.118SL
TroutDietSL$tl[TroutDietSL$species=="BLT"] <- 4.403+1.118*TroutDietSL$sl[TroutDietSL$species=="BLT"]
# add LCat length categories
TroutDietSL$LCat <- lencat(TroutDietSL$tl,breaks=c(0,350,500,650,950),right=TRUE)

# isolate the non-empty fish (as the authors did)
TroutDietSL1 <- Subset(TroutDietSL,empty=="NO")
with(TroutDietSL1,table(species,LCat))

# summarize by computing the mean percent of the three diet items
mn.mysis <- aggregate(pvol.mysis~species*LCat,data=TroutDietSL1,FUN=mean)
mn.oi <- aggregate(pvol.oi~species*LCat,data=TroutDietSL1,FUN=mean)
mn.fish <- aggregate(pvol.fish~species*LCat,data=TroutDietSL1,FUN=mean)
mndiet <- cbind(mn.mysis,mn.oi[,"pvol.oi"],mn.fish[,"pvol.fish"])
colnames(mndiet)[3:5] <- c("mysis","oi","fish")
# reorganize the result
mndiet <- reshape(mndiet,direction="long",varying=c("mysis","oi","fish"),v.names="mnprop",
                  idvar=c("species","LCat"),timevar="item",times=c("mysis","oi","fish"))
mndiet <- reshape(mndiet,direct="wide",v.names="mnprop",idvar=c("item","LCat"),timevar="species")

