#############################################################################
########This section runs the above analysis but splits everything up into 
########2012 and 2013.

dietsDEP2012<-subset(final.diets,DepNonDep=="DEP" & Year=="2012")
benthosDEP2012 <- subset(final.benthos, DepNonDep=="DEP" & Year=="2012")

dietsNONDEP2012 <-subset(final.diets, DepNonDep=="NONDEP" & Year=="2012")
benthosNONDEP2012<-subset(final.benthos, DepNonDep=="NONDEP" & Year=="2012")

##get percentages of both diets and benthos
###
##from DEP
###
#diets
justdiets.DEP2012<-dietsDEP2012[,4:16]
#justdiets<-(justdiets^(1/2))
diet.percent.DEP2012 <-percenta(justdiets.DEP2012, first=1, last=13) #dataframe of percentages in stomach
diet.percent.DEP2012[is.na(diet.percent.DEP2012)]<-0
diet.percent.DEP2012<-diet.percent.DEP2012[ , which(names(diet.percent.DEP2012) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                                        ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                                        ,"Trombidiformes","Veneroida"))]


#benthos:
justbenthos.DEP2012<-benthosDEP2012[,4:16]
#justbenthos<-(justbenthos^(1/2))
benthos.percent.DEP2012 <-percenta(justbenthos.DEP2012, first=1, last=13) #dataframe of percentages in benthos
benthos.percent.DEP2012<-benthos.percent.DEP2012[ , which(names(benthos.percent.DEP2012) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                                                 ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                                                 ,"Trombidiformes","Veneroida"))]

###
##from NONDEP
###
#diets:
justdiets.NONDEP2012<-dietsNONDEP2012[,4:16]
#justdiets<-(justdiets^(1/2))
diet.percent.NONDEP2012 <-percenta(justdiets.NONDEP2012, first=1, last=13) #dataframe of percentages in stomach
diet.percent.NONDEP2012[is.na(diet.percent.NONDEP2012)]<-0
diet.percent.NONDEP2012<-diet.percent.NONDEP2012[ , which(names(diet.percent.NONDEP2012) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                                                 ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                                                 ,"Trombidiformes","Veneroida"))]


#benthos
justbenthos.NONDEP2012<-benthosNONDEP2012[,4:16]
#justbenthos<-(justbenthos^(1/2))
benthos.percent.NONDEP2012 <-percenta(justbenthos.NONDEP2012, first=1, last=13) #dataframe of percentages in benthos
benthos.percent.NONDEP2012<-benthos.percent.NONDEP2012[ , which(names(benthos.percent.NONDEP2012) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                                                          ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                                                          ,"Trombidiformes","Veneroida"))]


#rename columns for graph
colnames(diet.percent.NONDEP2012)[colnames(diet.percent.NONDEP2012)=="Trombidiformes"]<-"Acari"
colnames(diet.percent.NONDEP2012)[colnames(diet.percent.NONDEP2012)=="Veneroida"]<-"Dreissenidae"
colnames(diet.percent.DEP2012)[colnames(diet.percent.DEP2012)=="Trombidiformes"]<-"Acari"
colnames(diet.percent.DEP2012)[colnames(diet.percent.DEP2012)=="Veneroida"]<-"Dreissinidae"
colnames(diet.percent.NONDEP2012)[colnames(diet.percent.NONDEP2012)=="Trombidiformes"]<-"Acari"
colnames(diet.percent.NONDEP2012)[colnames(diet.percent.NONDEP2012)=="Veneroida"]<-"Dreissenidae"
colnames(benthos.percent.NONDEP2012)[colnames(benthos.percent.NONDEP2012)=="Trombidiformes"]<-"Acari"
colnames(benthos.percent.NONDEP2012)[colnames(benthos.percent.NONDEP2012)=="Veneroida"]<-"Dreissenidae"
colnames(benthos.percent.DEP2012)[colnames(benthos.percent.DEP2012)=="Trombidiformes"]<-"Acari"
colnames(benthos.percent.DEP2012)[colnames(benthos.percent.DEP2012)=="Veneroida"]<-"Dreissenidae"

#order columns alphabetically
diet.percent.NONDEP2012<-diet.percent.NONDEP2012[,order(names(diet.percent.NONDEP2012))]
diet.percent.DEP2012<-diet.percent.DEP2012[,order(names(diet.percent.DEP2012))]
benthos.percent.NONDEP2012<-benthos.percent.NONDEP2012[,order(names(benthos.percent.NONDEP2012))]
benthos.percent.DEP2012<-benthos.percent.DEP2012[,order(names(benthos.percent.DEP2012))]

colnames(diet.percent.DEP2012)
colnames(benthos.percent.DEP2012)
colnames(diet.percent.NONDEP2012)
colnames(benthos.percent.NONDEP2012)


#####################
####calculate E...finally
#E= ri-pi/ri+pi
##ri= % of each critter in the gut
##pi= % of each critter in the benthos


##DEP VS NONDEP
E.DEP2012<-((diet.percent.DEP2012)-(benthos.percent.DEP2012))/((diet.percent.DEP2012)+(benthos.percent.DEP2012))
boxplot(E.DEP2012, use.cols = TRUE, main="Goby food selection: DEP 2012", ylab="Ivlev's Electivity Index (E)", las=2,par(mar =c(7, 4, 4, 2) + 0.1))

E.NONDEP2012<-((diet.percent.NONDEP2012)-(benthos.percent.NONDEP2012))/((diet.percent.NONDEP2012)+(benthos.percent.NONDEP2012))
boxplot(E.NONDEP2012, use.cols = TRUE, main="Goby food selection: NONDEP 2012", ylab="Ivlev's Electivity Index (E)", las=2,par(mar =c(7, 4, 4, 2) + 0.1))





####FROM2013
dietsDEP2013<-subset(final.diets,DepNonDep=="DEP" & Year=="2013")
benthosDEP2013 <- subset(final.benthos, DepNonDep=="DEP" & Year=="2013")

dietsNONDEP2013 <-subset(final.diets, DepNonDep=="NONDEP" & Year=="2013")
benthosNONDEP2013<-subset(final.benthos, DepNonDep=="NONDEP" & Year=="2013")


##get percentages of both diets and benthos
###
##from DEP
###
#diets
justdiets.DEP2013<-dietsDEP2013[,4:16]
#justdiets<-(justdiets^(1/2))
diet.percent.DEP2013 <-percenta(justdiets.DEP2013, first=1, last=13) #dataframe of percentages in stomach
diet.percent.DEP2013[is.na(diet.percent.DEP2013)]<-0
diet.percent.DEP2013<-diet.percent.DEP2013[ , which(names(diet.percent.DEP2013) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                                        ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                                        ,"Trombidiformes","Veneroida"))]


#benthos:
justbenthos.DEP2013<-benthosDEP2013[,4:16]
#justbenthos<-(justbenthos^(1/2))
benthos.percent.DEP2013 <-percenta(justbenthos.DEP2013, first=1, last=13) #dataframe of percentages in benthos
benthos.percent.DEP2013<-benthos.percent.DEP2013[ , which(names(benthos.percent.DEP2013) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                                                 ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                                                 ,"Trombidiformes","Veneroida"))]

###
##from NONDEP
###
#diets:
justdiets.NONDEP2013<-dietsNONDEP2013[,4:16]
#justdiets<-(justdiets^(1/2))
diet.percent.NONDEP2013 <-percenta(justdiets.NONDEP2013, first=1, last=13) #dataframe of percentages in stomach
diet.percent.NONDEP2013[is.na(diet.percent.NONDEP2013)]<-0
diet.percent.NONDEP2013<-diet.percent.NONDEP2013[ , which(names(diet.percent.NONDEP2013) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                                                 ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                                                 ,"Trombidiformes","Veneroida"))]


#benthos
justbenthos.NONDEP2013<-benthosNONDEP2013[,4:16]
#justbenthos<-(justbenthos^(1/2))
benthos.percent.NONDEP2013 <-percenta(justbenthos.NONDEP2013, first=1, last=13) #dataframe of percentages in benthos
benthos.percent.NONDEP2013<-benthos.percent.NONDEP2013[ , which(names(benthos.percent.NONDEP2013) %in% c( "Amphipoda", "Cladocera","Copepoda","Diptera"
                                                                                                          ,"Gastropoda","Isopoda","Oligochaeta","Ostracoda"
                                                                                                          ,"Trombidiformes","Veneroida"))]


#rename columns for graph
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

#order columns alphabetically
diet.percent.NONDEP2013<-diet.percent.NONDEP2013[,order(names(diet.percent.NONDEP2013))]
diet.percent.DEP2013<-diet.percent.DEP2013[,order(names(diet.percent.DEP2013))]
benthos.percent.NONDEP2013<-benthos.percent.NONDEP2013[,order(names(benthos.percent.NONDEP2013))]
benthos.percent.DEP2013<-benthos.percent.DEP2013[,order(names(benthos.percent.DEP2013))]

colnames(diet.percent.DEP2013)
colnames(benthos.percent.DEP2013)
colnames(diet.percent.NONDEP2013)
colnames(benthos.percent.NONDEP2013)


#####################
####calculate E...finally
#E= ri-pi/ri+pi
##ri= % of each critter in the gut
##pi= % of each critter in the benthos


##DEP VS NONDEP
E.DEP2013<-((diet.percent.DEP2013)-(benthos.percent.DEP2013))/((diet.percent.DEP2013)+(benthos.percent.DEP2013))
boxplot(E.DEP2013, use.cols = TRUE, main="Goby food selection: DEP 2013", ylab="Ivlev's Electivity Index (E)", las=2,par(mar =c(7, 4, 4, 2) + 0.1))

E.NONDEP2013<-((diet.percent.NONDEP2013)-(benthos.percent.NONDEP2013))/((diet.percent.NONDEP2013)+(benthos.percent.NONDEP2013))
boxplot(E.NONDEP2013, use.cols = TRUE, main="Goby food selection: NONDEP 2013", ylab="Ivlev's Electivity Index (E)", las=2,par(mar =c(7, 4, 4, 2) + 0.1))
