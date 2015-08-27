library(vegan)

##trying to do nmds on diets
all.diets <- cast(CompiledFishLogandDiets, YearSERUnique + Year + DepNonDep + SizeClass + Empty.Stomach. + Cheese.~ Family, value='Total.Organisms.sum', sum)
diets <- subset(all.diets,Empty.Stomach.=="N")  #deletes empties
diets <- subset(diets, Cheese.== "N") #deletes cheese fish
diets$Empty.Stomach. <-NULL
diets$Cheese. <-NULL


diets[c("DepNonDep", "SizeClass")][is.na(diets[c("DepNonDep", "SizeClass")])] <- "a" #changes all NAs to the letter a just cuz

factors<-subset(diets[,1:4])
sample <-paste(factors[,1], factors[,2], factors[,3], factors[,4])##makes rowname that has dep/year/sizeclass in it

diets<-cbind(sample,diets)
diets <- diets[ ,-which(names(diets) %in% c("YearSERUnique","i.Year","DepNonDep","SizeClass" ))]

diets <- diets[, colSums(diets == 0) != nrow(diets)]  #gets rid of columns that =0
diets<-diets[rowSums(diets)!=0,]##need to get rid of the rows that =0

row.names(diets)<-diets$sample 
diets<-diets[,-1]

diet_mat <-data.matrix(diets)
diet_dist<-vegdist(diet_mat, method="bray", binary=FALSE)

diet.nmds<-metaMDS(diet_dist, distance = "bray", k = 2) 
plot(diet.nmds)
