#########################################
##########BENTHOS MEGA SCRIPT############
#### for biomass and frequency calcs#####
#########################################

###Code by Taaja Tucker
###Last update 7/24/14

###RUN FIRST###
setwd("F:/DATA/SLBE/R scripts/Diet biomass and frequency/")#set directory
logfile <- file("output.log") #creates a backlog of the script activity (look in folder)
sink(logfile, append=TRUE) 
sink(logfile, append=TRUE, type="message")
source("dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000) #Load the data and run calculations
sink()  # Restore output to console
sink(type="message")
rm(fishdiet2,NA.omit.biomass)
#Now manually open the output.log file in the folder and check for any warnings
###############

##Files available to work with:
#1.  CompiledFishLogandDiets - MOST IMPORTANT FINAL PRODUCT - contains frequency/count data for  
#diet data (Total Orgs and Count) but also the total biomass for each taxon. 
#You can see the intermediate steps in these calculation processes in the following columns:
#Av.biomass.mg- the average biomass (mg) per measured critter
#Sum.biomass.mg - The sume of the biomass of each critter per taxon per SER
#Prpn.biomass.calc - The ratio of critters measured / number counted
#Final.biomass.mg -  Av.biomass.mg*Total.Organisms (this is inherently the sum of biomass if Prpn.biomass.calc=1)

#2. CompiledFishLog -  This file has information for each net/trapset (whether or not it was processed) and shows the associated BT info, the video ratings, weather, and more!


###GUIDE TO SOME OF THE THINGS YOU'LL FIND IN THE FILE: CompiledBenthosLogandFreq
# "Total.Organisms.sum" - Sum of all organisms found per SER / taxon. Heads are included if they were counted as an organism and rolled up into one category
# e.g. A chironomidae head was counted as one organism and 5 chironomidae TL were counted. This value would = 6, and the taxon would be "Chironomidae"                       
# "Num.critters.averaged.4.biomass"
#"Av.biomass.mg"  - the average biomass of the animals measured                           
#"Sum.biomass.mg" - the total biomass of ONLY the animals measured                           
#"Prpn.biomass.calc" - The proportion of the animals that were measured compared to the total counted     
#"Final.biomass.mg"   -  Av.biomass.mg  *  Total.Organisms.sum                                           
#"Max.BT.depth.m" - The maximum depth the BT was dropped to by SER, which was used roll up BT data with benthos data
#"BT.YearSer"  - the YearSER of the BT drop    
#"BT.Time.Diff.Days" - the difference in the number of days between the BT drop and the net/trap
#"BT.depth.and.trap.diff.m"  - the difference in the depth between the maximum BT drop depth and the net/trap depth
#"DepthDifference.m" - the difference between the standard depth (e.g. 10, 20, 30 m) and the actual depht of the net/trap                                                          
#"Video.Time.Diff.Days"  - the difference in the number of days between the video recording and the net/trap 
#"GeneralLoc" - the general location of the sample (e.g. South Manitou)                                
#"DepNonDep"  - Based on the site designations (2013) or the video ratings (from 2012).                                
#"DieoffYear"  - True or False. 2010&2012=dieoff years, 2011+2013 not die off years.    


#Below are examples of calculations to use. You can make your own by changing the variables in the equations!
######BIOMASS CALCULATIONS########
##Calculate total biomass
names(CompiledFishLogandDiets)
sum(CompiledFishLogandDiets$Final.biomass.mg, na.rm = TRUE)
summary(CompiledFishLogandDiets$Final.biomass.mg)

######Biomass determination by SER
#mean biomass
mean.biomass.mg <- cast(CompiledFishLogandDiets, Year + Site.Condition.For.Event + Depth.m.standard ~ Family, mean, value='Final.biomass.mg') 
#Total biomass
sum.biomass.mg <- cast(CompiledFishLogandDiets, Order ~  Year + Site.Condition.For.Event + Depth.m.standard, sum, value='Final.biomass.mg')

######Freq determination by Year and general location
freq.year <- cast(CompiledFishLogandDiets, Year + Site ~ Family, value='Total.Organisms.sum', sum)


