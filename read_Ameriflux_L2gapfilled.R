
#CHECK UNITS

#Purpose: To read a set of L2 Ameriflux files and concatenate them together and save as dot.rda file with the site name | also creates dot.txt file suitable for later processing with EddyProc 
#

#Author: davidjpmoore@email.arizona.edu
#Date: 03/11/2015
#to do - generalize code to create a function where you could pass the working directory and output the concantenated file.

# set working directory
#data is located on iPlant and available from Ameriflux

# data folder architechure - 
#   "data/FluxData/"
#   "data/EddyProcInput"

#Remove ALL DATAFRAMES FROM THE ENVIRIONMENT 
rm (list=ls()) #the only way I could work out how to bind all the new files together is to create a list of all dataframes in the environment after the loop | for safety I clear all variables at the outset.
# 

# read all the .csv files in the working directory (L2 gap filled) available for NR1 (1998-2013)
#USES RELATIVE PATH to data/FluxData
tempFilelist = list.files(path = "data/FluxData/",pattern="*.csv")

#loop through each element of the list of files and assigns until length(tempFilelist)
#can't figure out how to read lines 18 and 19 as headers using this approach
for (i in 1:2) {
#for (i in 1:length(tempFilelist)) {
    
  assign(substr(tempFilelist[i], 5, 14), read.csv(file = paste0("data/FluxData/",tempFilelist[i]),skip=20,header=FALSE, na.strings=c('-9999','-6999'), stringsAsFactors=FALSE))  
}

#custom function that returns a list of all dataframes in the environment
#function origin:Bill Dunlap http://comments.gmane.org/gmane.comp.lang.r.general/312495 
#I would RATHER create the list from the loop but I haven't been able to work that out.
#The use of finddataframes (below) requires that there are NO OTHER data frames in the environment *rm (list=ls())*
finddataframes <- function(envir = globalenv()) {
  tmp <- eapply(envir,
                all.names=TRUE,
                FUN=function(obj) if (is.data.frame(obj))
                  obj else NULL)
  # remove NULL's now
  tmp[!vapply(tmp, is.null, TRUE)]
}

#uses function 
allDataFrames <- finddataframes(globalenv()) # or just finddataframes()
#combines all the dataframes using rbind
#names the new dataframe with Ameriflux code max 5 characters (without the years)
# assign(substr(tempFilelist[1], 5, 9), do.call("rbind", allDataFrames))
assign("aggFluxtemp", do.call("rbind", allDataFrames))

#name that I just assigned to the dataframe
CombinedDataFrame=substr(tempFilelist[1], 5, 9)

#Read header from file
AmFluxheader=read.csv(file = paste0("data/FluxData/",tempFilelist[i]),skip=17, strip.white=TRUE, nrows=1 ,header=FALSE, na.strings=c('-9999','-6999'),stringsAsFactors=FALSE)
AmerifluxCols = as.list(AmFluxheader)

#Read Units
AmFluxUnits= read.csv(file = paste0("data/FluxData/",tempFilelist[i]),skip=18, nrows=1 ,header=FALSE, na.strings=c('-9999','-6999'), stringsAsFactors=FALSE)

colnames(AmFluxUnits)=AmerifluxCols
#add column names to new dataframe
#colnames(USNR1) <- AmerifluxCols #works but I'd rather not hard code 'USNR1'
colnames(aggFluxtemp) <- AmerifluxCols #works but I'd rather not hard code 'USNR1'
assign(substr(tempFilelist[1], 5, 9), aggFluxtemp)

####################################################
#### save(USNR1,file="data/FluxData/USNR1.rda") ####
# save(AmFluxUnits,file="data/FluxData/AmFluxUnits.rda")
# save(AmFluxheader,file="data/FluxData/AmFluxheader.rda")
####################################################

save(aggFluxtemp, file=paste0("data/FluxData/",substr(tempFilelist[1], 5, 9),".rda"))



aggFluxtemp <- aggFluxtemp[order(USNR1$YEAR,USNR1$DOY),]
tcheckdiff = diff(USNR1$HRMIN, lag=1)


#####################################################
#                                                   #
#        Creating output file for EddyData          #
#         to allow gap filling etc using            # 
#               EddyProc Package                    #
#                                                   #
#####################################################
# 
# Year  DoY  Hour  NEE	LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar
# -	-	-	umolm-2s-1	Wm-2	Wm-2	Wm-2	degC	degC	%	hPa	ms-1
# 1998	1	0.5	-1.21	1.49	-11.77	0	7.4	4.19	55.27	4.6	0.72
# 1998	1	1	1.72	3.8	-13.5	0	7.5	4.2	55.95	4.6	0.52
# 1998	1	1.5	-9999	1.52	-18.3	0	7.1	4.22	57.75	4.3	0.22
# 1998	1	2	-9999	3.94	-17.47	0	6.6	4.23	60.2	3.9	0.2
# 1998	1	2.5	2.55	8.3	-21.42	0	6.6	4.22	59.94	3.9	0.33
# 
Year = aggFluxtemp$YEAR
DoY = aggFluxtemp$DOY
#note the format for Hour is incorrect wrt EddyProc  - L2 data is 0 30 100 130 etc | required format is 0 0.5 1 1.5 etc 

  H1 = aggFluxtemp$HRMIN +10000;
  H2 = substr(H1,2,5)
  H3_hr = as.numeric(substr(H2,1,2))
  H4_Frac = as.numeric(substr(H2,3,4))/60
  Hour = H3_hr+H4_Frac;

# Year  DoY  Hour  NEE  LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar
# -	-	-	umolm-2s-1	Wm-2	Wm-2	Wm-2	degC	degC	%	hPa	ms-1
NEE =aggFluxtemp$NEE
LE = aggFluxtemp$LE
H = aggFluxtemp$H
Rg =aggFluxtemp$Rg
Tair = aggFluxtemp$TA
Tsoil = aggFluxtemp$TS1
rH = aggFluxtemp$RH
VPD = aggFluxtemp$VPD
Ustar = aggFluxtemp$UST

#Write header required for EddyProc 

#Create dataframe
EddyProcHead = data.frame("Year",  "DoY",  "Hour",  "NEE",  "LE",  "H",	"Rg",	"Tair",	"Tsoil",	"rH",	"VPD",	"Ustar")
#Write to file
write.table(EddyProcHead, file = paste0("data/EddyProcInput/",substr(tempFilelist[1], 5, 9),".txt"), row.names = F, col.names=F, quote = F, sep="\t",append=F)


#Write header2 (units) required for EddyProc & append to the file 
#Create dataframe
EddyProcunits =data.frame("-", "-", "-",  "umolm-2s-1",  "Wm-2",  "Wm-2",	"Wm-2",	"degC",	"degC",	"%",	"hPa",	"ms-1")
#write to file
write.table(EddyProcunits, paste0("data/EddyProcInput/",substr(tempFilelist[1], 5, 9),".txt"), row.names = F, col.names=FALSE,quote = F, sep="\t",append=T)

#Write aata required for EddyProc & append to the file
#create dataframe
EddyProcDataFrame = data.frame(Year,  DoY,  Hour,  NEE,  LE,	H,	Rg,	Tair,	Tsoil,	rH,	VPD,	Ustar)
#write to file
write.table(EddyProcDataFrame, paste0("data/EddyProcInput/",substr(tempFilelist[1], 5, 9),".txt"), row.names = F, col.names=FALSE, sep="\t",append=T)

