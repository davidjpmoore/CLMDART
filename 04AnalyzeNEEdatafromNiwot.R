#Purpose: Create annual, weekly totals for NEE for Niwot Ridge Ameriflux and compare to published estimates from Knowles et al | also plot time series of NEE vs Snowmelt
#Author: davidjpmoore@email.arizona.edu
#Date: 03/11/2015
#To Do: create rDate for this file
load("data/FluxData/USNR1.rda")
USNR1 = aggFluxtemp
#Definitions
# 1800 seconds per 30minute timestep
SecPerTstep = 1800
# 86400 seconds per day
SecPerDay= 86400
# 3.156e+7 seconds per year
SecPerYear= 3.156e+7
# 44.01 g/mol is the molecular weight of CO2
MolWtCO2 = 44.01 #g/mol 
# 12.0107 g/mol is the molecular weight of C
MolWtC = 12.0107 #g/mol 
umolToMol = 10^6 #umols in mol

#convert NEE into gC/m & account for 30 minute timesteps
NEEgm= USNR1$NEE/umolToMol*MolWtC*SecPerTstep 

# colnames(USNR1_1999)=AmerifluxCols
# plot(USNR1_1999$NEE,USNR1_1999$FC)
# 
# NEE_FC99 = (USNR1_1999$NEE-USNR1_1999$FC)
# plot (USNR1_1999$UST, USNR1_1999$NEE)
USNR1$GAP1 =factor(USNR1$GAP)
# USNR1_1999$GAP1 =factor(USNR1_1999$GAP)

# ###### Warning these are big plots - they work but they are large

# plot (USNR1$DTIME, USNR1$GAP)
# library(ggplot2)
# a <- ggplot(USNR1, aes(x=UST, y=NEE))
# a + geom_point(colour="grey50", size = 4) + geom_point(aes(colour = GAP)) + scale_colour_gradient(low = "red", high="yellow")
# 
# b <- ggplot(USNR1, aes(x=UST, y=NEE))
# b + aes(shape = factor(GAP1)) +
#   geom_point(aes(colour = factor(GAP1)), size = 4, alpha=0.2) 


# b99 <- ggplot(USNR1_1999, aes(x=UST, y=NEE))
# b99 + aes(shape = factor(GAP1)) +
#   geom_point(aes(colour = factor(GAP1)), size = 4, alpha=0.5) 

#calculating annual NEE, GPP, RE totals
NEE_ann=tapply(NEEgm[USNR1$YEAR>1998],USNR1$YEAR[USNR1$YEAR>1998],sum)


#from Table 3 of Knowles et al 2014 John F. Knowles, Sean P. Burns, Peter D. Blanken & Russell K. Monson (2014): Fluxes of energy,water, and carbon dioxide from mountain ecosystems at Niwot Ridge, Colorado, Plant Ecology & Diversity, DOI:10.1080/17550874.2014.904950 To link to this article: http://dx.doi.org/10.1080/17550874.2014.904950

#I moved this ... find it again
Knowles2014=read.csv("data/OtherData//Knowles2014Nwt.csv",header=TRUE)  
plot (Knowles2014$SWE_mm,Knowles2014$NEE_gCm2)
plot (Knowles2014$SWE_mm,NEE_ann[USNR1$YEAR<2013])


#GPP and RE are empty for Niwot Ridge
# GPP_ann=tapply(USNR1$GPP,USNR1$YEAR,sum)
# RE_ann=tapply(USNR1$RE,USNR1$YEAR,sum)

#get year indicator for graphing
Yind_flux=(1999:max(USNR1$YEAR))
length(USNR1$ NEE)

# plot (Yind_flux,NEE_ann)

##########################################################################
#
# Graph of Daily NEE / Weekly NEE for each year wrt to SNOW and precip.  #
#
##########################################################################
#clear space
#rm (list=ls()) 

#load("D:/Dropbox/rProjectsShare/Snow/data/FluxData/USNR1.rda")
#limit to 1999- 2014
USNR1_99on = subset(aggFluxtemp,aggFluxtemp$YEAR>1998 & aggFluxtemp$YEAR<2014)

#set up date and water year
USNR1_99on$Date = ISOdate(USNR1_99on$YEAR, 1, 1)-1 + USNR1_99on$DOY
USNR1_99on$waterdate=USNR1_99on$Date+90
USNR1_99on$wateryear=as.numeric(format(USNR1_99on$waterdate, "%Y"))


#  Estimate week of year
weeknum =(USNR1_99on$DOY %/% 7)
weeknumIND =(USNR1_99on$DOY %/% 7)+ 365*USNR1_99on$YEAR-1997
# #Sequential day
#  seqDOY = USNR1_99on$DOY + 365*aggFluxtemp$YEAR-1997

#calculate weekly and daily values for plots
weeklyNEE= tapply(USNR1_99on$NEE, weeknumIND, mean, na.rm = TRUE)
DailyNEE= tapply(USNR1_99on$NEE, USNR1_99on$Date, mean, na.rm = TRUE)
DailyLE= tapply(USNR1_99on$LE, USNR1_99on$Date, sum, na.rm = TRUE)
DailySWC1= tapply(USNR1_99on$SWC1, USNR1_99on$Date, sum, na.rm = TRUE)
DailyDate= tapply(USNR1_99on$Date, USNR1_99on$Date, min, na.rm = TRUE)
DailyYear= as.numeric(tapply(USNR1_99on$YEAR, USNR1_99on$Date, min, na.rm = TRUE))


#calculate cummulative sum using dplyr package 
library(dplyr)
D= Daily %>% 
  group_by(DailyYear) %>%
  mutate(annual.NEEcumsum=cumsum(DailyNEE))
plot(alp$annual.NEEcumsum)

#load data using read table into a dataframe called "SNOWnwt"
SNOWnwt = read.table("data/SNOTELNiwot Tue Mar 10 2015.txt", head=TRUE, sep=",")
#SNOTELNiwot Tue Mar 10 2015.txt was downloaded from SNOTEL site see README file

#defining the wateryear and waterdate
SNOWnwt$rDate=as.Date(SNOWnwt$Date)
SNOWnwt$Year=as.numeric(format(SNOWnwt$rDate, "%Y"))
SNOWnwt$waterdate=SNOWnwt$rDate+90
SNOWnwt$wateryear=as.numeric(format(SNOWnwt$waterdate, "%Y"))
#subset data for 1999 through 2014
SNOWflux = subset(SNOWnwt,SNOWnwt$Year > 1998 & SNOWnwt$Year < 2014)



########################################
#     loop to plot snow and fluxes     #
########################################
for( i in min(SNOWflux$wateryear):max(SNOWflux$wateryear)){
  
  x1=SNOWflux$rDate[SNOWflux$wateryear==(i)]
  y3=SNOWflux$PrecAcc_in[SNOWflux$wateryear==(i)]
  y2=SNOWflux$SWE_in[SNOWflux$wateryear==(i)]
  y1=DailyNEE[SNOWflux$wateryear==(i)]
  
  
  plot(y1~x1,ann=FALSE,type="n", ylim=c(-5,20))
  lines(y1~x1,lwd=2,col=27)
  lines(y2~x1,lwd=2,lty=2)
  lines(y3~x1,lwd=2,lty=3)
  title("Snow accumulation",xlab="Date",ylab="Precipitation")
}

