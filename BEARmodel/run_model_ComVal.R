####Model running code for BEARmod v.0.6
rm(list=ls())
library(data.table) # fread - fastly reading data
library(lubridate)

# setwd('//worldpop.files.soton.ac.uk/Worldpop/Projects/WP519091_Seasonality')
# setwd('D:/OneDrive - University of Southampton/Wuhan Coronavirus R0/Spread risk')
#setwd('C:/Users/sl4m18/OneDrive - University of Southampton/Wuhan Coronavirus R0/Spread risk')


source("bearmod_fx.R")
# source("bearmod/bearmod_fx.R")
source("preprocess_ComVal.R")
#Initial parameters
NPat = length(patNames)
patnInf = rep(0,NPat)
patnExp = c(rep(0,NPat) )

#pat_locator$pop = 4941509
#three provinces 
pat_locator$pop = rep(0, NPat)
pat_locator$pop[1] = 575470
pat_locator$pop[2] = 2540707
pat_locator$pop[3] = 1825332

#start infection in Comunidad Valenciana
patnInf[which(patNames == 1)] = 5
patnInf[which(patNames == 2)] = 25
patnInf[which(patNames == 3)] = 5

#recovery rate variable for each available day 
recover_df = data.frame(date = seq(from=min(movement_data$date),to=max(movement_data$date),by="days"),recrate = recrate)

##load a new mobility scenario 
relative_move_df=data.frame()
relative_move_data = data.frame()
 
#relative_move_data = read.table("file://C:/Users/nuria/OneDrive/myProjects/COVID-19/BEARmod-master/BEARmod-master/zeroMobility_CV.csv",sep=",",header=T)

#convert dates to format R can read
#relative_move_data$date = ymd("2020-02-25")+30 
 
#### Running the model  ####

HPop = InitiatePop(pat_locator,patnInf,patnExp)
###dates of simulation

#input_dates = rep("2020-02-26",30)
#input_dates=append(input_dates,rep("2020-03-27",15))
#input_dates=append(input_dates,rep("2020-04-11",50))
input_dates = seq(from=min(movement_data$date),to=max(movement_data$date),by="days") # unique(movement_data$date)

#input_dates = seq(from=min(movement_data$date),to=max(movement_data$date),by="days")
#input_dates = seq(date("2020-02-26"),date("2020-4-26"),by="days") # coresponding to the period from 2020-12-08 to 2 wks after LNY's day 
# input_dates = seq(date("2013-12-02"),date("2014-2-27"),by="days") # coresponding to the period from 2020-12-08 to 4 wks after LNY's day
results = list()

HPop_update2 = runSim(HPop,pat_locator,relative_move_data,movement_data, input_dates,recover_df, exposerate,exposepd,exposed_pop_inf_prop = 0, TSinday = 1)
run=1
results[[run]] = HPop_update2$all_spread
onerun <- data.frame(results[run])


for (run in 1:2){
  
  HPop_update2 = runSim(HPop,pat_locator,relative_move_data,movement_data, input_dates,recover_df, exposerate,exposepd,exposed_pop_inf_prop = 0, TSinday = 1)
  #print(paste0("Run # ",run))
  results[[run]] = HPop_update2$all_spread
}
#######################
# Store simulation data
#######################
results <- as.data.frame(results)
write.table(results,file="simulation.csv",sep = "\t", row.names = F)
######################
# Plot simulation data 
######################
par(mfrow=c(2,2))
plot(results$inf_1~as.Date(results$dates,"%y/%m/%d"),
      type='l',col="blue",ylab="I", xlab="Date")
plot(results$exp_1~as.Date(results$dates,"%y/%m/%d"),
      type='l',col="blue",ylab="E", xlab="Date")
plot(results$rec_1~as.Date(results$dates,"%y/%m/%d"),
      type='l',col="blue",ylab="R", xlab="Date")
plot(results$sus_1~as.Date(results$dates,"%y/%m/%d"),
     type='l',col="blue",ylab="S", xlab="Date")
# this is just one run of the model instance 10 
#onerun <- data.frame(results[run])
#plot 
#infected 
#plot(onerun$X3)
#onerun$X3[35:50]
#save(results,file="results_ComVal.RData")
# 