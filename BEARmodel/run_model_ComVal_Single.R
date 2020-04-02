####Model running code for BEARmod v.0.6
rm(list=ls())
library(data.table) # fread - fastly reading data
library(lubridate)

# setwd('//worldpop.files.soton.ac.uk/Worldpop/Projects/WP519091_Seasonality')
# setwd('D:/OneDrive - University of Southampton/Wuhan Coronavirus R0/Spread risk')
#setwd('C:/Users/sl4m18/OneDrive - University of Southampton/Wuhan Coronavirus R0/Spread risk')


source("bearmod_fx.R")
# source("bearmod/bearmod_fx.R")
source("preprocess_ComVal_Single.R")
#Initial parameters
NPat = length(patNames)
patnInf = rep(0,NPat)
patnExp = c(rep(0,NPat) )

#pat_locator$pop =
pat_locator$pop = rep(0, NPat)
pat_locator$pop[1] = 4963703

#start infection in Comunidad Valenciana
patnInf[which(patNames == "CV")] = 35

input_dates = seq(from=date("2020-02-26"),to=date("2020-03-31"),by="days") # unique(movement_data$date

#recovery rate variable for each available day 
recover_df = data.frame(date = input_dates,recrate = recrate)

##load a new mobility scenario 
relative_move_data = data.frame()

#### Running the model  ####

HPop = InitiatePop(pat_locator,patnInf,patnExp)

results = list()

HPop_update2 = runSim(HPop,pat_locator,relative_move_data,movement_data, input_dates,recover_df, exposerate,exposepd,exposed_pop_inf_prop = 0, TSinday = 1)
run=1
results[[run]] = HPop_update2$all_spread
onerun <- data.frame(results[run])

plot(onerun$CV)

save(results,file="results_ComVal_Single.RData")
# 