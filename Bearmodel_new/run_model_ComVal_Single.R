####Model running code for BEARmod v.0.6
rm(list=ls())
library(data.table) # fread - fastly reading data
library(lubridate)

setwd("~/Dropbox/COVID-19/SEIR_Model/Bearmodel_new")
source("bearmod_helper.R")


# Establece fechas de la simulacion
start_date = ymd("2020-03-01")
end_date = ymd("2020-04-30")
input_dates = seq(from=start_date,to=end_date,by="days") 

# Parametros del modelo
params = list()
params$prop_reported = 0.08392  # Infecciosos Reportados/(Infecciosos Reportados + Infecciosos no reportados)
params$exposepd = 5.5 # incubation period (in days) (1/exposepd = prob. of exp -> inf transition) [1,14]
params$recrate = 1/7.5 # daily probability of recovery (prob. of inf -> rec transition) gamma: [1/14,1/3]
params$exposed_pop_inf_prop = 1/5  # daily infect probability of exposed population:  sigma: [1/14,1/3]
#params$exposerate = 2.68/6 # R0 of 2.68, 5.8 days till seeking treatment
# How many people a single person potentially infects per day -- can be calculated from R0 estimate if you divide R0 by infectious period
#params$exposerate = data.frame(date=1, exposerate=exposerate)
#params$exposerate = fillDates(input_dates, params$exposerate)

milestones = data.frame()
milestones = rbind(milestones, data.frame(date=ymd("2020-02-01"), desc="Normal", exposerate=0.75))
milestones = rbind(milestones, data.frame(date=ymd("2020-03-09"), desc="Social distancing", exposerate=0.625))
milestones = rbind(milestones, data.frame(date=ymd("2020-03-13"), desc="School closure", exposerate=0.583))
milestones = rbind(milestones, data.frame(date=ymd("2020-03-14"), desc="Lockdown", exposerate=0.15))
milestones = rbind(milestones, data.frame(date=ymd("2020-03-28"), desc="Complete lockdown", exposerate=0.06))
milestones = rbind(milestones, data.frame(date=ymd("2020-04-11"), desc="Total isolation", exposerate=0.06))
milestones = rbind(milestones, data.frame(date=ymd("2020-04-14"), desc="Lockdown", exposerate=0.15))
params$exposerate = fillDates(input_dates, milestones)


# Tabla de regiones (patches) por provincias (_prov) y de toda la comunidad (_cv)
pat_locator_prov = read.table("patches_cv.csv",sep=",",header=T)
pat_locator_prov$nInf = c(124, 758, 479) 
pat_locator_prov$nExp = c(124, 758, 479) * 1.3

pat_locator_cv = data.frame(patIDs="1", patNames="CV", pop=4963703, nInf=15, nExp=15*2)

# Tabla de movilidad por provincias (_prov) y sin movilidad (_zero)
movement_data_prov = read.table("testmove_CV_normal.csv",sep=",",header=T)
movement_data_prov = fillDates(input_dates, movement_data_prov)

movement_data_zero = data.frame(date=NA, fr_pat=NA, to_pat=NA, fr_users=NA, movers=NA)

#### Running the model  ####
results = runModel(pat_locator_cv, movement_data_zero, input_dates, params, 
                   num_runs = 10)
#results = runModel(pat_locator_cv, movement_data_zero, input_dates, params)

real_data = read.table("cv_covid19.csv",sep=",",header=T)
real_data$removed = real_data$deaths + real_data$recovered
real_data$inf = real_data$cases - real_data$removed

# Plot 
#plotAllPatches(results$all_spread, patNames)
plotAllPatches(results$all_spread, patNames, col="inf", milestones=milestones, real_data=real_data[61:nrow(real_data),])
#plotCurves(results$all_spread, patNames)
