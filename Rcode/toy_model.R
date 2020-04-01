#Script for downloading the CSV file into “R” software
#these libraries need to be loaded
library(utils)
library(httr)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
	authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf)
############################################################
covid19_world_data <- read.csv("~/Dropbox/COVID-19/SEIR_Model/Rcode/covid19_world_data.csv")
covid19_world_data$date <- as.Date(covid19_world_data$date)
covid19 <-data.frame(covid19_world_data$date,covid19_world_data$new_cases,
                     covid19_world_data$new_deaths,covid19_world_data$total_cases,covid19_world_data$total_deaths)
plot(covid19$covid19_world_data.total_cases~as.Date(covid19$covid19_world_data.date,"%y/%m/%d"),
     type="l",col="blue",ylab="Frequency", xlab="Date")
lines(covid19$covid19_world_data.total_deaths~as.Date(covid19$covid19_world_data.date,"%y/%m/%d"),
      type='l',col="red")
legend("topleft", legend=c("Total Cases", "Total Deaths"), 
       lty=c(1,1), col=c("blue", "red"))
########Ratio Death/Infected Dynamics
death_ratio <- covid19$covid19_world_data.total_deaths/covid19$covid19_world_data.total_cases
plot(death_ratio~as.Date(covid19$covid19_world_data.date,"%y/%m/%d"),
      type='l',col="blue",ylab="Relative Frequency", xlab="Date")
legend("topleft", legend=c("Ratio Deaths/Infected"), 
       lty=c(1), col=c("blue"))