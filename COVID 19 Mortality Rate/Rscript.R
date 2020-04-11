library(utils)
library(httr)
######################
#download the dataset from the MOMO website to a local temporary file
GET("https://momo.isciii.es/public/momo/data", 
    authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf)
############################################################
#data <- read.csv("~/Dropbox/COVID-19/References/FluMOMO_version_4_2_R/data.csv")
##########################################
# Parámetros del script
##########################################
fecha_inicio_1 <- as.Date("2020-01-10")
fecha_final_1 <- as.Date("2020-04-06")
fecha_inicio_0 <- as.Date("2019-01-10")
fecha_final_0 <- as.Date("2019-04-07") #es año bisiesto y hay 29 de Febrero
##########################################
data$fecha_defuncion <- as.Date(data$fecha_defuncion)
data <- data.frame(data$nombre_ambito,data$nombre_sexo,data$nombre_gedad,data[9:15])
variable <- colnames(data)
dataCV <-data[data$data.nombre_ambito=="Comunitat Valenciana" & data$data.nombre_sexo=="todos" & data$data.nombre_gedad=="todos",]
dataCV <- na.exclude(dataCV)
#########################################
par(mfrow=c(3,1))
#########################################
plot(dataCV$defunciones_observadas~as.Date(dataCV$fecha_defuncion,"%y/%m/%d"),type='l'
     ,ylab="Defunciones Observadas", xlab="Date",col="blue",main="Comunidad Valenciana")
lines(dataCV$defunciones_esperadas~as.Date(dataCV$fecha_defuncion,"%y/%m/%d")
      ,type='l',col="red",lwd=2)
legend("topleft", legend=c("Reported", "Mean"), 
       lty=c(1,1), col=c("blue", "red"))
##########################################
dataCV1 <- dataCV[dataCV$fecha_defuncion>=fecha_inicio_1 & dataCV$fecha_defuncion <= fecha_final_1,]
dataCV0 <- dataCV[dataCV$fecha_defuncion>=fecha_inicio_0 & dataCV$fecha_defuncion <= fecha_final_0,]
plot(dataCV1$defunciones_observadas~as.Date(dataCV1$fecha_defuncion,"%y/%m/%d"),type='b'
     ,ylab="Defunciones Observadas", xlab="Date",col="blue",lwd=2,ylim=c(100,220))
lines(dataCV0$defunciones_observadas~as.Date(dataCV1$fecha_defuncion,"%y/%m/%d")
      ,type='b',col="red",lwd=2)
legend("topleft", legend=c("2020", "2019"), 
       lty=c(1,1), col=c("blue", "red"))
#############################################
diferences <- (dataCV1$defunciones_observadas - dataCV0$defunciones_observadas)
mu <- rep(mean(diferences),length(diferences))
two_sigma <- rep(1.5*sd(diferences),length(diferences))
plot(diferences~as.Date(dataCV1$fecha_defuncion),type="b",col="blue",xlab = "Day",ylab="Defunciones 2020 - Defunciones 2019",
     main="Diferencias entre fallecidos \n 2020 - 2019 en la Comunidad Valenciana",lwd=2)
lines(x~as.Date(dataCV1$fecha_defuncion),type='l',col="red",lwd=2)
lines(two_sigma~as.Date(dataCV1$fecha_defuncion),type='l',col="green",lwd=1)
lines(-two_sigma~as.Date(dataCV1$fecha_defuncion),type='l',col="green",lwd=1)
############################################



