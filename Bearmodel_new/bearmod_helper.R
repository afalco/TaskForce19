library(lubridate)

source("bearmod_fx.R")

#' Devuelve los indices de la tabla de curvas por region
#' 
#' @param patNames Lista con los nombres de las regiones (patches)
#' @return Lista con todos los indices de datos que encontraremos en la tabla all_spread que devuelve la simulacion
#' 
getIndices = function(patNames) {
  paste0(rep(c("inf_", "rec_", "exp_", "sus_", "inf_day_", "rec_day_"), each=length(patNames)), patNames)
}

#' Ejecuta varias simulaciones del modelo y obtiene las curvas promedio de cada región y totales
#' 
#' @param pat_locator
#' @param movement_date
#' @param input_dates
#' @param params
#' @param num_runs
#' @return Devuelve una lista con dos objetos: 
#'     \itemize{
#'         \item `epidemic_curve`. Incluye una columna \code{inf} con el total de infectados por fecha. 
#'         \item `all_spread`. Para cada \em{patch}, se incluyen las siguientes columnas:
#'             \itemize{
#'                 \item \code{inf_<patch_name>} Infectados activos
#'                 \item \code{rec_<patch_name>} Recuperados hasta la fecha  
#'                 \item \code{exp_<patch_name>} Expuestos activos
#'                 \item \code{sus_<patch_name>} Susceptibles
#'                 \item \code{inf_day_<patch_name>} Nuevos infectados en cada fecha
#'                 \item \code{rec_day_<patch_name>} Nuevos recuperados en cada fecha
#'                 \item \code{rep_day_<patch_name>} Nuevos reportados en cada fecha
#'                 \item \code{rec_total_<patch_name>} Recuperados totales hasta la fecha
#'                 \item \code{rep_total_<patch_name>} Reportados totales hasta la fecha
#'                 \item \code{inf_total_<patch_name>} Infectados totales hasta la fecha
#'             }
#'     }
#' 
#' 
runModel = function(pat_locator, movement_data, input_dates, params, num_runs = 10) {
  
  ## Lee los parametros
  exposepd <- params$exposepd;
  if (is.null(exposepd))  {
    exposepd <- 5.1
    print(paste0("Using default value for expose period: ", exposepd))
  }
  
  recover <- params$recrate;
  if (is.null(recover))  {
    recover <- 1/12
    print(paste0("Using default value for recover rate: ", recover))
  }
  if(is.numeric(recover)) {
    recover <- data.frame(date = input_dates,recrate = recover)
  } else {
    recover = fillDates(input_dates, recover)
  }
  
  exposerate <- params$exposerate;
  if (is.null(exposerate)) {
    exposerate <- 2.68/6
    print(paste0("Using default value for expose rate: ", exposerate))
  } else if(!is.numeric(exposerate)) {
    exposerate = fillDates(input_dates, exposerate)
  }

  prop_reported <- params$prop_reported;
  if (is.null(prop_reported)) {
    prop_reported <- 1
    print(paste0("Using default value for reported fraction: ", prop_reported))
  }
  if(is.numeric(prop_reported)) {
    prop_reported <- data.frame(date = input_dates,prop = prop_reported)
  } else {
    prop_reported = fillDates(input_dates, prop_reported)
  }

  exposed_pop_inf_prop <- params$exposed_pop_inf_prop
  if (is.null(exposed_pop_inf_prop)) {
    exposed_pop_inf_prop <- 0
    print(paste0("Using default value for exposed population infect prob.: ", exposed_pop_inf_prop))
  }

  ## Inicializa datos de patches a partir de la tabla de movilidad
  patNames <<- pat_locator$patNames  
  patIDs <<- pat_locator$patIDs
  NPat <<- length(patNames)
  
  patnInf <- pat_locator$nInf / prop_reported$prop[1]
  patnExp <- pat_locator$nExp / prop_reported$prop[1]
  initialRep = pat_locator$nInf
  initialInf = patnInf
    
  ## No se especifica reduccion de movilidad
  relative_move_data <- data.frame()
  
  ## Inicializa la población y realiza la primera simulacion
  HPop = InitiatePop(pat_locator,patnInf,patnExp)
  HPop_update2 = runSim(HPop,pat_locator,relative_move_data,movement_data,input_dates,recover,exposerate,exposepd,exposed_pop_inf_prop = exposed_pop_inf_prop, TSinday = 1)
  #print(paste0("Run # ",1))
  
  epidemic_curve = HPop_update2$epidemic_curve
  all_spread = HPop_update2$all_spread
  
  curve_indices = "inf"
  all_spread_indices = getIndices(patNames)
  
  ## Ejecuta N simulaciones
  for (run in 2:num_runs){
    HPop_update2 = runSim(HPop,pat_locator,relative_move_data,movement_data, input_dates,recover,exposerate,exposepd,exposed_pop_inf_prop = exposed_pop_inf_prop, TSinday = 1)
    #print(paste0("Run # ",run))
    
    epidemic_curve[, curve_indices] = epidemic_curve[, curve_indices] + HPop_update2$epidemic_curve[, curve_indices]
    all_spread[, all_spread_indices] = all_spread[, all_spread_indices] + HPop_update2$all_spread[, all_spread_indices]
  }

  ## Obtiene la media de todas las curvas y corrige con el factor de reportados
  epidemic_curve[, curve_indices] = epidemic_curve[, curve_indices] /num_runs
  all_spread[, all_spread_indices] = all_spread[, all_spread_indices] / num_runs

  ## Corrige la proporcion de infectados reportados cada dia
  current_inf_indices = paste0("inf_", patNames)
  current_rep_indices = paste0("rep_", patNames)
  daily_inf_indices = paste0("inf_day_", patNames)
  daily_rec_indices = paste0("rec_day_", patNames)
  daily_rep_indices = paste0("rep_day_", patNames)
  total_rec_indices = paste0("rec_total_", patNames)
  total_rep_indices = paste0("rep_total_", patNames)
  total_inf_indices = paste0("inf_total_", patNames)
  all_spread[, current_rep_indices] = all_spread[, current_inf_indices]* prop_reported$prop
  all_spread[, daily_rep_indices] = all_spread[, daily_inf_indices]* prop_reported$prop
  all_spread[, total_rep_indices] = all_spread[, daily_rep_indices]
  all_spread[, total_inf_indices] = all_spread[, daily_inf_indices]
  all_spread[, total_rec_indices] = all_spread[, daily_rec_indices]

  all_spread[1, total_inf_indices] = all_spread[1, total_inf_indices] + initialInf
  all_spread[1, total_rep_indices] = all_spread[1, total_rep_indices] + initialRep
  
  for (r in 2:nrow(all_spread)) {
    all_spread[r, total_rec_indices] = all_spread[r-1, total_rec_indices] + all_spread[r, total_rec_indices]
    all_spread[r, total_rep_indices] = all_spread[r-1, total_rep_indices] + all_spread[r, total_rep_indices]
    all_spread[r, total_inf_indices] = all_spread[r-1, total_inf_indices] + all_spread[r, total_inf_indices]
  }
  
  #save(results,file="results_ComVal_Mobility.RData")
  #   
  results = list()
  results$epidemic_curve = epidemic_curve
  results$all_spread = all_spread
  
  results
}


#' Dado un intervalo de fechas, rellena en una tabla todas las entradas correspondientes a los dias que
#' estan vacios, utilizando los datos del ultimo dia que tuviera algun valor.
#' 
#' @param input_dates Lista de fechas que deben figurar en la tabla
#' @param table Tabla de datos con una columna "date" que indica las fechas. Se pueden dar como datos de tipo
#' fecha, o como un valor numerico indicando el desplazamiento de dias respecto a la fecha inicial. En el ultimo caso,
#' el valor `1` corresponderia a la primera fecha de `input_dates`, y el valor `n` a `input_dates[n]`. 
#' @return Tabla `table` completada con entradas para todas las fechas de `input_dates`, y en la que los
#' valores numericos de la columna `date` se habran sustituido por las fechas de `input_dates`.
#' 
fillDates = function(input_dates, table) {
  num_dates = length(input_dates)
  result = data.frame()
  
  # Si se dan fecha absolutas, se transforman a dias de desplazamiento respecto a la fecha inicial
  if(is.Date(table$date)) {
    offset = table$date - input_dates[1] + 1
    table$date = as.numeric(offset)
  }
  
  # Como patron inicial toma la ultima fecha anterior a la primera fecha de input_dates, o la primera fecha
  # de la tabla si no hubiese ninguna anterior
  first_date = min(table$date)
  previous_rows = subset(table, date<=1)
  if(nrow(previous_rows) > 0) {
    first_date = max(previous_rows$date)
  }
  day_pattern = subset(table, date==first_date)
  
  for (day in 1:num_dates) {
    # Obtiene las entradas de la tabla original para la fecha "day"
    daily_entries = subset(table, date==day)
    if(nrow(daily_entries) > 0) {
      # Si existen datos en la tabla original para la fecha, los toma como patron
      day_pattern = daily_entries
    } else {
      # En caso contrario, utiliza el ultimo patron guardado actualizando su fecha
      day_pattern$date = day
    }
    result = rbind(result, day_pattern)
  }
  
  # Transforma la columna numérica de fecha a las fecha reales
  result$date = input_dates[1] + result$date - 1
  
  result
}


plotAllPatches = function(table, patNames, col="inf", milestones=NULL, real_data=NULL) {
  par(mfrow=c(1, 1))
  
  columns = paste0(col, "_", patNames)

  total =  table[, columns]
  if(length(columns) > 1) {
    total = rowSums(table[, columns])
  }
  plot(table$dates, total, type="l", col="black", ylim=c(0,max(total)+1000), xlab = "date", ylab = col)

  legend_title = "Total"
  legend_col = "black"
  legend_lty = 1
  
  if(!is.null(milestones)) {
    for(i in 1:nrow(milestones)) {
      abline(v=milestones$date[i], col='blue', lty=1)
    }
  }

  if(!is.null(real_data)) {
    lines(as.Date(real_data$dates), real_data[,col], type="l", col="black", lty=2)
    
    legend_title = c(legend_title, "Actual cases")
    legend_col = c(legend_col, "black")
    legend_lty = c(legend_lty, 2)
  }
  
  if(length(columns)>1) {
    cl <- rainbow(length(columns))
    for (i in 1:length(columns)) {
      lines(table$dates, table[, columns[i]], col=cl[i], type="l")
    }
    legend_title = c(legend_title,as.character(patNames))
    legend_col = c(legend_col, cl)
    legend_lty = c(legend_lty, rep(1, length(columns)))
  }
  
  legend("topleft", col=legend_col, legend=legend_title,  lty=legend_lty)
}

plotPatch = function(table, patName, cols=c("inf", "rec", "exp"), milestones=NULL, real_data=NULL, real_data_cols="cases") {
  par(mfrow=c(1, 1))
  
  columns = paste0(cols, "_", patName)
  
  ymax = max(max(table[,columns]))
  if(!is.null(real_data)) {
    ymax = max(ymax, max(real_data[, real_data_cols]))
  }
  
  plot(table$dates, rep(0, length(table$dates)), type="n", ylim=c(0,ymax), xlab = "date", ylab = "count")
  
  legend_title = c()
  legend_col = c()
  legend_lty = c()
  
  if(!is.null(milestones)) {
    for(i in 1:nrow(milestones)) {
      abline(v=milestones$date[i], col='blue', lty=1)
    }
  }

  rcl <- rainbow(length(real_data_cols))
  cl <- rainbow(length(cols))
  
  if(!is.null(real_data)) {
    for (i in 1:length(real_data_cols)) {
      lines(as.Date(real_data$dates), real_data[,real_data_cols[i]], type="l", col=rcl[i], lty=2)
    }
    
    legend_title = c(legend_title, real_data_cols)
    legend_col = c(legend_col, rcl)
    legend_lty = c(legend_lty, rep(2, length(real_data_cols)))
  }
  
  for (i in 1:length(columns)) {
    lines(table$dates, table[, columns[i]], col=cl[i], type="l")
  }
  legend_title = c(legend_title,as.character(cols))
  legend_col = c(legend_col, cl)
  legend_lty = c(legend_lty, rep(1, length(columns)))

  legend("topleft", col=legend_col, legend=legend_title,  lty=legend_lty)
}


plotCurves = function(table, patNames) {
  
  par(mfrow=c(2, 2))
  
  total_inf = rowSums(table[, paste0("inf_", patNames)])
  total_exp = rowSums(table[, paste0("exp_", patNames)])
  total_rec = rowSums(table[, paste0("rec_", patNames)])
  total_sus = rowSums(table[, paste0("sus_", patNames)])
  
  plot(table$dates, total_inf, col="red", type='l', 
       xlab="date", ylab="population", ylim = c(0, max(total_sus)), main="Comunidad Valenciana")
  #abline(v=c(which(social_distancing_date==input_dates)), col='blue', lty=1)
  lines(table$dates, total_exp, col="light blue", lty=1)
  lines(table$dates, total_rec, col="green", lty=2)
  lines(table$dates, total_sus, col="magenta", lty=3)
  legend("left", legend=c("infectious", "exposed", "recovered", "susceptible"), 
         col=c("blue", "red", "green", "magenta"), lty=c(1, 1, 2, 3), cex=0.7)
  
  
  for (patch in patNames) {
    ymax = max(table[,getIndices(patch)])
    plot(table$dates, table[,paste0("exp_", patch)], col="red", type='l', 
         xlab="date", ylab="population", ylim = c(0, ymax), main=patch)
    lines(table$dates, table[,paste0("exp_", patch)], col="light blue", lty=1)
    lines(table$dates, table[,paste0("rec_", patch)], col="green", lty=2)
    lines(table$dates, table[,paste0("sus_", patch)], col="magenta", lty=3)
    legend("left", legend=c("infectious", "exposed", "recovered", "susceptible"), 
           col=c("blue", "red", "green", "magenta"), lty=c(1, 1, 2, 3), cex=0.7)
  }
  
}
