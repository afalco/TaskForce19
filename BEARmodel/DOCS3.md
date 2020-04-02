
# Inicialización de la población

```R
HPop = InitiatePop(pat_locator,
                   patnInf,
                   patnExp)
```

Inicialización de la población. Toma como parámetro una tabla con todas las regiones (_patches_), y el número inicial de infectados y expuestos por región.

## Input

* `pat_locator` : Tabla de regiones (_patches_). Cada fila representa una región, de de cada una se incluye su identificador (columna `patIDs`), su nombre (columna `patNames`) y su población (columna `pop`).

    ```
      patNames patIDs pop
    1        1      1 100
    2        2      2 100
    3        3      3 100
    4        4      4 100
    5        5      5 100
    ```

* `patnInf` : Tupla de **infectados** iniciales por región. Cada elemento de la tupla representa una región.

    ```
    50  0  0  0  0
    ```

* `patnExp` : Tupla de **expuestos** iniciales por región. Cada elemento de la tupla representa una región.

    ```
    0 0 0 0 0
    ```

## Output

Como salida produce la población inicial, con las siguientes entradas:

```
$nInitialInf
[1] 50  0  0  0  0

$nInitialExp
[1] 0 0 0 0 0

$nInf
[1] 50  0  0  0  0

$nExp
[1] 0 0 0 0 0

$nRec
[1] 0 0 0 0 0

$nTotal
[1] 100 100 100 100 100

$names
[1] 1 2 3 4 5

$IDs
[1] 1 2 3 4 5

$relativeInf
[1] 1 1 1 1 1

$nRecoveredToday
[1] 0 0 0 0 0

$nInfectedToday
[1] 0 0 0 0 0

$nExposedToday
[1] 0 0 0 0 0

$nInfMovedToday
[1] 0 0 0 0 0

$controlled
[1] 0 0 0 0 0
```


# Ejecución de la simulación

```R
HPop_update = runSim(HPop, 
                     pat_locator, 
                     relative_move_data, 
                     movement_data, 
                     input_dates, 
                     recover_df, 
                     exposerate,
                     exposepd,
                     exposed_pop_inf_prop = 0, 
                     TSinday = 1,
                     prob_move_per_TS=0)
```

Ejecuta una simulación a partir de la población inicial, la tabla de movilidad y las tasas de recuperación y exposición. 


## Input


* `HPop` : Población inicial.

* `pat_locator`: Tabla de regiones (_patches_)

    ```
      patNames patIDs pop
    1        1      1 100
    2        2      2 100
    3        3      3 100
    4        4      4 100
    5        5      5 100
    ```

* `relative_move_data` : Permite reducir una fracción de la población que se mueve, respecto a los valores absolutos de la siguiente tabla. Se indica región de origen (`from`), región destino (`to`), y el factor por el que se multiplicará la movilidad entre dichas regiones (`relative_move`),

    ```
            date from to relative_move
    1 2020-05-01    1  2             0
    2 2020-05-01    1  3             0
    3 2020-05-01    1  4             0
    4 2020-05-01    1  5             0
    5 2020-05-01    5  1             0
    ```


* `movement_data` : Tabla de movilidad. Se incluyen los movimiento que se aplicarán en cada fecha. Se indica región de origen (`fr_pat`), región destino (`to_pat`), personas que se desplazan (`movers`), población en el origen (`fr_users`) y en el destino (`to_users`).

    ```
            date from to movers fr_pat to_pat fr_users to_users
    1 2020-05-01    1  2     10      1      2      100      100
    2 2020-05-01    1  3      5      1      3      100      100
    3 2020-05-01    1  4      4      1      4      100      100
    4 2020-05-01    1  5      3      1      5      100      100
    5 2020-05-01    5  1      8      5      1      100      100
    ```

* `input_dates` : Lista de fechas en las que se realizará la simulación. 

    ```
    2020-05-01  2020-05-02  2020-05-03  2020-05-04  2020-05-05    2020-05-06  2020-05-07  2020-05-08  2020-05-09  2020-05-10 
    ```

* `recover_df`: _Data frame_ con tasa de recuperación por fecha (1.0 / periodo de recuperación)

    ```
            date   recrate
    1  2020-05-01 0.1666667
    2  2020-05-02 0.1666667
    3  2020-05-03 0.1666667
    4  2020-05-04 0.1666667
    5  2020-05-05 0.1666667
    6  2020-05-06 0.1666667
    7  2020-05-07 0.1666667
    8  2020-05-08 0.1666667
    9  2020-05-09 0.1666667
    10 2020-05-10 0.1666667
    ```

* `exposerate` : Se puede especificar un valor único, para todas las fechas, o bien un _data frame_ con diferentes tasas para cada fecha. Se calcula como (R0 / periodo de recuperación).

    ```
	0.4466667
    ```

* `exposepd`: Periodo de exposición. La tasa diaria de conversión de exposición a infectado se calcula como (1.0 / exposepd)

    ```
	3
    ```

* `exposed_pop_inf_prop = 0` : Permite indicar un porcentaje de individuos expuestos que se consideraría población infecciosa, junto a los infectados. 

* `TSinday = 1` : _Time Steps_ de ejecución para cada día. Por defecto 1. Permite realizar varios pasos de simulación para cada fecha.

* `prob_move_per_TS=0` : Se ignora si es `0` . En caso contrario, se aplica como factor multiplicativo a la matriz de movilidad, indicando la probabilidad de movimiento por cada _time step_. 

## Output

Genera los resultados de la simulación con las siguientes entradas:

* `epidemic_curve`: Tabla con el número de infectados por día.

    ```
            Date inf
    1  2020-05-01  44
    2  2020-05-02  48
    3  2020-05-03  48
    4  2020-05-04  43
    5  2020-05-05  52
    6  2020-05-06  58
    7  2020-05-07  66
    8  2020-05-08  77
    9  2020-05-09  86
    10 2020-05-10  96
    ```


* `all_spread`: Número de infectados por region (_patch_) y día.

    ```
            dates runday  1  2  3  4  5
    1  2020-05-01      1 36  2  2  4  0
    2  2020-05-02      2 32  6  4  6  0
    3  2020-05-03      3 22 10  4 10  2
    4  2020-05-04      4 13 13  4 10  3
    5  2020-05-05      5 14 14  8 13  3
    6  2020-05-06      6 10 18  9 17  4
    7  2020-05-07      7 10 18 14 20  4
    8  2020-05-08      8 13 18 20 22  4
    9  2020-05-09      9 14 24 18 25  5
    10 2020-05-10     10 13 25 21 29  8
    ```


# Pasos de la simulación

## Movimiento

```R
HPop = movementTimeStep(HPop,
                        mobmat,
                        day,
                        control_df,
                        prob_move_per_TS)
```

A partir de la tabla de movilidad, calcula una matriz de probabilidad `P` de ir del patch `i` al `j`:

```
no_mov(1,1)/poblacion(1)  movers(1,2)/poblacion(1)  movers(1,3)/poblacion(1)
movers(2,1)/poblacion(2)  no_mov(2,2)/poblacion(2)  movers(2,3)/poblacion(2)
movers(3,1)/poblacion(3)  movers(3,2)/poblacion(3)  no_mov(3,3)/poblacion(3)
```

Los elementos de la diagonal de esta matriz nos indican la probabilidad de que no se muevan a otra región. Por ejemplo podemos tener la siguiente matriz de probabilidad de transición `P`:

```
1 0.9714295644 0.026428087 0.002142348
2 0.0071428375 0.985714325 0.007142837
3 0.0007141558 0.006428185 0.992857659
```

Para estimar el número de personas que se mueven realmente en cada posible transición `(i, j)`, genera valores aleatorios a partir de una distribución binomial con los siguientes parámetros:

* `prob` : Valor de la celda `(i, j)` de la matriz de probabilidad anterior (`P(i, j)`).
* `size` : Número actual de infectados del _patch_ `i` de origen.  

Con ello se obtiene una matriz de número de personas que se mueven de una región a otra. Por ejemplo, consideremos que tenemos una matriz de probabilidad `P` como la del ejemplo anterior, y el siguiente número de infectados por región

```
1435 4519 1588
```

A partir de la binomial se podrían generar unos valores de movimiento como los siguientes:

```
     1    2    3
1 1395   42    6
2   28 4452   33
3    1   10 1581
```

Al ser valores generados aleatoriamente, es posible que no sumen la población de cada región. Por ello, se normaliza cada fila y se multiplica por el número de infectados en cada región, obteniendo así el número de gente que se mueve a cada región.

La suma de las columnas de la matriz anterior será el número número de infectados que habrá en cada una de las regiones.




### Input

* `HPop` : Población actual.
* `mobmat` : Matriz de movilidad (_data frame_).
* `day` : Fecha del día a simular.
* `control_info` : _Data frame_ de control de movilidad.
* `prob_move_per_TS` : Probabilidad de movimiento en cada _time step_.


### Código comentado

```R
movementTimeStep = function(HPop, mobmat,day,control_df,prob_move_per_TS){
  
  # Crea una matrix de nPatches x nPatches
  movement_matrix = matrix(0,NPat,NPat,dimnames=list(patIDs,patIDs))
  # Obtiene el subconjunto de movimientos del dia actual
  daily_move = subset(mobmat,date == day)
  # Elimina filas con valores vacíos
  daily_move = subset(daily_move,!is.na(fr_pat) & !is.na(to_pat) & !is.na(fr_users) & !is.na(movers))
  # Filtra solo las columnas que nos interesan
  daily_move_mat = daily_move[,is.element(names(daily_move),c("fr_pat","to_pat","fr_users","movers"))]
  daily_move_mat = as.matrix(daily_move_mat)
  
  # Obtiene los indices de las diferentes columnas
  col1 = which(colnames(daily_move_mat) == "fr_pat")
  col2=which(colnames(daily_move_mat) == "to_pat")
  colmove = which(colnames(daily_move_mat) == "movers")
  colusers=which(colnames(daily_move_mat) == "fr_users")

  # Matriz de movimientos relativos. Cada elemento (i,j) representa que proporcion de usuarios de i se mueven a j
  movement_matrix[daily_move_mat[,c(col1,col2)]] = daily_move_mat[,colmove]/daily_move_mat[,colusers]
  # Se comprueba que ninguna fila sume más de 1. Si esto ocurriese, se corrige normalizando
  if (length(which(rowSums(movement_matrix)>1)) > 0){
    print("Warning: row sums > 1 in movement matrix. Correcting...")
    correctingrows = which(rowSums(movement_matrix)>1)
    for (i in correctingrows){
    movement_matrix[i,] = movement_matrix[i,] /sum(movement_matrix[i,] )
    }
  }
  # En caso de indicar probabilidad de movimiento por time step, se multiplica la matriz de movimiento por este factor
  if (prob_move_per_TS > 0){
    movement_matrix = movement_matrix*prob_move_per_TS
  }
  # En la diagonal se pone 1 menos la suma del resto de la fila
  for (i in 1:length(patIDs)){
    movement_matrix[i,i] = 1 - sum(movement_matrix[i,-i])
  }
  
  # Crea un vector de poblacion con movilidad controlada
  HPop$controlled = rep(0,length(HPop$names))
  # Busca en la tabla de control de movilidad (si la hubiese) las entradas de la fecha actual
  if (length(which(control_df$date == day)) > 0){
    # Filtra las filas de la tabla correspondientes al dia actual
    control_df_sub = subset(control_df,date == day)
    # Comprueba si el filtrado ha devuelto filas
    if (dim(control_df_sub)[1] > 0){
    # Pone en cada entrada (patch) del vector de movilidad controlada el ratio a aplicar
    for (i in 1:dim(control_df_sub)[1]){
      HPop$controlled[which(HPop$names == control_df_sub$from[i])] = control_df_sub$relative_move[i]
      
    }
  }
  }
  # En caso de haber control de movilidad, se aplica la reduccion de movilidad con stopMovement
  if (sum(HPop$controlled)>0){
    movement_matrix = stopMovement(HPop,movement_matrix,day)
  }
  #deterministic version
  #HPop$nInfMovedToday = colSums(diag(HPop$nInf) %*% movement_matrix) - HPop$nInf
  #HPop$nInf = colSums(diag(HPop$nInf) %*% movement_matrix)
  HPop$nInf = ceiling(HPop$nInf)
  # stochastic version

  # Para cada entrada (i,j) de la matriz de movilidad:
  #     - movement_matrix(i,j) representa la probabilidad p de que un invididuo de la region i se mueve a j
  #     - Se toma el número de infectados de la region i como numero de ensayos de la distribucion binomial
  #     - Se genera un valor aleatorio a partir de dicha distribucion para cada entrada, que representa los infectados que se han movido de i a j (incluyendo los que se quedan en la misma región)
    z <- rbinom(n=NPat^2,size = rep(HPop$nInf,each=NPat),prob = t(movement_matrix)[])
  moved_matrix = t(matrix(z,NPat,NPat,dimnames=list(patIDs,patIDs)))
  # Se normaliza cada fila de la matriz de movimientos y se multiplican por el número de infectados en i para asegurarnos que la suma de cada fila sea igual al número de infectados en cada región
  for (i in 1:dim(moved_matrix)[1]){
     if (sum(moved_matrix[i,]) > 0){
     moved_matrix[i,] = moved_matrix[i,]/sum(moved_matrix[i,]) * HPop$nInf[i]
     }
  }
  # Se suman las columnas de la matrix (nuevo número de infectados en cada region j)
  HPop$nInfMovedToday = ceiling(colSums(moved_matrix))
  
  # A la población infectada de cada patch, se le restan los que salen y se suman los que entran
  HPop$nInf = HPop$nInf - floor(rowSums(moved_matrix)) + ceiling(colSums(moved_matrix))
  #quick fix

  # Evita que el numero de infectados de una region pueda ser superior al numero total de habitantes
  for (i in 1:length(HPop$nInf)){
    if (HPop$nInf[i] > HPop$nTotal[i]){
      HPop$nInf[i] = HPop$nTotal[i]
    }
  }
  
  #print(paste0("Number of infected people moving: ",sum(abs(HPop$nInfMovedToday))/2))
  HPop
}


###### Response functions: Control
#relative_movement is the proportion of original movement out/in that we want to keep -- ie. .1 = 10% of original movement rate
stopMovement = function(HPop,mobmat,current_date){
  stopping = which(HPop$controlled > 0)
    if (length(stopping) > 0){
     # print(paste("stopping movement in patches", HPop$names[stopping]))
      for (ctrl_pat in stopping){
    control_patches = HPop$IDs[ctrl_pat]
    # Para cada patch con movilidad controlada, se aplica el factor de control en toda la fila y columna de la matriz de movilidad correspondiente al patch
    mobmat[control_patches,] = mobmat[control_patches,] * HPop$controlled[ctrl_pat]
    mobmat[,control_patches] = mobmat[,control_patches] * HPop$controlled[ctrl_pat]
    # Recalcula la diagonal
    for (i in 1:length(HPop$IDs)){
      mobmat[i,i] = 1 - sum(mobmat[i,-i])
    }
      }
    }
    mobmat
}
```