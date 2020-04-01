
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
