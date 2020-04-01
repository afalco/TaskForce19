# covid19model
Code for modelling estimated deaths and cases for COVID19. 

This repository has code for replication purposes. The bleeding edge code and advancements are done in a private repository. Ask report authors for any collaborations. 

## Comunitat Valenciana extension

El modelo original se basa en los datos actualizados de 11 países. A esos países originales hemos añadido ´Comunitat_Valenciana´

    countries <- c(
        "Denmark",
        "Italy",
        "Germany",
        "Spain",
        "United_Kingdom",
        "France",
        "Norway",
        "Belgium",
        "Austria", 
        "Sweden",
        "Switzerland",
        "Comunitat_Valenciana"
)

### Cambios con respecto al modelo original

Para que esto funcione hay que aportar los datos actualizados de la CV. El software original incluye el script `fetch-ecdc.R`, que se encarga de descargar los datos actualizados para todos los países. Los datos se limitan a número oficial de casos y número de muertes. Para añadir los datos de la CV hemos incluido un script en *Python* (`covid19_cv.py`) que descarga los datos actualizados desde el github de [Datadista](https://github.com/datadista/datasets), y hemos modificado el script original `fetch-ecdc.R` para que ejecute este script y actualice los datos. Por lo tanto, el procedimiento no cambia, para actualizar los datos es suficiente con ejecutar el script `fetch-ecdc.R`.

Además de esto, el modelo utiliza una serie de datos propios de cada país, como la distribución de población por edades, que se encuentra en los archivos `ages.csv`, `interventions.csv`, `weighted_fatality`. Estos archivos los hemos actualizado con la información de la CV obtenida de la web de GVA. También es necesario introducir las fechas en las que se llevaron a cabo intervenciones gubernamentales (*lockdown*, *schools closed*, etc). Para esto simplemente hemos duplicado los datos de España.

Además de todos estos cambios, hay partes del código, en los scripts `base.r`, `plot-forecast.r` y `plot-3-panel.r` que realizan iteraciones con un número de repeticiones igual al número de países original (11). De momento hemos cambiado los valores a 12, pero habría que ponerlos en función de la longitud de la lista de países.

### Funcionamiento

1. En primer lugar ejecutamos el script `fetch-ecdc.R` para actualizar los datos
2. Ejecutamos el script `base.r`. Este script ajusta el modelo predictivo (tarda unos pocos minutos), y al final genera una serie de gráficos, en formato `.pdf`, que se guardan en la carpeta `figures`. Los resultados obtenidos se guardan en varios ficheros `.Rdata` en la carpeta results.

