
## EXTRACCIÓN DE LA INFORMACIÓN DE LA MATRIZ DE MADRID Y FUSIÓN CON LA NACIONAL ##

install.packages("readxl")
install.packages("tidyverse")
install.packages("jsonlite")
install.packages("geosphere")
install.packages("purrr")

library(purrr)
library(geosphere)
library(jsonlite)
library(tidyverse)
library(readxl)


########## PREPARACIÓN DE LAS MATRICES ##########


# Para borrar seccióndes del enviroment hay que poner en la consola rm(lo que quieras borrar)

# 1º: VAMOS A ABRIR LA MATRIZ DE MADRID Y VER UN POCO EN QUE ESTADO NATURAL ESTÁ:

macro_madrid_22 <- read.csv2("C:/Users/Pablo/Documents/R/extraccion_madrid_22/macro_madrid_22.csv")
# Hay que usar read.csv2 para que asuma que los separadores son puntos y comas y lea
#bien el documento, si no hace cosas raras. 




## 2º: VAMOS A ABRIR LA MATRIZ DE LA PEÍNSULA: Esta matriz va a estar muy verde. 

macro_peninsula_22_sucio <- read.csv2("C:/Users/Pablo/Documents/R/extraccion_madrid_22/macro_peninsula_22.csv")


## De momento vamos a trabajar con la matriz de la península. 
## En primer lugar vamos a quitarle las columnas que no nos interesen y dejar solo la ID y los macros. 

macro_peninsula_mangueo_22 <- macro_peninsula_22_sucio[, -c(2:9)]


## Vamos a pasar a hacer una matriz con lagunas en vez de con magueos, para esto en primer lugar
## hay que rellenar los huecos vacíos debajo de cada MU1, MU2, MU3 etc. para poder decirle luego que 
## quiero la suma de estas filas para hacer la tabla por lagunas y no por mangueos. 

## Instalamos el paquete tidyr. Este nos permite usal la función fill () con la que vamos a poder 
## rellenar los huecos. 

install.packages("tidyr")
library(tidyr)

## Primero haya que convertir los huecos en NA, que en realidad sean celdas vacías. 

macro_peninsula_mangueo_NA_22$id[macro_peninsula_mangueo_22$id == ""] <- NA

## Vamos a rellenar los huevos vacíos.

macro_peninsula_mangueo_relleno_22 <- macro_peninsula_mangueo_NA_22 %>% fill(id, .direction = "down")

## Ahora vamos a sumar todas las filas con el mismo valor (MU1, MU2, etc.) para conseguir una matriz por
## laguna en vez de por mangueo. 
## Para ello necesitamos el paquete dplyr().

install.packages("dplyr")
library(dplyr)              
## Para poder hacer el sumatorio hay voy a convertir las columnas de bichos en numéricas. 

macro_peninsula_mangueo_relleno_22 <- macro_peninsula_mangueo_relleno_22 %>%
  mutate(across(-1, as.numeric))

## Ahora procedemos a hacer el sumatorio.

macro_peninsula_laguna_22 <- macro_peninsula_mangueo_relleno_22 %>%
  group_by(id) %>% #Agrupa todas las filas por el mismo nombre de la laguna. 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) # Hace sumatorio de todo lo que es numérico (las especies).

## Voy a extraer esta matriz para comprobar que esté bien. 

write.csv(macro_peninsula_laguna_22, "C:/Users/Pablo/Documents/R/extraccion_madrid_22/macro_peninsula_laguna_22.csv", row.names = TRUE)

### La parte facil está, ahora necesito quitar de la matriz de la península el _22 de todos los encabezados de las columnas. 

colnames(macro_peninsula_laguna_22) <- gsub("_22$", "", colnames(macro_peninsula_laguna_22)) # La función gsub() identifica todo lo que acabe en _22 y le he pedido que se lo quite, de tal manera que lo que no termina en esto no lo toca. 

# Volvemos al punto 1, vamos a pedir que me diga cuantos matches hay entre las columnas del macro de Madrid y del macro de la península. 
# Para ello hay que hacer lo mismo con la matriz de Madrid que con la de la península (pensaba que ya estaba por laguna y me equivocaba).

# 1º: Elimino las columnas que me sobran:

macro_madrid <- macro_madrid_22[, -c(2:6)]

# 2º: Convertimos los huecos en NA:

macro_madrid$Codigo[macro_madrid$Codigo == ""] <- NA

# 3º: Añadimos el nombre de cada laguna a su fila aunque estuviera vacía por no ser el primer mangueo. 

macro_madrid_relleno <- macro_madrid %>% fill(Codigo, .direction = "down")

# 4º: Hacemos el sumatorio. 1º convertimos a numéricas y segundo los sumamos. 

macro_madrid_relleno <- macro_madrid_relleno %>%
  mutate(across(-1, as.numeric))

macro_madrid_limpio_22 <- macro_madrid_relleno %>%
  group_by(Codigo) %>% #Agrupa todas las filas por el mismo nombre de la laguna. 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) # Hace sumatorio de todo lo que es numérico (las especies).




########## VER NOMBRES DE ESPECIES COMUNES ##########

# En primer lugar vamos a intentar hacer un match entre las dos matrices para saber cuantos nombres de 
# columnas hay en cada sitio y cuantas coinciden y cuantas no. 

# 1º: Vamos a extraer los nombres de las especies de las columnas y van a aparecer como Values en la pestaña de Enviroment. 

macro_peninsula_limpio <- macro_peninsula_laguna_22 # Hago esto por si se lía seguir teniendo la Data de macro_peninsula_laguna_22. 
macro_madrid_limpio <- macro_madrid_limpio_22 # Lo mismo que la fila de arriba pero para Madrid. 

cols_peninsula <- colnames(macro_peninsula_limpio)[!colnames(macro_peninsula_limpio) %in% c("id")]
cols_madrid <- colnames(macro_madrid_limpio)[!colnames(macro_madrid_limpio) %in% c("Codigo")]

# 2º: Vamos a comprobar que columnas hay en común:

comunes <- intersect(cols_peninsula, cols_madrid) # Salen 44 comunes. 

# Me parecen muy pocos comunes, voy a intentar hacer una aproximación por similitudes. 
# Para ello vamos a usar el paquete stringdist(calcula qué tan parecidas son dos cadenas de texto, midiendo la “distancia” entre ellas (por ejemplo, cuántas letras hay que cambiar para que una se convierta en la otra).).

install.packages("stringdist")

library(stringdist) #Da la impresión de que este paquete entra dentro del de tidyr, así que igual ni hacía falta. 

similares <- stringdist::amatch(cols_madrid, cols_peninsula, maxDist = 2)
similar_pairs <- data.frame(
  data_madrid = cols_madrid,
  posible_match_peninsula = cols_peninsula[similares]
)
head(similar_pairs)

# Esto es un poco raro, voy a extraerlo a excell directamente para verlo mejor: Voy a usar un paquete para eso. 

install.packages("openxlsx")

library(openxlsx)

write.xlsx(similar_pairs, "C:/Users/Pablo/Documents/R/extraccion_madrid_22/especies_similares_M_P_22.xlsx", rowNames = FALSE)

########## FUSIÓN DE LAS MATRICES #####

# Lo primero que vamos a hacer es llamar de la misma manera a las columnas de id. 

install.packages("dplyr")

library(dplyr)

macro_madrid_limpio <- macro_madrid_limpio %>% # De esta forma llamo id en las dos matrices a la primera columna. 
  rename(id = Codigo)

# Para fusionar vamos a usar la función bind_rows().

macro_22 <- bind_rows(macro_madrid_limpio, macro_peninsula_limpio)

# Sale increible, lo único que los valores que no comparten una u otra matriz quedan como N/A y prefiero que sean 0. 
# Vamos a convertirlos en 0. 

macro_22[is.na(macro_22)] <- 0

# Vamos a exportar la matriz a excell para tener la fusión en bruto. 

write.xlsx(macro_22, "C:/Users/Pablo/Documents/R/extraccion_madrid_22/macro_22_bruto.xlsx", rowNames = FALSE)

# Vamos a intentar eliminar las columnas cuyo valor sume 0 (columnas vacías que no nos sirven de nada).

macro_22_limpia <- macro_22[, c("id", names(macro_22[,-1])[colSums(macro_22[,-1], na.rm = TRUE) != 0])]

                  # macro_22[,-1] → selecciona todas las columnas excepto la primera.
                  # colSums(..., na.rm = TRUE) → calcula la suma de cada columna ignorando NA.
                  # != 0 → filtra solo las columnas que no están vacías.
                  # names(...) → obtiene los nombres de esas columnas.
                  # c("id", ...) → mantiene la primera columna id.
                  # macro_22[, ...] → selecciona finalmente las columnas útiles en tu data frame.


# Vamos a exportar la matriz final:

write.xlsx(macro_22_limpia, "C:/Users/Pablo/Documents/R/extraccion_madrid_22/macro_22_limpio.xlsx", rowNames = FALSE)



