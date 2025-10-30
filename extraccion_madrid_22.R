


# DOCUMENTO NUEVO PARA LA EXTRACCIÓN DE LA INFORMACIÓN DE LA MATRIZ DE MADRID. 

install.packages("readxl")
install.packages("tidyverse")
install.packages("jsonlite")
install.packages("geosphere")
install.packages("purrr")

library(purrr)
library(geosphere)
library(jsonlite)
library(tidyverse)
library(dplyr)
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




########## AHORA EMPIEZA LO QUE REALMENTE QUEREMOS HACER ##########








