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

## Para borrar seccióndes del enviroment hay que poner en la consola rm(lo que quieras borrar)

# 1º: VAMOS A ABRIR LA MATRIZ Y VER UN POCO EN QUE ESTADO NATURAL ESTÁ:

macro_madrid_22 <- read.csv2("C:/Users/Pablo/Documents/R/extraccion_madrid_22/macro_madrid_22.csv")
# Hay que usar read.csv2 para que asuma que los separadores son puntos y comas y lea
#bien el documento, si no hace cosas raras. 

#Si yo modifico algo en este documento ya se me va a actualizar también en el otro lado y viceversa?







              

