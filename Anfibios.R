 
 ## MATRIZ ANFIBIOS CLIMARISKINPOND ##



# Voy simplemente a ordenar por lagunas los anfibios del proyecto para poder hacer una tabla dinámica en Excell. 

# 1º: Abrimos el documento: 

Anfibios <- read.csv2("C:/Users/Pablo/Documents/R/extraccion_madrid_22/Anfibios/Anfibios.csv")

# 2º: Vamos a eliminar las columnas que no me interesan demasiado:

Anfibios_id <- Anfibios [, -c(2:6)]

# 3º: Vamos a convertir los huecos vacíos en NA. 

install.packages("tidyr") # Necesitamos la función fill() del paquete tidyr. 
library(tidyr)

Anfibios_id$id[Anfibios_id$id == ""] <- NA # Básicamente le estamos diciendo que en la columna id lo huevos vacíos ("") son NA. 

# 4º: Una vez tenemos esto, podemos rellenar los huevos vacíos. 

Anfibios_relleno <- Anfibios_id %>% fill(id, .direction = "down")

# Vamos a pasar a hacerlo por lagunas: 

install.packages("dplyr")
library(dplyr)     

Anfibios_numeric <- Anfibios_relleno %>% # Primero pasamos a hacer como numeric todas las columnas menos la primera. Esta nos serviría para trabajar con los datos por mangueo. 
    mutate(across(-1, as.numeric))

# Ahora procedemos a hacer el sumatorio.

Anfibios_laguna <- Anfibios_numeric %>%
  group_by(id) %>% #Agrupa todas las filas por el mismo nombre de la laguna. 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) # Hace sumatorio de todo lo que es numérico (las especies).

# Ya podemos extraer el documento. 

install.packages("openxlsx")

library(openxlsx)


write.xlsx(Anfibios_laguna, "C:/Users/Pablo/Documents/R/extraccion_madrid_22/Anfibios/Anfibios_laguna.xlsx", rowNames = FALSE)





