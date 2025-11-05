    

    ## SCRIPT PARA MACROINVERTEBRADOS DEL 2021 ##


install.packages("tidyr") 
install.packages("openxlsx")
install.packages("dplyr")


# La matriz de macro del 21 en Doñana es parecida a la del nacional, voy a unirla por laguna y poco más. 

# 1º: Vamos a ver en que estado está la matriz:

Macro_21 <- read.csv2("C:/Users/Pablo/Documents/R/extraccion_madrid_22/Macro_21/Macro_21.csv")

# 2º: Voy a rellenar todos los huecos vacíos con NA en la matriz. 

library(tidyr)

Macro_21 [Macro_21 == ""] <- NA # Básicamente le estamos diciendo que elimine los  huevos vacíos por NA en todo el documento. 

# Vamos a pasar a hacerlo por lagunas: 

library(dplyr)     

Macro_21_numeric <- Macro_21 %>% # Primero pasamos a hacer como numeric todas las columnas menos la primera. Esta nos serviría para trabajar con los datos por mangueo. 
  mutate(across(-1, as.numeric))

# Ahora procedemos a hacer el sumatorio.

Macro_21_laguna <- Macro_21_numeric %>%
  group_by(id) %>% #Agrupa todas las filas por el mismo nombre de la laguna. 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) # Hace sumatorio de todo lo que es numérico (las especies).

# Ya podemos extraer el documento. 

library(openxlsx)

write.xlsx(Macro_21_laguna, "C:/Users/Pablo/Documents/R/extraccion_madrid_22/Macro_21/Macro_21_laguna.xlsx", rowNames = FALSE)







