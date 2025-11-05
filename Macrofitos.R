
                 ## MATRIZ MACRÓFITOS CLIMARISKINPOND ##

# Esta es la última matriz me parece que voy a meter en los resúmenes, pero esta matriz tiene una particularidad. 
# Esta matriz tiene que ser por mangueo, no tiene sentido hacerla por laguna, ya que los datos por mangueo tienen
# que sumar un 100%, es verdad que podrías extrapolar más o menos la vegetación de la laguna en fucnión de la suma 
# de todos los componentes de cada mangueo, sin embargo hay que tener en cuenta que no vas a contemplar bien las 
# proporciones y es posible que te dejes algún tipo de heliófito, árboles, etc. que no dejen reflejados los mangueos. 

install.packages("tidyr") 
install.packages("openxlsx")

# 1º: Empezemos mirando como tenemos los datos medio limpios. 

Macrofitos <- read.csv2("C:/Users/Pablo/Documents/R/extraccion_madrid_22/Macrofitos/Macrofitos.csv")

# Voy a intentar convertir en numéricas las columnas con los datos de los macrófitos.

# 2º: Vamos a convertir los huecos vacíos en NA. 

library(tidyr)

Macrofitos$id[Macrofitos$id == ""] <- NA # Básicamente le estamos diciendo que en la columna id lo huevos vacíos ("") son NA. 

# 4º: Una vez tenemos esto, podemos rellenar los huevos vacíos. 

Macrofitos <- Macrofitos %>% fill(id, .direction = "down")

# 5º: No era mi intención esta vez, pero voy a quitarme algunas columnas que creo que no se van a utilizar. 

Macrofitos_limpia <- Macrofitos [, -c(2:6)]

# 6º: Quiero rellenar los huevos vacíos pero en el resto de la matriz. 

Macrofitos_limpia[Macrofitos_limpia == ""] <- NA # Básicamente le estamos diciendo que en la columna id lo huevos vacíos ("") son NA. 

# 7º: Voy a extraer el documento a ver qué tal se ve. 

library(openxlsx)

write.xlsx(Macrofitos_limpia, "C:/Users/Pablo/Documents/R/extraccion_madrid_22/Macrofitos/Macrofitos limpia.xlsx", rowNames = FALSE)






