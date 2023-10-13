
# ---------------------------- #
# EJERCICIOS DE CLASE          #
# RUTH JORGANES Y SARA MONEO   #
# ---------------------------- #


# EJERCICIO 1

# Crea un data.frame o tibble a partir del fichero credit. Tiene datos de ingresos y balance
# de tarjetas de crédito, así como datos sociodemográficos de los clientes.
# Calcula la media de los ingresos (columna Income) a nivel de Gender y Ethnicity. O sea,
# calcula la media de Income para cada posible par de valores de Gender y Ethnicity.

library(readr)
library(dplyr)

credit_delim <- read_delim("../Herramientas/Práctica/credit")

credit_delim

df_credit <- as.data.frame(credit_delim)

glimpse(df_credit)


#Cálculo de la media

df_credit %>% 
  group_by(Gender, Ethnicity) %>% 
  summarise(mean(Income))




# EJERCICIO 2

# Mete todo lo anterior en una función cuyo argumento de entrada sea la ruta al fichero.
# La función devolverá el resultado con la media a los niveles mencionados anteriormente (lo
# más cómodo para ti es que este resultado sea un data.frame o tibble, pero plantéalo como
# quieras).


credit_funcion <- function(ruta) {
  credit_delim <- read_delim(ruta)
  df_credit <- as.data.frame(credit_delim)
  df_respuesta <- df_credit %>% 
    group_by(Gender, Ethnicity) %>% 
    summarise(mean(Income))
  return(df_respuesta)
}

credit_funcion("../Herramientas/Práctica/credit")



# EJERCICIO 3
# A partir de los datos calculados antes, replica este gráfico. He usado el tema claro con la capa
# theme_light() (el color de relleno es lo de menos: pon el que quieras, pero especifica uno):

df_credit2 <- df_credit %>% 
  group_by(Gender, Ethnicity) %>% 
  summarise(media =mean(Income))


ggplot(df_credit2) + 
  geom_col(aes(x=Gender, y =media), color = "purple", fill="purple") +
  facet_wrap(~Ethnicity) +
  theme_light()



# EJERCICIO 4
# Completa ahora tu función anterior para que, en lugar de devolver los datos de antes, devuelva
# el gráfico anterior. La función tendrá ahora tres argumentos:
# • La ruta al fichero
# • El título del gráfico
# • El color de relleno


credit_funcion <- function(ruta, título, colorinchi) {
  credit_delim <- read_delim(ruta)
  df_credit <- as.data.frame(credit_delim)
  df_respuesta <- df_credit %>% 
    group_by(Gender, Ethnicity) %>% 
    summarise(media =mean(Income))
  ggplot(df_credit2)+ 
    geom_col(aes(x=Gender, y =media), color = "purple", fill=colorinchi) +
    facet_wrap(~Ethnicity)+
    theme_light()+
    labs(title=título)
}

credit_funcion("../Herramientas/Práctica/credit", "Sara", "purple")






