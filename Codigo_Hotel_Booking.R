# LIMPIEZA INICIAL
rm(list=ls(all=TRUE))      
graphics.off()             
cat("\014")                

# INSTALACIÓN DE LIBRERÍAS NECESARIAS
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("lubridate")

# CARGA DE LIBRERÍAS
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

#cargamos el archivo

setwd("C:/Users/paite/OneDrive/Escritorio/UPC/Data Science")
data_hotel<-read_csv("hotel_bookings.csv")
View(data_hotel)


#==============================================================================================
#INSPECCIONAR DATOS
#exploracion general
head(data_hotel)             
str(data_hotel)             
summary(data_hotel)         
names(data_hotel)            
dim(data_hotel)              
glimpse(data_hotel) 

#convertir "NULL" Y "" A NA
data_hotel_modificada <- data.frame(lapply(data_hotel, function(x) {
  x <- as.character(x)
  x[x == "NULL" | x == ""] <- NA
  return(x)
}), stringsAsFactors = FALSE)

#deteccion de duplicados
sum(duplicated(data_hotel_modificada))                # Cuántos duplicados
data_hotel_modificada <- data_hotel_modificada[!duplicated(data_hotel_modificada), ]

#identificamos tipo de variables
num_cols <- names(Filter(is.numeric, data_hotel_modificada))
cat_cols <- names(Filter(is.character, data_hotel_modificada))
# Variables numericas
print(num_cols)
# Variables categoricas
print(cat_cols)

#conversion a factores de variables categoricas
data_hotel_modificada <- data_hotel_modificada %>%
  mutate(
    hotel = as.factor(hotel),
    meal = as.factor(meal),
    market_segment = as.factor(market_segment),
    distribution_channel = as.factor(distribution_channel),
    reserved_room_type = as.factor(reserved_room_type),
    assigned_room_type = as.factor(assigned_room_type),
    deposit_type = as.factor(deposit_type),
    customer_type = as.factor(customer_type)
  )

#creacion de variable fecha
data_hotel_modificada <- data_hotel_modificada %>%
  mutate(arrival_date = dmy(paste(arrival_date_day_of_month, arrival_date_month, arrival_date_year)))

#==============================================================================================
#PRE-PROCESAMIENTO DE DATOS
#resumir estadisticas basicas
summary(data_hotel_modificada)
range(as.numeric(data_hotel_modificada$lead_time))
table(data_hotel_modificada$hotel)
table(data_hotel_modificada$meal)
table(data_hotel_modificada$reserved_room_type)

#==============================================================================================
#identificacion de datos faltantes
#mostramos todos los elementos NA por columna del dataframe original
colSums(is.na(data_hotel_modificada))

#Porcentaje de datos vacios en la columna children
data_hotel_modificar_Children<-data_hotel_modificada
sum(is.na(data_hotel_modificar_Children$children))
mean(is.na(data_hotel_modificar_Children$children)) * 100

#==============================================================================================
#tratamiento de datos faltantes
#eliminamos los 4 registros que tienen NA en children porque son una cantidad irrelevante
data_hotel_limpio <- data_hotel_modificar_Children[!is.na(data_hotel_modificar_Children$children), ]
#verificamos cuantos registros se eliminaron
nrow(data_hotel_modificar_Children) - nrow(data_hotel_limpio)

#porcentaje de datos vacios en la columna company
data_hotel_modificar_Company<-data_hotel_modificada
sum(is.na(data_hotel_modificar_Company$company))
mean(is.na(data_hotel_modificar_Company$company)) * 100

#eliminamos la columna company debido al alto porcentaje de elementos NULL
data_hotel_limpio <- data_hotel_modificar_Company %>% select(-company)


#porcentaje de datos vacios en la columna agent
data_hotel_modificar_Agent<-data_hotel_limpio
sum(is.na(data_hotel_modificar_Agent$agent))
mean(is.na(data_hotel_modificar_Agent$agent)) * 100

#como el porcentaje de elementos vacios en la columna agent no es lo suficientemente
#pequeño como para eliminar los registros ni lo suficientemente grande
#como para eliminar la columna, vamos a rellenar los datos con la moda

#Calculamos la moda
moda <- names(sort(table(data_hotel_limpio$agent), decreasing = TRUE))[1]

#Reemplazamos NA con la moda
data_hotel_limpio$agent[is.na(data_hotel_limpio$agent)] <- moda


#porcentaje de datos vacios en la columna country
data_hotel_modificar_Country<-data_hotel_limpio
sum(is.na(data_hotel_modificar_Country$country))
mean(is.na(data_hotel_modificar_Country$country)) * 100

#como el porcentaje es menor a 1% vamos a eliminar los registros
data_hotel_limpio <- data_hotel_modificar_Country[!is.na(data_hotel_modificar_Country$country), ]
#verificamos cuantos registros se eliminaron
nrow(data_hotel_modificar_Country) - nrow(data_hotel_limpio)

#====================================================================================================
#Detectar outliers
#utilizamos diagramas de caja (boxplot) para detectar valores atípicos en lead_time y adr
boxplot(as.numeric(data_hotel_limpio$lead_time), main = "Boxplot de lead_time")
boxplot(as.numeric(data_hotel_limpio$adr), main = "Boxplot de adr")

#==============================================================================================
#tratamiento de outliers
# Creamos una función para aplicar winsorización (recorta los valores extremos al percentil 1% y 99%)
winsorizar <- function(x, low = 0.01, high = 0.99) {
  x <- as.numeric(x)
  q <- quantile(x, probs = c(low, high), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

#aplicamos la winsorizacion a las columnas lead_time y adr
data_hotel_limpio$adr <- winsorizar(data_hotel_limpio$adr)
data_hotel_limpio$lead_time <- winsorizar(data_hotel_limpio$lead_time)

#revisamos los resultados después del tratamiento de outliers
summary(data_hotel_limpio$adr)
summary(data_hotel_limpio$lead_time)

#guardamos el archivo para el siguiente paso
write.csv(data_hotel_limpio, "hotel_bookings_limpio.csv", row.names = FALSE)

#==============================================================================================
#liberamos los data frame temporales
rm(data_hotel_modificar_Children)
rm(data_hotel_modificar_Agent)
rm(data_hotel_modificar_Country)
rm(data_hotel_modificar_Company)


#==============================================================================================
#RESPUESTA A LAS PREGUNTAS DEL DOCUMENTO

data_hotel_limpio <- read.csv("hotel_bookings_limpio.csv", stringsAsFactors = FALSE)

# Convertimos la columna de fecha
data_hotel_limpio$arrival_date <- ymd(data_hotel_limpio$arrival_date)

# Creamos columnas adicionales
data_hotel_limpio$mes_llegada <- month(data_hotel_limpio$arrival_date, label = TRUE, abbr = TRUE)
data_hotel_limpio$duracion_estancia <- as.numeric(data_hotel_limpio$stays_in_weekend_nights) +
  as.numeric(data_hotel_limpio$stays_in_week_nights)


#1) ¿Cuantas reservas se realizaron por tipo de hotel?¿Que tipo de hotel prefiere la gente?
table(data_hotel_limpio$hotel)
prop.table(table(data_hotel_limpio$hotel)) * 100

ggplot(data_hotel_limpio, aes(x = hotel)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Reservas por tipo de hotel", x = "Tipo de hotel", y = "Cantidad de reservas") +
  theme_minimal()

#2) ¿Está aumentando la demanda con el tiempo?
data_hotel_limpio %>%
  group_by(arrival_date) %>%
  summarise(reservas = n()) %>%
  ggplot(aes(x = arrival_date, y = reservas)) +
  geom_line(color = "skyblue") +
  labs(title = "Evolución de la demanda con el tiempo", x = "Fecha", y = "Reservas") +
  theme_minimal()


#3) ¿Cuáles son las temporadas de reservas (alta, media, baja)?
data_hotel_limpio %>%
  group_by(arrival_date_month) %>%
  summarise(reservas = n()) %>%
  arrange(desc(reservas))

data_hotel_limpio$arrival_date_month <- factor(data_hotel_limpio$arrival_date_month,
                                levels = month.name)

ggplot(data_hotel_limpio, aes(x = arrival_date_month)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Reservas por Mes", x = "Mes", y = "Reservas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#4) ¿Cuál es la duración promedio de las estancias por tipo de hotel?
data_hotel_limpio$stays_in_week_nights <- as.numeric(as.character(data_hotel_limpio$stays_in_week_nights))
data_hotel_limpio$stays_in_weekend_nights <- as.numeric(as.character(data_hotel_limpio$stays_in_weekend_nights))

data_hotel_limpio %>%
  mutate(estancia_total = stays_in_week_nights + stays_in_weekend_nights) %>%
  group_by(hotel) %>%
  summarise(duracion_promedio = mean(estancia_total, na.rm = TRUE))

data_hotel_limpio %>%
  mutate(estancia_total = stays_in_week_nights + stays_in_weekend_nights) %>%
  group_by(hotel) %>%
  summarise(duracion_promedio = mean(estancia_total, na.rm = TRUE)) %>%
  ggplot(aes(x = hotel, y = duracion_promedio, fill = hotel)) +
  geom_col(width = 0.6) +
  labs(title = "Duración Promedio de Estancia por Tipo de Hotel",
       x = "Tipo de Hotel",
       y = "Duración Promedio (noches)") +
  theme_minimal()



#5) ¿Cuántas reservas incluyen niños y/o bebés?
reservas_con_ninos <- data_hotel_limpio %>%
  filter(as.numeric(children) > 0 | as.numeric(babies) > 0)

ggplot(reservas_con_ninos, aes(x = hotel)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Reservas con ninos y/o bebes por hotel", x = "Hotel", y = "Cantidad") +
  theme_minimal()

cat("Reservas con ninos o bebes:", nrow(reservas_con_ninos), "\n")


ggplot(reservas_con_niños_bebes, aes(x = hotel)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Reservas con ninos y/o bebes por hotel", x = "Hotel", y = "Cantidad") +
  theme_minimal()


#6) ¿Es importante contar con espacios de estacionamiento?

ggplot(data_hotel_limpio, aes(x = as.factor(required_car_parking_spaces), fill = as.factor(is_canceled))) +
  geom_bar(position = "fill") +
  labs(title = "Relacion entre estacionamiento y cancelacion", x = "Espacios requeridos", y = "Proporcion") +
  scale_fill_manual(values = c("skyblue", "red"), name = "¿Cancelado?", labels = c("No", "Si")) +
  theme_minimal()


#7) ¿En qué meses del año se producen más cancelaciones de reservas?
data_hotel_limpio %>%
  filter(is_canceled == 1) %>%
  group_by(arrival_date_month) %>%
  summarise(cancelaciones = n()) %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name)) %>%
  arrange(desc(cancelaciones))

ggplot(filter(data_hotel_limpio, is_canceled == 1), aes(x = factor(arrival_date_month, levels = month.name))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Cancelaciones por Mes", x = "Mes", y = "Cancelaciones") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#8) Pregunta adicional: ¿Influye el canal de distribución en las cancelaciones?
ggplot(data_hotel_limpio, aes(x = distribution_channel, fill = as.factor(is_canceled))) +
  geom_bar(position = "fill") +
  labs(title = "Cancelación por canal de distribución", x = "Canal", y = "Proporción") +
  scale_fill_manual(values = c("skyblue", "red"), name = "¿Cancelado?", labels = c("No", "Sí")) +
  theme_minimal()
