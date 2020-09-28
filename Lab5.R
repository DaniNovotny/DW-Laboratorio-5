library(nycflights13)
library(tidyverse)
library(lubridate)
library(dplyr)


# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

# Parte I

eclipse <- "aug, 21th 2017 18:26:40"
# mdy_hms(eclipse, tz = "GMT")
synodicM <- days(29) + hours(12) + minutes(44) + seconds(3)
saros <- synodicM * 233
siguiente <- mdy_hms(eclipse, tz = "GMT") + saros
siguiente # el siguiente eclipse sera en 23 de junio del 2036, a las 9 de la mañana con 30 minutos y 19 segundos


# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

# Parte II

# data <- read_excel("~/Documents/6to semestre/Data Wrangling/Lab5/data.xlsx")
data <- read_csv("~/Documents/6to semestre/Data Wrangling/Lab5/data.csv")
# format(as.Date(43020, origin = "1899-12-30"), '%d-%m-%Y')
df2 <- as.data.frame(sapply(data,gsub,pattern="/",replacement="-"))
head(df2)

dmy(head(df2[,1]))


# 1.	¿En qué meses existe una mayor cantidad de llamadas por código?
a <- df2 %>% 
  mutate(df2, mes = month(dmy(df2$`Fecha Creación`)))

a %>% 
  select(mes,Cod) %>% 
  group_by(mes,Cod) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad)) %>% 
  group_by(Cod) %>% 
  slice_max(cantidad, n = 1)

# 2.	¿Qué día de la semana es el más ocupado?

b <- df2 %>% 
  mutate(df2, dia = weekdays(as.Date(dmy(df2$`Fecha Creación`),'%d-%m-%Y')))

#b <- data %>% 
#  mutate(df2, dia = day(dmy(df2$`Fecha Creación`)))

b %>% 
  group_by(dia) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad))

b$signo <- hms(b$`Hora Final`) - hms(b$`Hora Creación`)
b$duracion <- ifelse(b$signo >= 0 , 
                               (hms(b$Hora_Final) - hms(b$Hora_Creacion))/60,
                               (((hms(b$Hora_Creacion) - hms(b$Hora_Final))/60)-1440)*-1) #Ajuste 24hrs (1440 mins)





# 3.	¿Qué mes es el más ocupado?

a %>% 
  group_by(mes) %>% 
  summarise(llamadas = n()) %>% 
  arrange(desc(llamadas)) 

# 4.	¿Existe una concentración o estacionalidad en la cantidad de llamadas?

library(highcharter)

a %>% 
  group_by(mes) %>% 
  summarise(llamadas = n()) %>% 
  hchart("column", hcaes(x = mes, y = n)) 

df2 %>% 
  select(year,artist) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(artist)) %>% 
  hchart("column", hcaes(x = year, y = n)) %>% 
  hc_title( text = "<b>Artistas distintos por año <b>") %>% 
  hc_subtitle(text = "<i>2019 tuvo la menor variedad, mientras que 2015 ha sido el año con mayor diversidad de artistas.<i>")


# 5.	¿Cuántos minutos dura la llamada promedio?

# 6.	Realice una tabla de frecuencias con el tiempo de llamada.


# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

# Parte III


#mdy("aug, 21th 2017") > mdy("oct, 21th 2017")

#fecha <- readline(prompt="Ingrese su cumpleaños en la siguiente forma: DD.MM.YYYY  ejemplo: 27.01.1990: ")
#dmy(fecha)

#days(365) - days(mdy("aug, 21th 2017"))
#make_date(month = 11, day = 21) > make_date( month = month(mdy("aug, 21th 1960")), day = day(mdy("aug, 21th 1960")))

fecha <- "jul. 09 1998"

vector <- c("Dec. 22 2020","Jan. 19 2020","Capricorn",
            "Jan. 20 2020","Feb. 17 2020","Aquarius",
            "Feb. 18 2020","Mar. 19 2020","Pisces",
            "March 20 2020","April 19 2020","Aries",
            "April 20 2020","May 19 2020","Taurus",
            "May 20 2020","June 20 2020","Gemini",
            "June 21 2020","July 21 2020","Cancer",
            "July 22 2020","Aug. 22 2020","Leo",
            "Aug 23 2020","Sept. 21 2020","Virgo",
            "Sep. 22 2020","Oct. 22 2020","Libran",
            "Oct. 23 2020","Nov. 21 2020","Scorpio",
            "Nov. 22 2020","Dec. 21 2020","Sagittarius")

matriz <- matrix(vector,12,3,TRUE)


# format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[1,1])), "%m-%d")

if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[1,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[1,2])), "%m-%d")) {
  sig <- matriz[1,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[2,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[2,2])), "%m-%d")) {
  sig <- matriz[2,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[3,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[3,2])), "%m-%d")) {
  sig <- matriz[3,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[4,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[4,2])), "%m-%d")) {
  sig <- matriz[4,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[5,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[5,2])), "%m-%d")) {
  sig <- matriz[5,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[6,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[6,2])), "%m-%d")) {
  sig <- matriz[6,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[7,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[7,2])), "%m-%d")) {
  sig <- matriz[7,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[8,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[8,2])), "%m-%d")) {
  sig <- matriz[8,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[9,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[9,2])), "%m-%d")) {
  sig <- matriz[9,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[10,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[10,2])), "%m-%d")) {
  sig <- matriz[10,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[11,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[11,2])), "%m-%d")) {
  sig <- matriz[11,3]
} else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[12,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[12,2])), "%m-%d")) {
  sig <- matriz[12,3]
} 





funcion <- function() {
  fecha <- readline(prompt="Ingrese su cumpleaños en la siguiente forma: MM.DD.YYYY  ejemplo: 01.27.1990: ")
  if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[1,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[1,2])), "%m-%d")) {
    sig <- matriz[1,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[2,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[2,2])), "%m-%d")) {
    sig <- matriz[2,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[3,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[3,2])), "%m-%d")) {
    sig <- matriz[3,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[4,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[4,2])), "%m-%d")) {
    sig <- matriz[4,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[5,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[5,2])), "%m-%d")) {
    sig <- matriz[5,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[6,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[6,2])), "%m-%d")) {
    sig <- matriz[6,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[7,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[7,2])), "%m-%d")) {
    sig <- matriz[7,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[8,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[8,2])), "%m-%d")) {
    sig <- matriz[8,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[9,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[9,2])), "%m-%d")) {
    sig <- matriz[9,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[10,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[10,2])), "%m-%d")) {
    sig <- matriz[10,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[11,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[11,2])), "%m-%d")) {
    sig <- matriz[11,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[12,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[12,2])), "%m-%d")) {
    sig <- matriz[12,3]
  } 
  return(sig)
}

funcion()


funcion2 <- function(fecha) {
  if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[1,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[1,2])), "%m-%d")) {
    sig <- matriz[1,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[2,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[2,2])), "%m-%d")) {
    sig <- matriz[2,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[3,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[3,2])), "%m-%d")) {
    sig <- matriz[3,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[4,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[4,2])), "%m-%d")) {
    sig <- matriz[4,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[5,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[5,2])), "%m-%d")) {
    sig <- matriz[5,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[6,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[6,2])), "%m-%d")) {
    sig <- matriz[6,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[7,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[7,2])), "%m-%d")) {
    sig <- matriz[7,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[8,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[8,2])), "%m-%d")) {
    sig <- matriz[8,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[9,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[9,2])), "%m-%d")) {
    sig <- matriz[9,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[10,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[10,2])), "%m-%d")) {
    sig <- matriz[10,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[11,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[11,2])), "%m-%d")) {
    sig <- matriz[11,3]
  } else if (format(as.Date(mdy(fecha)), "%m-%d") > format(as.Date(mdy(matriz[12,1])), "%m-%d") && format(as.Date(mdy(fecha)), "%m-%d") < format(as.Date(mdy(matriz[12,2])), "%m-%d")) {
    sig <- matriz[12,3]
  } 
  return(sig)
}

funcion2("07.09.1990")

# PARTE IV




# raul: 
operaciones$signo <- operaciones$Hora_Final - operaciones$Hora_Creacion
operaciones$duracion <- ifelse(operaciones$signo >= 0, 
                               (operaciones$Hora_Final - operaciones$Hora_Creacion)/60,
                               (((operaciones$Hora_Creacion - operaciones$Hora_Final)/60)-1440)*-1) #Ajuste 24hrs (1440 mins)


