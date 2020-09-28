Laboratorio 5
================

Daniela Dominguez 20180365

``` r
library(nycflights13)
library(tidyverse)
library(lubridate)
library(dplyr)
```

Parte I
=======

``` r
eclipse <- "aug, 21th 2017 18:26:40"
# mdy_hms(eclipse, tz = "GMT")
synodicM <- days(29) + hours(12) + minutes(44) + seconds(3)
saros <- synodicM * 233
siguiente <- mdy_hms(eclipse, tz = "GMT") + saros
siguiente # el siguiente eclipse sera en 23 de junio del 2036, a las 9 de la mañana con 30 minutos y 19 segundos
```

    ## [1] "2036-06-23 09:30:19 GMT"

Parte II
========

``` r
# data <- read_excel("~/Documents/6to semestre/Data Wrangling/Lab5/data.xlsx")
data <- read_csv("~/Documents/6to semestre/Data Wrangling/Lab5/data.csv")
# format(as.Date(43020, origin = "1899-12-30"), '%d-%m-%Y')
df2 <- as.data.frame(sapply(data,gsub,pattern="/",replacement="-"))
head(df2)
```

    ##   Fecha Creación Hora Creación Caller ID           Cod Email SMS Call
    ## 1       10-12-17      22:18:00    368224 Cancelaciones     0   1    0
    ## 2       19-03-17      17:35:00    368224  Otros-Varios     0   1    0
    ## 3       13-03-17      22:03:00    368224     Consultas     0   1    0
    ## 4       14-04-17      17:55:00    368224     Consultas     0   1    0
    ## 5        11-4-17      09:08:00    748633     Consultas     0   1    0
    ## 6       29-04-17      07:19:00    599434 Cancelaciones     0   1    0
    ##   Fecha Final Hora Final
    ## 1    10-12-17   22:29:00
    ## 2    19-03-17   17:52:00
    ## 3    13-03-17   22:27:00
    ## 4    14-04-17   18:09:00
    ## 5     11-4-17   09:13:00
    ## 6    29-04-17   07:23:00

``` r
dmy(head(df2[,1]))
```

    ## [1] "2017-12-10" "2017-03-19" "2017-03-13" "2017-04-14" "2017-04-11"
    ## [6] "2017-04-29"

1. ¿En qué meses existe una mayor cantidad de llamadas por código?
==================================================================

``` r
a <- df2 %>% 
  mutate(df2, mes = month(dmy(df2$`Fecha Creación`)))

a %>% 
  select(mes,Cod) %>% 
  group_by(mes,Cod) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad)) %>% 
  group_by(Cod) %>% 
  slice_max(cantidad, n = 1)
```

    ## # A tibble: 7 x 3
    ## # Groups:   Cod [7]
    ##     mes Cod                          cantidad
    ##   <dbl> <chr>                           <int>
    ## 1     7 0                                1471
    ## 2     5 Actualización de Información     1679
    ## 3     7 Cancelaciones                    4132
    ## 4     3 Cobros                            698
    ## 5    10 Consultas                       10890
    ## 6     3 Empresarial                      3108
    ## 7     9 Otros-Varios                     1116

2. ¿Qué día de la semana es el más ocupado?
===========================================

``` r
b <- df2 %>% 
  mutate(df2, dia = weekdays(as.Date(dmy(df2$`Fecha Creación`),'%d-%m-%Y')))

dia_ocupado <- b %>% 
  group_by(dia) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad))

dia_ocupado
```

    ## # A tibble: 7 x 2
    ##   dia       cantidad
    ##   <chr>        <int>
    ## 1 Sunday       38248
    ## 2 Friday       37843
    ## 3 Monday       37714
    ## 4 Tuesday      37588
    ## 5 Thursday     37544
    ## 6 Saturday     37459
    ## 7 Wednesday    37329

3. ¿Qué mes es el más ocupado?
==============================

``` r
mes_opcupado <-a %>% 
  group_by(mes) %>% 
  summarise(llamadas = n()) %>% 
  arrange(desc(llamadas)) 

mes_opcupado
```

    ## # A tibble: 12 x 2
    ##      mes llamadas
    ##    <dbl>    <int>
    ##  1    10    22681
    ##  2     7    22613
    ##  3     3    22547
    ##  4     8    22413
    ##  5     5    22410
    ##  6    12    22403
    ##  7     1    22202
    ##  8     9    21829
    ##  9    11    21681
    ## 10     4    21631
    ## 11     6    21464
    ## 12     2    19851

4. ¿Existe una concentración o estacionalidad en la cantidad de llamadas?
=========================================================================

``` r
mes_opcupado
```

    ## # A tibble: 12 x 2
    ##      mes llamadas
    ##    <dbl>    <int>
    ##  1    10    22681
    ##  2     7    22613
    ##  3     3    22547
    ##  4     8    22413
    ##  5     5    22410
    ##  6    12    22403
    ##  7     1    22202
    ##  8     9    21829
    ##  9    11    21681
    ## 10     4    21631
    ## 11     6    21464
    ## 12     2    19851

``` r
dia_ocupado
```

    ## # A tibble: 7 x 2
    ##   dia       cantidad
    ##   <chr>        <int>
    ## 1 Sunday       38248
    ## 2 Friday       37843
    ## 3 Monday       37714
    ## 4 Tuesday      37588
    ## 5 Thursday     37544
    ## 6 Saturday     37459
    ## 7 Wednesday    37329

``` r
#Con base a estas tablas se puede observar que las llamadas realizadas tienen cantidades cercanas, por lo que podremos decie que no existe alguna concentracion o estacionalidad en las llamadas
```

5. ¿Cuántos minutos dura la llamada promedio?
=============================================

``` r
c <- data
c$duracion <- c$`Hora Final` - c$`Hora Creación`
c$duracion <- ifelse(c$duracion > 0, 
                               (c$`Hora Final` - c$`Hora Creación`)/60,
                               (((c$`Hora Final` - c$`Hora Creación`)/60)-1440)*-1)

llamada_promedio <- c %>% filter(Call == 1) %>% summarise(Promedio_Minutos = mean(duracion))
llamada_promedio
```

    ## # A tibble: 1 x 1
    ##   Promedio_Minutos
    ##              <dbl>
    ## 1             83.6

6. Realice una tabla de frecuencias con el tiempo de llamada.
=============================================================

``` r
d <- c %>% select(Call, duracion) %>% filter(Call == 1)
frecuencias <- d %>% group_by(duracion) %>% summarise(Frecuencia_LLamadas = sum(Call))
frecuencias
```

    ## # A tibble: 48 x 2
    ##    duracion Frecuencia_LLamadas
    ##       <dbl>               <dbl>
    ##  1        1                 211
    ##  2        2                 173
    ##  3        3                 195
    ##  4        4                 193
    ##  5        5                 184
    ##  6        6                 193
    ##  7        7                 196
    ##  8        8                 209
    ##  9        9                 165
    ## 10       10                 189
    ## # … with 38 more rows

Parte III
=========

``` r
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
```

    ## [1] "Cancer"

PARTE IV
========

1. 1. Genere 4 nuevas columnas para cada variable con formato fecha y hora.
===========================================================================

``` r
vuelos <- flights
vuelos$fecha <- substr(vuelos$time_hour,1,10)
vuelos$dep_time_nueva <- ifelse(vuelos$dep_time >= 1000 & vuelos$dep_time <2500, 
                                paste0(substr(vuelos$dep_time,1,2),":",substr(vuelos$dep_time,3,4),":00"),
                                ifelse(vuelos$dep_time >=100 & vuelos$dep_time <1000, 
                                       paste0("0",substr(vuelos$dep_time,1,1),":",substr(vuelos$dep_time,2,3),":00"),
                                       ifelse(vuelos$dep_time >=10 & vuelos$dep_time <100, 
                                              paste0("00:",vuelos$dep_time,":00"), paste0("00:0",flights$dep_time,":00"))))

vuelos$sched_dep_time_nueva <- ifelse(vuelos$sched_dep_time >= 1000 & vuelos$sched_dep_time <2500, 
                                paste0(substr(vuelos$sched_dep_time,1,2),":",substr(vuelos$sched_dep_time,3,4),":00"),
                                ifelse(vuelos$sched_dep_time >=100 & vuelos$sched_dep_time <1000, 
                                       paste0("0",substr(vuelos$sched_dep_time,1,1),":",substr(vuelos$sched_dep_time,2,3),":00"),
                                       ifelse(vuelos$sched_dep_time >=10 & vuelos$sched_dep_time <100, 
                                              paste0("00:",vuelos$sched_dep_time,":00"), paste0("00:0",flights$sched_dep_time,":00"))))

vuelos$arr_time_nueva <- ifelse(vuelos$arr_time >= 1000 & vuelos$arr_time <2500, 
                                paste0(substr(vuelos$arr_time,1,2),":",substr(vuelos$arr_time,3,4),":00"),
                                ifelse(vuelos$arr_time >=100 & vuelos$arr_time <1000, 
                                       paste0("0",substr(vuelos$arr_time,1,1),":",substr(vuelos$arr_time,2,3),":00"),
                                       ifelse(vuelos$arr_time >=10 & vuelos$arr_time <100, 
                                              paste0("00:",vuelos$arr_time,":00"), paste0("00:0",flights$arr_time,":00"))))

vuelos$sched_arr_time_nueva <- ifelse(vuelos$sched_arr_time >= 1000 & vuelos$sched_arr_time <2500, 
                                paste0(substr(vuelos$sched_arr_time,1,2),":",substr(vuelos$sched_arr_time,3,4),":00"),
                                ifelse(vuelos$sched_arr_time >=100 & vuelos$sched_arr_time <1000, 
                                       paste0("0",substr(vuelos$sched_arr_time,1,1),":",substr(vuelos$sched_arr_time,2,3),":00"),
                                       ifelse(vuelos$sched_arr_time >=10 & vuelos$sched_arr_time <100, 
                                              paste0("00:",vuelos$sched_arr_time,":00"), paste0("00:0",flights$sched_arr_time,":00"))))

vuelos$dep_time_fecha_hora <- paste(vuelos$fecha,vuelos$dep_time_nueva)
vuelos$sched_dep_time_fecha_hora <- paste(vuelos$fecha,vuelos$sched_dep_time_nueva)
vuelos$arr_time_fecha_hora <- paste(vuelos$fecha,vuelos$arr_time_nueva)
vuelos$sched_arr_time_fecha_hora <- paste(vuelos$fecha,vuelos$sched_arr_time_nueva)

#nuevas columnas
head(vuelos[,25:28])
```

    ## # A tibble: 6 x 4
    ##   dep_time_fecha_ho… sched_dep_time_fech… arr_time_fecha_h… sched_arr_time_fech…
    ##   <chr>              <chr>                <chr>             <chr>               
    ## 1 2013-01-01 05:17:… 2013-01-01 05:15:00  2013-01-01 08:30… 2013-01-01 08:19:00 
    ## 2 2013-01-01 05:33:… 2013-01-01 05:29:00  2013-01-01 08:50… 2013-01-01 08:30:00 
    ## 3 2013-01-01 05:42:… 2013-01-01 05:40:00  2013-01-01 09:23… 2013-01-01 08:50:00 
    ## 4 2013-01-01 05:44:… 2013-01-01 05:45:00  2013-01-01 10:04… 2013-01-01 10:22:00 
    ## 5 2013-01-01 05:54:… 2013-01-01 06:00:00  2013-01-01 08:12… 2013-01-01 08:37:00 
    ## 6 2013-01-01 05:54:… 2013-01-01 05:58:00  2013-01-01 07:40… 2013-01-01 07:28:00

2. Encuentre el delay total que existe en cada vuelo. El delay total se puede encontrar sumando el delay de la salida y el delay de la entrada.
===============================================================================================================================================

``` r
vuelos$dep_time_nueva <- hms(vuelos$dep_time_nueva)
vuelos$sched_dep_time_nueva <- hms(vuelos$sched_dep_time_nueva)
vuelos$arr_time_nueva <- hms(vuelos$arr_time_nueva)
vuelos$sched_arr_time_nueva <- hms(vuelos$sched_arr_time_nueva)

vuelos$delay <- (vuelos$dep_time_nueva - vuelos$sched_dep_time_nueva) + (vuelos$arr_time_nueva - vuelos$sched_arr_time_nueva)
#conversion a minutos
vuelos$delay <- period_to_seconds(vuelos$delay)/60
head(vuelos$delay)
```

    ## [1]  13  24  35 -19 -31   8
