library(tidyverse)
library(gt)
library(devtools)
library(milestones)


## Initialize defaults
column <- lolli_styles()

## Read data
data <- read_csv(col_names=TRUE, show_col_types=FALSE, file= 
                   "event,     date,    category, comment
        Hinnamnor, 2022-8-28, 5, 242 km W of Keelung
        Kompasu,   2022-10-7, 0, Taiwan
        Chanthu,   2021-9-5,  5, Taiwan
        In Fa,     2021-7-15, 2, Taiwan
        Surigae,   2021-4-11, 5, Taiwan
        Maysak,    2020-8-26, 2, Taiwan
        Bavi,      2020-8-20, 3, Taiwan")

## Make sure dates are real dates
data$date <- ymd(data$date)

## Generate a table
gt(data) |>
  fmt_date(columns = date,
           date_style = "m_day_year") |>
  tab_footnote(
    footnote = "tropical storm = 0",
    locations = cells_column_labels(columns=category)) |>
  tab_source_note(source_note = "Source: worlddata.info")


column$color <- "orange"
column$size  <- 4
column$source_info <- "Source: Wikipedia"

## Milestones timeline
a <- milestones(datatable = data, styles = column)
a
a$layers


## -------

timeline <- function(data, time){
  
  datos <- data |>
    arrange(by = date) |> 
    mutate(time = abs(as.numeric(date-date[nrow(data)])),
           lines = case_when(nrow(data) > 4 ~ rep(c(2,2,5,5), length.out = nrow(data)) * rep(c(1,-1), length.out = nrow(data)),
                             T ~ 3))
  
  ggplot(datos) + 
  # Base
  geom_segment(aes(y = 0-max(time)*0.1, yend = max(time)+max(time)*0.1, x = 0, xend = 0)) + # Linea central
  geom_segment(aes(y = 0-max(time)*0.1, yend = 0-max(time)*0.1, x = -0.1, xend = 0.1)) + # Linea base
  geom_segment(aes(y = max(time)+max(time)*0.1, yend = max(time)+max(time)*0.1, x = -0.1, xend = 0.1)) + # Linea tope
  annotate(geom = "text", label = datos$date[1]+max(datos$time)*0.15, x = 0, y = max(datos$time)+max(datos$time)*0.15) + # Fecha inicial
  annotate(geom = "text", label = datos$date[nrow(datos)]-max(datos$time)*0.15, x = 0, y = 0-max(datos$time)*0.15) + # Fecha final
  # Titulo
  annotate(geom = "text", label = "Timeline", x = 0, y = max(datos$time)+max(datos$time)*0.3) +
  # Eventos
  geom_segment(aes(y = time, yend = time, x = 0, xend = lines), linetype ="dashed") + # Linea del evento
  geom_point(aes(y = time, x = lines), color = "firebrick3") + # Punto del evento
  annotate(geom = "text", label = datos$date, x = datos$lines, y = datos$time+max(datos$time)*0.05) + # Tiempo del evento
  annotate(geom = "text", label = datos$event, x = datos$lines+sign(datos$lines)*2, y = datos$time) + # Comentarios de los eventos
  # Escalas de los ejes
  scale_y_continuous(limits = c(0-max(datos$time)*0.2,max(datos$time)+max(datos$time)*0.3)) + 
  scale_x_continuous(limits = c(-10,10))
  
}



