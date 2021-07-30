# Librerias ----
library(tidyverse)
library(datos)

# 28.2.1 Ejercicios ----

# Crea un gráfico partiendo de los datos 
# de economía de combustible con etiquetas 
# para title , subtitle, caption, x, y y 
# color personalizadas.
ggplot(millas, aes(ciudad, autopista)) + 
  geom_point(aes(fill = combustible),
             shape = 21,
             position = position_jitter(width  = 1, 
                                        height = 1, 
                                        seed   = 1234)) + 
  scale_fill_discrete(labels = c("Diesel", "Etanol",
                                 str_glue("Gas natural 
                                        comprimido"),
                                 "Premium", "Regular")) + 
  labs(title = "Consumo de energía de vehículos",
       x = "Millas/galón en ciudad",
       y = "Millas/galón en autopista",
       fill = str_glue("Tipo de 
                       combustible"),
       caption = "Fuente: http://fueleconomy.gov")

# La función geom_smooth() es un poco engañosa 
# porque autopista está sesgada positivamente 
# para motores grandes, debido a la inclusión 
# de autos deportivos livianos con motores grandes. 
# Usa tus herramientas de modelado para ajustar y 
# mostrar un modelo mejor.

ggplot(millas, aes(cilindrada, autopista)) +
  geom_point(aes(colour = clase)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Tamaño del motor (litros)",
    y = "Economía de combustible de carretera (millas)",
    colour = "Tipo de automóvil"
  ) + 
  annotate(geom = "rect",
           xmin = 5.5, 
           xmax = 7.5,
           ymin = 22,
           ymax = 27, 
           alpha = 0.2, fill = "red") +
  annotate(geom = "text",
           x = 5,
           y = 35,
           label = "2 asientos") + 
  annotate(geom = "curve",
           x = 5,
           y = 34,
           xend = 5.5,
           yend = 27,
           arrow = arrow(length = unit(2, "mm")))

## Primera opción
ggplot(millas, aes(cilindrada, autopista)) +
  geom_point(aes(colour = clase)) +
  geom_smooth(se = FALSE) +
  geom_smooth(method = "lm", se = FALSE,
              color = "red") +
  labs(
    x = "Tamaño del motor (litros)",
    y = "Economía de combustible de carretera (millas)",
    colour = "Tipo de automóvil"
  )

## Segunda opción
ggplot(millas, aes(cilindrada, autopista)) +
  geom_point(aes(colour = clase)) +
  geom_smooth(aes(color = clase),
              method = "lm", se = FALSE) +
  labs(
    x = "Tamaño del motor (litros)",
    y = "Economía de combustible de carretera (millas)",
    colour = "Tipo de automóvil"
  ) + 
  annotate(geom = "rect",
           xmin = 5.5, 
           xmax = 7.5,
           ymin = 22,
           ymax = 27, 
           alpha = 0.2, fill = "red") +
  annotate(geom = "text",
           x = 5,
           y = 35,
           label = "2 asientos") + 
  annotate(geom = "curve",
           x = 5,
           y = 34,
           xend = 5.5,
           yend = 27,
           arrow = arrow(length = unit(2, "mm")))

# Elige un gráfico exploratorio que hayas creado 
# en el último mes y agrégale títulos informativos 
# para volverlo más fácil de comprender para otros.

# 28.3.1 Ejercicios ----

# Usa las infinitas posiciones que permite 
# geom_text() para colocar texto en cada 
# una de las cuatro esquinas del gráfico.

# Usa las infinitas posiciones que permite 
# geom_text() para colocar texto en cada una 
# de las cuatro esquinas del gráfico.

## Ver https://ggplot2.tidyverse.org/reference/geom_text.html
data <- tibble(x = c(-1, -1, 1, 1),
               y = c(-1,  1, 1, -1),
               text = rep("texto", 4))

ggplot(data, aes(x, y)) +
  geom_point() + 
  geom_text(aes(label = text),
            hjust = "inward", 
            vjust = "inward")

# ¿Cómo interactúan las etiquetas producidas
# por geom_text() con la separación en 
# facetas? ¿Cómo puedes agregar una etiqueta 
# a una sola faceta? ¿Cómo puedes poner una 
# etiqueta diferente en cada faceta? 
# (Sugerencia: piensa en los datos 
# subyacentes).

## Ver https://r-graphics.org/recipe-annotate-facet
label_tbl <- tibble(clase = unique(millas$clase),
                    text  = unique(millas$clase)) 

ggplot(millas) +
  geom_point(aes(ciudad, autopista)) +
  geom_text(data = label_tbl,
            aes(x = 25, y = 20,
                label = text),
            size = 3) +
  facet_wrap(facets = vars(clase))

## Otra posibilidad es annotate
## pero aparecera en todas las facetas
ggplot(millas) +
  geom_point(aes(ciudad, autopista)) + 
  annotate(geom = "text", 
           x = 25, y = 20,
           label = "Texto",
           size = 3) +
  facet_wrap(facets = vars(clase))

# ¿Qué argumentos para geom_label() 
# controlan la apariencia de la caja 
# que se ve atrás?

## Sin modificaciones
ggplot(millas) +
  geom_point(aes(ciudad, autopista)) +
  geom_label(data = label_tbl,
            aes(x = 25, y = 20,
                label = text),
            size = 3) +
  facet_wrap(facets = vars(clase))

## Con modificaciones
ggplot(millas) +
  geom_point(aes(ciudad, autopista)) +
  geom_label(data = label_tbl,
             aes(x = 25, y = 20,
                 label = text),
             size = 3,
             color = "black",
             fill = "red",
             alpha = 0.7,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0.2, "lines"),
             label.size = 0.3,
             fontface = "bold",
             family = "mono") +
  facet_wrap(facets = vars(clase))

# ¿Cuáles son los cuatro argumentos de arrow()? 
# ¿Cómo funcionan? Crea una serie de gráficos 
# que demuestren las opciones más importantes.
ggplot(millas, aes(cilindrada, autopista)) +
  geom_point(aes(colour = clase)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Tamaño del motor (litros)",
    y = "Economía de combustible de carretera (millas)",
    colour = "Tipo de automóvil"
  ) + 
  annotate(geom = "rect",
           xmin = 5.5, 
           xmax = 7.5,
           ymin = 22,
           ymax = 27, 
           alpha = 0.2, fill = "red") +
  annotate(geom = "text",
           x = 5,
           y = 35,
           label = "2 asientos") + 
  annotate(geom = "curve",
           x = 5,
           y = 34,
           xend = 5.5,
           yend = 27,
           arrow = arrow(angle = 30,
                         length = unit(2, "mm"),
                         # "last", "first", "both"
                         end = "last",
                         # "open", "closed"
                         type = "open"))

# 28.4.4 Ejercicios ----

# ¿Por qué el siguiente código no reemplaza 
# la escala predeterminada?

set.seed(1234)
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  # El color esta relacinado con el 
  # borde y fill con la escala que se
  # muesta
  geom_hex(color = "black") + 
  coord_fixed(ratio = 1) + 
  # Esto no modificara el fill
  scale_colour_gradient(low = "white", high = "red")


ggplot(df, aes(x, y)) +
  geom_hex(color = "black") + 
  coord_fixed(ratio = 1) +
  # Esta si lo modificará
  scale_fill_gradient(low = "white", high = "red")

# ¿Cuál es el primer argumento para cada escala? 
# ¿Cómo se compara con labs()?  

## En la mayoría es "name"
## Algunas excepciones son discrete_scale()
## continuous_scale()

## Gráfico original
ggplot(millas) +
  geom_point(aes(ciudad, autopista))

## Gráfico modificado
ggplot(millas) +
  geom_point(aes(ciudad, autopista)) + 
  scale_x_continuous(name = "Millas/galón en ciudad") + 
  scale_y_continuous(name = "Millas/galón en autopista")

# Cambia la visualización de los términos presidenciales

presidencial <- presidencial %>%
  mutate(id = 33 + row_number())

presidencial %>% 
  ggplot(aes(inicio, id, color = partido)) +
  geom_point() +
  geom_segment(aes(xend = fin, yend = id)) +
  scale_colour_manual(values = c(Republicano = "red", 
                                 Demócrata = "blue")) + 
  scale_x_date(name = NULL,
               date_labels = "%Y",
               breaks = c(presidencial$inicio[1],
                          presidencial$fin)) +
  scale_y_continuous(name = NULL,
                     breaks = presidencial$id, 
                     labels = presidencial$nombre) +
  labs(color = NULL,
       title = "Presidentes y partidos políticos en USA",
       subtitle = str_glue("Periodo: {presidencial$inicio[1]} : {presidencial$fin[length(presidencial$fin)]}")) + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8, 
                                   face = "bold",
                                   angle = 90, 
                                   vjust = 0.5),
        axis.text.y = element_text(size = 8, 
                                   face = "bold"),
        title = element_text(size = 10), 
        plot.subtitle = element_text(size = 8))

# Utiliza override.aes para que la leyenda en el 
# siguiente gráfico sea más fácil de ver:

## Gráfico original
ggplot(diamantes, aes(quilate, precio)) +
  geom_point(aes(colour = corte), alpha = 1 / 20)

## Gŕafico con cambios
ggplot(diamantes, aes(quilate, precio)) +
  geom_point(aes(colour = corte), alpha = 1 / 20) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
