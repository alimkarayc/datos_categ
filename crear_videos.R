#############################################
#  Presentacion Electivo: Datos Categoricos #
#                01-2026                    #
#           Camila Herrera C.               #
#############################################
setwd("C:/Users/camil/OneDrive/Escritorio/IE/8/E. Datos Cat/videos")

#librerias-----
library(ggplot2)
library(gganimate)
library(av)
library(transformr)
library(scales)

azul_oscuro <- "#12355B"
azul        <- "#4A90E2"
gris        <- "#64748B"
fondo       <- "#F8FAFC"



# VIDEO 1----
set.seed(2026)

puntos <- data.frame(
  x = c(
    runif(18, -4, 0.8),
    runif(18, -0.8, 4)
  ),
  y = c(
    rep(0, 18),
    rep(1, 18)
  )
)

puntos$y_grafico <- puntos$y +
  runif(nrow(puntos), -0.035, 0.035)

x_grid <- seq(-4, 4, length.out = 250)

curvas <- rbind(
  data.frame(
    x = x_grid,
    probabilidad = 0.50 + 0.18 * x_grid,
    estado = "Modelo lineal: puede producir valores fuera de 0 y 1"
  ),
  data.frame(
    x = x_grid,
    probabilidad = plogis(1.15 * x_grid),
    estado = "Modelo logístico: mantiene la probabilidad entre 0 y 1"
  )
)

video_logit <- ggplot() +
  
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = 0,
    ymax = 1,
    fill = "#EAF4FF",
    alpha = 0.65
  ) +
  
  geom_hline(
    yintercept = c(0, 1),
    linewidth = 0.7,
    linetype = "dashed",
    color = gris
  ) +
  
  geom_point(
    data = puntos,
    aes(
      x = x,
      y = y_grafico
    ),
    shape = 21,
    size = 4.2,
    stroke = 0.9,
    fill = "white",
    color = azul_oscuro,
    alpha = 0.9
  ) +
  
  geom_line(
    data = curvas,
    aes(
      x = x,
      y = probabilidad,
      group = 1
    ),
    linewidth = 2.2,
    color = azul
  ) +
  
  scale_x_continuous(
    breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  scale_y_continuous(
    limits = c(-0.25, 1.25),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    title = "¿Por qué usamos regresión logística?",
    subtitle = "{closest_state}",
    x = "Valor del predictor",
    y = "Probabilidad estimada",
    caption = "La respuesta observada toma los valores 0 y 1"
  ) +
  
  theme_minimal(base_size = 20) +
  
  theme(
    plot.background = element_rect(
      fill = fondo,
      color = NA
    ),
    panel.background = element_rect(
      fill = fondo,
      color = NA
    ),
    plot.title = element_text(
      color = azul_oscuro,
      face = "bold",
      size = 28,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      color = azul,
      face = "bold",
      size = 20,
      hjust = 0.5
    ),
    plot.caption = element_text(
      color = gris,
      size = 14,
      hjust = 0.5
    ),
    axis.title = element_text(
      color = azul_oscuro,
      face = "bold"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(30, 50, 30, 50)
  ) +
  
  transition_states(
    estado,
    transition_length = 3,
    state_length = 2,
    wrap = FALSE
  ) +
  
  ease_aes("cubic-in-out")


## REDENDERIZAMOS-----
animacion_1 <- animate(
  video_logit,
  width = 1280,
  height = 720,
  res = 120,
  fps = 20,
  nframes = 300,
  start_pause = 20,
  end_pause = 30,
  device = "ragg_png",
  renderer = av_renderer(
    "videos/video_logit.mp4"
  )
)






# VIDEO 2-----
escenarios <- data.frame(
  estado = c(
    "Sin recordatorio SMS: probabilidad estimada de 27%",
    "Con recordatorio SMS: probabilidad estimada de 10%"
  ),
  probabilidad = c(0.27, 0.10),
  paciente = "Paciente"
)

video_prediccion <- ggplot(
  escenarios,
  aes(
    x = paciente,
    y = probabilidad
  )
) +
  
  geom_col(
    width = 0.48,
    fill = azul,
    alpha = 0.85
  ) +
  
  geom_point(
    size = 9,
    shape = 21,
    fill = "white",
    color = azul_oscuro,
    stroke = 2
  ) +
  
  geom_hline(
    yintercept = c(0.10, 0.27),
    linetype = "dashed",
    linewidth = 0.6,
    color = gris,
    alpha = 0.6
  ) +
  
  scale_y_continuous(
    limits = c(0, 0.35),
    breaks = seq(0, 0.35, by = 0.05),
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    title = "De los predictores a una probabilidad individual",
    subtitle = "{closest_state}",
    x = NULL,
    y = "Probabilidad estimada de inasistencia",
    caption = paste(
      "Perfil: 20 días de espera",
      "sin inasistencia previa",
      sep = " · "
    )
  ) +
  
  theme_minimal(base_size = 21) +
  
  theme(
    plot.background = element_rect(
      fill = fondo,
      color = NA
    ),
    panel.background = element_rect(
      fill = fondo,
      color = NA
    ),
    plot.title = element_text(
      color = azul_oscuro,
      face = "bold",
      size = 27,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      color = azul,
      face = "bold",
      size = 19,
      hjust = 0.5
    ),
    plot.caption = element_text(
      color = gris,
      size = 15,
      hjust = 0.5
    ),
    axis.title.y = element_text(
      color = azul_oscuro,
      face = "bold"
    ),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(30, 50, 30, 50)
  ) +
  
  transition_states(
    estado,
    transition_length = 4,
    state_length = 2,
    wrap = FALSE
  ) +
  
  ease_aes("cubic-in-out")


## REDENDERIZAMOS 2--------
animacion_2 <- animate(
  video_prediccion,
  width = 1280,
  height = 720,
  res = 120,
  fps = 20,
  nframes = 280,
  start_pause = 20,
  end_pause = 30,
  device = "ragg_png",
  renderer = av_renderer(
    "C:/Users/camil/OneDrive/Escritorio/IE/8/E. Datos Cat/videos/video_prediccion.mp4"
  )
)
