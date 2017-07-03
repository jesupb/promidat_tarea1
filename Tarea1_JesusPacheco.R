# I.
lenses <- read.csv("data/lenses.csv", head = F, 
                col.names = c("id", "edad", "prescripcion", "astigmatico", "tasa_lagrimas", "clase_lentes"))

modelo <- FactoMineR::PCA(dplyr::select(lenses, edad, prescripcion, astigmatico, tasa_lagrimas), 
                          graph = FALSE)

draw.circle <- function(centre, radius, npoints = 100, ...) {
  theta <- seq(0, 2*pi, length.out = npoints)
  x <- radius * cos(theta) + centre[1]
  y <- radius * sin(theta) + centre[2]
  lines(x, y, ...)
}

# funciones auxiliares ----------------------------------------------------


plot(modelo, ylim = c(-2, 2), xlim = c(-2, 2))
draw.circle(c(-0.28, 1.39), 0.71, col = "red", lwd = 2)
draw.circle(c(-1.31, -0.26), 0.67, col = "blue", lwd = 2)
draw.circle(c(0.28, -1.39), 0.71, col = "chartreuse4", lwd = 2)
draw.circle(c(1.31, 0.26), 0.67, col = "orange2", lwd = 2)

data.frame(modelo$ind$coord) %>% 
  mutate(grupo = grupos$cluster) %>% 
  group_by(grupo) %>% 
  summarise(avg_x = mean(Dim.1), avg_y = mean(Dim.2)) %>% 
  mutate(rad = sqrt(avg_x^2 + avg_y^2) / 2)

par(mfrow = c(2,2))
plot(xx)


# III.
library(dplyr)
library(ggmap)
library(ggplot2)

crime2 <- crime %>% filter(offense == "murder")

houston_map <- (get_map(location = c(lon = mean(crime2$lon), lat = mean(crime2$lat)),
                       zoom = 11,
                       source = "google",
                       messaging = T))

ggmap(houston_map, extent = "device") + 
  geom_point(data = crime2, aes(x = lon, y=lat, color=day, size=hour), alpha = 0.5) 
