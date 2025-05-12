#######################################
## Introducción al análisis de datos ##
#######################################
# Cargar paquetes o librerias
library ("Hmisc")
library ("dplyr")
library ("reshape2")
library("ggplot2")
library ("RColorBrewer")
library("tidyr")
library("grid") 
library("gridExtra")
library("scales")
library("ggridges")
library ("treemap")
library ("ggthemes")
library ("viridis")

# Cargar base de datos de agronet como objeto a R
agricola <- read.delim ("agronet.23.txt", dec=",")

str (agricola) # revisar estructura de los datos cargados
names (agricola) # revisar el nombre de las columnas del data frame
head(agricola) # primera observaciones del data.frame

## Asignar factores y variables numericas a las columnas
## que componen el set de datos
agricola$depa <- as.factor (agricola$depa) # asignar categoria de factor a las columna
agricola$muni <- as.factor (agricola$muni) # asignar categoria de factor a las columna
agricola$grupo <- as.factor (agricola$grupo) # asignar categoria de factor a las columna
agricola$culti <- as.factor (agricola$culti) # asignar categoria de factor a las columna
agricola$ciclo <- as.factor (agricola$ciclo)

agricola$ar.semb <- as.numeric (agricola$ar.semb) # asignar categoria de variable numerica a las columna
agricola$ar.cose <- as.numeric (agricola$ar.cose) # asignar categoria de variable numerica a las columna
agricola$produ <- as.numeric (agricola$produ) # asignar categoria de variable numerica a las columna
agricola$rendi <- as.numeric (agricola$rendi) # asignar categoria de variable numerica a las columna

str (agricola) 
names (agricola)

levels (agricola$grupo) # observar lista con niveles de un factor
levels (agricola$depa)
levels (agricola$culti)

# Pregunta. Clasificacion de las observaciones
# Asignar categorias 
agricola <- agricola %>% mutate(tamanho_cul = case_when(
  ar.semb <=5 ~ "Pequenho",
  ar.semb >5 & ar.semb <=10 ~ "Mediano",
  ar.semb >10 ~ "Grande"))

# reducir nombres de dapartamentos
levels (agricola$depa) <- c("AMAZ","ANTI","ARAU","ATLA","SAYP",               
                            "BOLI","BOYA","CALD","CAQU",                 
                            "CASA","CAUC","CESA","CHOC",                   
                            "CORD","CUNDI","GUAI","GUAV",                
                            "HUIL","GUAJ","MAGD","META",
                            "NARI","NO.SA","PUTU","QUIN",                 
                            "RISA","SAYP",
                            "SANT","SUCR","TOLI","VALL",         
                            "VAUP", "VICH")


## Analisis de leguminosas
# filtrar leguminosas
leguminosas <- agricola %>% filter (grupo == "LEGUMINOSAS")

# Tendencias observadas en la produccion de leguminosas
leguminosas.1 <- filter (leguminosas, ar.semb > 0 & 
                           ar.cose > 0 & produ > 0 & 
                           rendi > 0)


ggplot(leguminosas.1,aes(x=reorder(depa, -rendi, FUN=mean), y=rendi)) +
  geom_boxplot(alpha=1,colour="black",fill="royalblue3") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + 
  ylab (expression(paste('Rendimiento'~(Ton~x~ha^-1)))) + xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=12), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

# tendencias en el tiempo de leguminosas
# promedios area sembrada y produccion

names (leguminosas.1)
legu.prom.anho <- leguminosas.1 [-c(1:5,11)] %>% 
  group_by (ano) %>%
  summarise_all (list(mean=mean, sd=sd))

ggplot(legu.prom.anho) + 
  geom_line(aes (x=ano,y=produ_mean)) +
  geom_point(aes(x = ano, y = produ_mean), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = ano, y = produ_mean),colour = "blue", shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste ('Año'))) +
  ylab (expression (paste ('Producción'~(ton)))) +
  theme_bw() +
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                               2014,2015,2016,2017,2018,2019,2020,2021,
                               2022,2023))+
  theme (axis.title = element_text (size=12), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot(legu.prom.anho) + 
  geom_line(aes (x=ano,y=ar.semb_mean)) +
  geom_point(aes(x = ano, y = ar.semb_mean), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = ano, y = ar.semb_mean),colour = "blue", shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste ('Año'))) +
  ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme_bw() +
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                               2014,2015,2016,2017,2018,2019,2020,2021,
                               2022,2023))+
  theme (axis.title = element_text (size=12), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())


# total de area sembrada y produccion de leguminosas
names (leguminosas.1)
legu.total <- leguminosas.1 [-c(1:5,11)] %>% 
  group_by (ano) %>%
  summarise_all (list(suma=sum))

ggplot(legu.total) + 
  geom_line(aes (x=ano,y=produ_suma)) +
  geom_point(aes(x = ano, y = produ_suma), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = ano, y = produ_suma),colour = "forestgreen", shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste ('Año'))) +
  ylab (expression (paste ('Producción'~(ton)))) +
  theme_bw() +
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                               2014,2015,2016,2017,2018,2019,2020,2021,
                               2022,2023))+
  theme (axis.title = element_text (size=12), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot(legu.total) + 
  geom_line(aes (x=ano,y=ar.semb_suma)) +
  geom_point(aes(x = ano, y = ar.semb_suma), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = ano, y = ar.semb_suma),colour = "forestgreen", shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste ('Año'))) +
  ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme_bw() +
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                               2014,2015,2016,2017,2018,2019,2020,2021,
                               2022,2023))+
  theme (axis.title = element_text (size=12), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())


# Producción total por cultivo y por departamento
names (leguminosas.1)
legu.total.2 <- leguminosas.1 [-c(2:4,6,11)] %>% 
  group_by (depa,culti) %>%
  summarise_all (list(suma=sum))

# comparar municipios en produccion de arroz

mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(15)

ggplot(legu.total.2,aes(x=reorder(depa, produ_suma, sum), 
                           y=produ_suma/1000, fill=culti)) +
  geom_bar(position = position_stack(), stat = "identity", width = 0.8, 
           colour = "white") +
  theme_bw() + 
  scale_fill_manual(name="Cultivo", values = mycolors) +
  ylab (expression(paste('Producción'~(miles~de~ton)))) +
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "right")

ggplot(legu.total.2,aes(x=reorder(depa, ar.semb_suma, sum), 
                        y=ar.semb_suma/1000, fill=culti)) +
  geom_bar(position = position_stack(), stat = "identity", width = 0.8, 
           colour = "white") +
  theme_bw() + 
  scale_fill_manual(name="Cultivo", values = mycolors) +
  ylab (expression(paste('Area sembrada'~(miles~de~ha)))) +
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "right")

# mas producción y sembrados frijol, soya, arveja y habichuela

names (leguminosas.1)
legu.total.3 <- leguminosas.1 [-c(3,4,6,11)] %>% 
  filter (culti %in% c("FRIJOL","SOYA","ARVEJA","HABICHUELA")) %>%
  group_by (depa,muni,culti) %>%
  summarise_all (list(suma=sum, mean=mean, sd=sd))

# Elaboracion de mapa
names (leguminosas.1)

legu.total.4 <- leguminosas.1 [-c(2:6,11)] %>% 
  group_by (depa) %>%
  summarise_all (list(suma=sum))

library(sf)
my.sf.muni <- read_sf("municipios_GeoJSON.geojson")
my.sf.depa <- read_sf("custom.geo.json")

# Plot it with ggplot2
ggplot(my.sf.muni) +
  geom_sf(fill = "skyblue3", color = "white") +
  theme_dark()

my.sf.muni$dpt <- as.factor (my.sf.muni$dpt)
levels (my.sf.muni$dpt) <- c("AMAZ","ANTI","ARAU","SAYP","ATLA",               
                            "BOLI","BOYA","CALD","CAQU",                 
                            "CASA","CAUC","CESA","CHOC",                   
                            "CORD","CUNDI","GUAI","GUAV",                
                            "HUIL","GUAJ","MAGD","META",
                            "NARI","NO.SA","PUTU","QUIN",                 
                            "RISA","CUNDI",
                            "SANT","SUCR","TOLI","VALL",         
                            "VAUP", "VICH")



# Presentación de los datos por departamento
my.sf.merged <- my.sf.muni %>%
  left_join(legu.total.4, by = c("dpt" = "depa")) %>% 
  # Note that if the number of restaurant is NA, it is in fact 0
  mutate(ar.semb_suma = ifelse(is.na(ar.semb_suma), 0.01, ar.semb_suma)) %>%
  mutate(produ_suma = ifelse(is.na(produ_suma), 0.01, produ_suma))


#  Total area sembrada en 13 años
ggplot(my.sf.merged) +
  geom_sf(aes(fill = ar.semb_suma), linewidth = 0, alpha = 0.8) +
  theme_void() +
    scale_fill_viridis_c(
    trans = "log", limits = c(0.01, 500000), 
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 500000),
    labels = label_comma(),
    name = "Área sembrada (ha)",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      ncol = 1
    )
  ) +
  labs(
    title = "Total de área sembrada con leguminosas", 
    subtitle = "desde el 2006 hasta el 2023"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(
      size = 14, hjust = 0.01, color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 1,
        unit = "cm"
      )
    ),
    plot.subtitle = element_text(
      size = 12, hjust = 0.01,
      color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.43, l = 1,
        unit = "cm"
      )
    ),
    legend.position = "right"
    )


#  Total producción en 13 años
ggplot(my.sf.merged) +
  geom_sf(aes(fill = produ_suma), linewidth = 0, alpha = 0.8) +
  theme_void() +
  scale_fill_viridis_c(
    trans = "log", limits = c(0.01, 900000), 
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 900000),
    labels = label_comma(),
    name = "Producción (Ton)",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      ncol = 1
    )
  ) +
  labs(
    title = "Total de toneladas producidas de leguminosas", 
    subtitle = "desde el 2006 hasta el 2023"
  ) +
    theme(
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(
        size = 14, hjust = 0.01, color = "#4e4d47",
        margin = margin(
          b = -0.1, t = 0.4, l = 1,
          unit = "cm"
        )
      ),
      plot.subtitle = element_text(
        size = 12, hjust = 0.01,
        color = "#4e4d47",
        margin = margin(
          b = -0.1, t = 0.43, l = 1,
          unit = "cm"
        )
      ),
    legend.position = "right"
  )

names (my.sf.merged)
plot(my.sf.merged [c(5,7)])

## Analisis de raices


