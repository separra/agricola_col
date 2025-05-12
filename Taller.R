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

# Cargar base de datos de agronet como objeto a R
agricola <- read.delim ("agronet.23.txt", dec=",")

frijol <- filter (agricola, culti=="FRIJOL")

#write.table (frijol, "frijol_1.xls", 
#             sep="\t", dec=",", row.names=F)

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

# pregunta. Clasificacion de las observaciones
# Asignar categorias 
agricola <- agricola %>% mutate(tamanho_cul = case_when(
  ar.semb <=5 ~ "Pequenho",
  ar.semb >5 & ar.semb <=10 ~ "Mediano",
  ar.semb >10 ~ "Grande"))

## Extraer de la base de datos un grupo de observaciones
## y generar un nuevo objeto 
## martes 11 de marzo
names (agricola)
cereales.1 <- filter(agricola, grupo=="CEREALES")
arroz <- filter (agricola, culti=="ARROZ")
cereales.2 <- filter (cereales.1, depa=="RISARALDA"|
                        depa=="META")

maiz.pereira <- filter (agricola, culti=="MAIZ" & muni=="PEREIRA") 
maiz.pereira.1 <- filter (agricola, culti=="MAIZ" | muni=="PEREIRA") 

names (cereales.1)
cereales.3 <- cereales.1 [c(1,2,4:9)]
names (cereales.3)
cereales.4 <- select (cereales.1, c(depa, muni)) 
cereales.5 <- filter (agricola, grupo=="CEREALES" & 
                        depa=="VALLE DEL CAUCA")
cereales.tulua <- filter (cereales.1, muni=="TULUA")
cereales.top <- filter (cereales.1, ar.semb >= 10) # filtrar por valores numericos 
cereales.int <- filter (cereales.1, ar.semb > 5 & ar.semb < 10)
cereales.low <- filter (cereales.1, ar.semb <= 5)


################
# calcular promedio para arroz

arroz <- arroz %>% group_by(depa)
prom.arroz.depa <- summarise_each(arroz, 
                                  funs (mean(.,na.rm=TRUE)))
ungroup(arroz)
names (arroz)
arroz <- arroz %>% group_by(ano)
prom.arroz <- summarise_each(arroz, 
                  funs (mean(.,na.rm=TRUE)))

ungroup(arroz)
arroz <- arroz %>% group_by(depa, muni)
prom.arroz.muni <- summarise_each(arroz, 
                             funs (mean(.,na.rm=TRUE)))



# tendencia produccion arroz desde 2006 a 2023
ggplot(prom.arroz) + 
  geom_line(aes (x=ano,y=produ)) +
  geom_point(aes(x = ano, y = produ), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = ano, y = produ),colour = "blue", shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste ('Año'))) +
  ylab (expression (paste ('Semilla producida'~(ton)))) +
  theme_bw() + 
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                               2014,2015,2016,2017,2018,2019,2020,2021,
                               2022,2023))+
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=14, angle=90), 
         legend.position="",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

# comparar municipios en produccion de arroz
ggplot(prom.arroz.muni,aes(x=reorder(muni, ar.semb, mean), 
                     y=ar.semb)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.8, 
           colour = "white", fill="blue") +
  theme_bw() + facet_wrap(vars(depa), nrow=3, scales="free_x") +
  ylab (expression(paste('Área sembrada'~(ha)))) +
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=5), 
         axis.text = element_text(size=5), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

#write.table (prom.arroz.depa, "promedio arroz por depa.txt", 
#            sep="\t", dec=",", row.names=F)

cereales.10 <- mutate(cereales.1, ar.per = ar.semb - ar.cose) ## crear una variable nueva


## Pregunta: proporción de tipo de cultivadores en el total de la 
## base de datos.
## Elaboración de treemap
levels (agricola$depa) <- c("AMAZ","ANTI","ARAU","ATLA","SAYP",               
                            "BOLI","BOYA","CALD","CAQU",                 
                            "CASA","CAUC","CESA","CHOC",                   
                            "CORD","CUNDI","GUAI","GUAV",                
                            "HUIL","GUAJ","MAGD","META",
                            "NARI","NO.SA","PUTU","QUIN",                 
                            "RISA","SAYP",
                            "SANT","SUCR","TOLI","VALL",         
                            "VAUP", "VICH")

## compartir por correo
names (agricola)
table <- as.data.frame (xtabs(~tamanho_cul+depa, agricola))
table.1 <- as.data.frame (xtabs(~tamanho_cul+depa+culti, agricola))
#table.2 <- as.data.frame (xtabs(~tamanho_cul+depa+muni, agricola))
table$Freq <- as.numeric (table$Freq)
vec <- table$Freq!=0
table <- table[vec,] ##Subset as usual
dev.new(width=10, height=10)
treemap(table,
        index=c("depa","tamanho_cul"), 
        vSize="Freq",
        type="index", border.col=c("black","white"),
        overlap.labels = 0.2, 
        palette="Blues",
        fontsize.labels = 8,
        align.labels=list(
          c("center", "center"), 
          c("right", "top"))) 
dev.off()

## Primer paso: observar la distribucion de las variables numéricas

boxplot(cereales.1$ar.semb, main = "Area sembrada", col="dodgerblue",
        ylab=expression(Area~sembrada~(ha)))

summary(cereales.1$ar.semb)

boxplot(cereales.1$ar.cose, main = "Area cosechada", col="forestgreen",
        ylab=expression(Area~cosechada~(ha)))
boxplot(cereales.1$produ, main = "Producción", col="gold",
        ylab=expression(Produccion~(ton)))
boxplot(cereales.1$rendi, main = "Rendimiento", col="green3", 
        ylab=expression(Rendimiento~(ton~x~ha^-1)))

summary (agricola$rendi)

hist(cereales.1$ar.semb, main = "Area sembrada", col="dodgerblue",
        xlab=expression(Area~sembrada~(ha)), breaks=10)
hist(cereales.1$ar.cose, main = "Area cosechada", col="forestgreen",
        xlab=expression(Area~cosechada~(ha)))
hist(cereales.1$produ, main = "Producción", col="gold",
        xlab=expression(Produccion~(ton)))
hist(cereales.1$rendi, main = "Rendimiento", col="green3", 
        xlab=expression(Rendimiento~(ton~x~ha^-1)))

## Elaboración de diagrama de cajas 
# Distribución de los datos por departamento

cereales.1 <-cereales.1 %>% 
  filter(!is.na(ar.semb), !is.na(rendi)) 


## Distribución de la variable área sembrada
ggplot(cereales.5, aes(x=reorder(culti,ar.semb, mean), y=ar.semb)) +
  geom_boxplot(alpha=0.5,colour="black",fill="green4") +
  stat_summary(fun.y=mean, geom="point", col="black", shape = 10, size=5) +
  theme_grey() + 
  ylab (expression(paste('Área sembrada'~(ha^-1)))) + 
  xlab (expression(paste('Cultivo'))) +
  theme (axis.title = element_text (size=2), 
         axis.text = element_text(size=2), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

ggplot(cereales.5, aes(x=reorder(muni, ar.semb, FUN=mean), y=ar.semb)) +
  geom_boxplot(alpha=1,colour="black",fill="forestgreen") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + #facet_grid(. ~ gr.culti, scales = "free", space = "free") +
  ylab (expression(paste('Área sembrada'~(ha)))) + 
  xlab (expression(paste('Municipio'))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

ggplot(cereales.5, aes(x=muni, y=ar.semb)) +
  geom_boxplot(alpha=1,colour="black",fill="forestgreen") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + 
  ylab (expression(paste('Área sembrada'~(ha)))) + 
  xlab (expression(paste('Municipio'))) +
  facet_wrap(vars(culti), nrow=2, scales="free_x") +
  theme (axis.title = element_text (size=7), 
         axis.text = element_text(size=7), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

## eficiencia municipios cereales en el valle
ggplot(cereales.5, aes(x=reorder(muni, rendi, FUN=mean), y=rendi)) +
  geom_boxplot(alpha=1,colour="black",fill="forestgreen") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + #facet_grid(. ~ gr.culti, scales = "free", space = "free") +
  ylab (expression(paste('Rendimiento'~(ton~x~ha^-1)))) + 
  xlab (expression(paste('Municipio'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=12), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

ggplot(cereales.5, aes(x=reorder(muni, rendi, FUN=mean), y=rendi)) +
  geom_boxplot(alpha=1,colour="black",fill="forestgreen") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + facet_wrap(vars(culti), nrow=2, scales="free_x") +
  ylab (expression(paste('Rendimiento'~(ton~x~ha^-1)))) + 
  xlab (expression(paste('Municipio'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=12), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

## promedios por cultivo y municipio para cereales en el Valle
cereales.5 <- cereales.5 %>% group_by(culti, muni) #agrupaci?n por a?o##
prom.cereales.valle <- summarise_all (cereales.5, 
                             funs (mean(.,na.rm=TRUE))) #promedio#)


ggplot (prom.cereales.valle, aes(x=reorder(muni, rendi, FUN=mean), y=rendi)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.8, 
           colour = "white", fill="blue") +
  theme_bw() + facet_wrap(vars(culti), nrow=2, scales="free_x") +
  ylab (expression(paste('Rendimiento'~(ton~x~ha^-1)))) + 
  xlab (expression(paste('Municipio'))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")



## Separa rendimiento por cultivos de cereales por municipios
f1 <- ggplot(cereales.5, aes(x=reorder(muni, rendi, FUN=mean), y=rendi)) +
  geom_boxplot(alpha=1,colour="black",fill="forestgreen") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + facet_wrap(vars(culti), nrow=2, scales="free_x") +
  ylab (expression(paste('Rendimiento'~(ton~x~ha^-1)))) + 
  xlab (expression(paste('Municipio'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=8), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

library(plotly)
ggplotly(f1, tooltip="text")

## sabado 29 de marzo

#separar el diagrama por cultivo##
ggplot(cereales.1, aes(x=reorder(culti,-ar.semb, FUN=mean), y=ar.semb)) +
  geom_boxplot(alpha=1,colour="black",fill="forestgreen") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + #facet_grid(. ~ depa, scales = "free", space = "free") +
  ylab (expression(paste('Área sembrada'~(ha)))) + 
  xlab (expression(paste('Cultivo'))) +
  theme (axis.title = element_text (size=12), 
         axis.text = element_text(size=10), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")


## Tarea Combinar variables numericas en ggplot2 boxplot
names (cereales.1)
reshape.cereales <- melt(cereales.1 [,-c(3,4)], id=c("depa","muni",
                                                     "culti")) ## transformar la tabla para realizar figuras de area

names(reshape.cereales)

ggplot(reshape.cereales, aes(x=reorder(depa, -value, FUN=mean), 
                             y=value, fill=variable)) +
  geom_boxplot(alpha=1,colour="black") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + #facet_grid(. ~ depa, scales = "free", space = "free") +
  ylab (expression(paste('valor'~(ha)))) + 
  xlab (expression(paste('Cultivo'))) +
  theme (axis.title = element_text (size=12), 
         axis.text = element_text(size=10), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

areas <- filter (reshape.cereales, variable=="ar.semb"|variable=="ar.cose")

ggplot(areas, aes(x=reorder(depa, -value, FUN=mean), 
                             y=value, fill=variable)) +
  geom_boxplot(alpha=1,colour="black") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=2, 
               position=position_dodge(0.8)) +
  theme_bw() +
  scale_fill_brewer(palette="Set1", ) + 
  ylab (expression(paste('Area'~(ha)))) + 
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=12), 
         axis.text = element_text(size=10), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "right")

ggplot(areas, aes(x=depa, y=value, fill=variable, color=variable)) +
  geom_boxplot(alpha=1,colour="black") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=2, 
               position=position_dodge(0.8)) +
  theme_bw() +
  scale_fill_brewer(palette="Set1", ) + 
  scale_color_brewer(palette="Set1") +
  ylab (expression(paste('Area'~(ha)))) + 
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=12), 
         axis.text = element_text(size=10), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "right")

ggplot(areas, aes(x=culti, y=value, fill=variable)) +
  geom_boxplot(alpha=1,colour="black") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=2, 
               position=position_dodge(0.8)) +
  theme_bw() +
  scale_fill_brewer(palette="Set2") + 
  ylab (expression(paste('Area'~(ha)))) + 
  xlab (expression(paste(''))) +
  theme (axis.title = element_text (size=12), 
         axis.text = element_text(size=10), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "right")

par (c(1,2,1,1))
par (mfrow=c(2,2))
boxplot (cereales.1$rendi,xlab="Rendimiento", col="dodgerblue")
boxplot (cereales.1$ar.semb,xlab="Area sembrada", col="forestgreen", horizontal=T)
hist (cereales.1$rendi, col="dodgerblue", xlab="Rendimiento", main="", breaks=50)
hist (cereales.1$ar.semb, col="forestgreen", xlab="Area sembrada", main="", 
      breaks=50)
dev.off ()

#ggsave ("figura1.png", width=15, height=10)


summary (cereales.1$rendi)
summary (cereales.1$ar.semb)
summary (arroz$ar.semb)
mean (cereales.1$ar.semb, na.rm = T)
sd (cereales.1$ar.semb, na.rm = T)

summary (cereales.1$ar.semb)
mean (cereales.1$ar.semb, na.rm = T)
sd (cereales.1$ar.semb, na.rm = T)

## Dia 3 seisón martes 19
#Distribución de la variable rendimiento#
ggplot(cereales.1,aes(x=reorder(depa, -rendi, FUN=mean), y=rendi)) +
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

ggplot(cereales.1,aes(x=reorder(culti, -rendi, FUN=mean), y=rendi)) +
  geom_boxplot(alpha=1,colour="black",fill="royalblue3") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + 
  ylab (expression(paste('Rendimiento'~(Ton~x~ha^-1)))) + xlab (expression(paste('Cultivo'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=12), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

##Analisis de los promedios por anho##
cereales.1 <- cereales.1 %>% group_by(ano) #agrupaci?n por a?o##
prome.anho <- summarise_each(cereales.1, 
                             funs (mean(.,na.rm=TRUE))) #promedio#
head(prome.anho)

#write.table (prome.anho, "promedios por año.txt", sep="\t", dec=",")



#Grafica lineas y puntos Tendencia en el tiempo 
ggplot(prome.anho)+ 
  geom_line(aes (x=ano,y=ar.semb)) +
  geom_point(aes(x = ano, y = ar.semb), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = ano, y = ar.semb),colour = "blue", shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste ('Año'))) +
  ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme_bw() + 
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                              2014,2015,2016,2017,2018,2019,2020,2021,
                              2022,2023)) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=14, angle=90), 
         legend.position="",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

#ggplotly(f2, tooltip="text")

#lineas sencilla#
ggplot(prome.anho)+ 
  geom_line(aes (x=ano,y=ar.semb),size=3,colour ="blue3")+ #size para cambiar grosor de la l?nea#
  xlab (expression (paste (''))) +
  ylab (expression (paste ('Area sembrada'~(hectareas)))) +
  theme_bw() + scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                                            2014,2015,2016,2017,2018,2019,2020,2021,
                                            2022,2023)) +
  theme (strip.text.y = element_text (face="bold", size=12, angle=0), 
         strip.background = element_rect(fill="grey95"),
         axis.title = element_text (size=14), 
         axis.text = element_text(size=14, angle=90), legend.position="",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())


## Calculo tendencia por cultivo y año
ungroup(cereales.1)
cereales.1 <- cereales.1 %>% group_by(ano, culti) #agrupaci?n por a?o##
prome.maiz <- summarise_each(cereales.1, 
                  funs (mean(.,na.rm=TRUE)))

## Tendencia todos los cereales
ggplot(prome.maiz)+ 
  geom_line(aes (x=ano,y=ar.semb, color=culti)) +
  geom_point(aes(x = ano, y = ar.semb), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = ano, y = ar.semb, colour = culti), shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste ('Año'))) +
  ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme_bw() + scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                                            2014,2015,2016,2017,2018,2019,2020,
                                            2021,2022,2023)) +
  scale_color_brewer(name="Cultivo", palette="Paired") + 
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=14, angle=90),
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank(), 
         legend.position = "right")


ggplot(prome.maiz)+ 
  geom_line(aes (x=ano,y=ar.semb, color=culti)) +
  geom_point(aes(x = ano, y = ar.semb), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = ano, y = ar.semb, colour = culti), shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste ('Año'))) +
  ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme_bw() + scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                                            2014,2015,2016,2017,2018,2019,2020,
                                            2021,2022,2023)) +
  scale_color_manual(name="Cultivo", values=c("red3", "yellow2", "green2",
                            "blue4", "forestgreen", "lightblue2", "magenta4",
                            "brown2", "grey50", "black", "darkorange2")) + 
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=14, angle=90),
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank(), 
         legend.position = "right")

# Dia 4
# Tendencia solo maiz y arroz
arroz.maiz <- filter (prome.maiz, culti=="MAIZ"|culti=="ARROZ")

ggplot(filter(prome.maiz, culti=="MAIZ"|culti=="ARROZ"))+ 
  geom_line(aes (x=ano,y=ar.semb/100, color=culti)) +
  geom_point(aes(x = ano, y = ar.semb/100), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = ano, y = ar.semb/100, colour = culti), shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste ('Año'))) +
  ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme_bw() + scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                                            2014,2015,2016,2017,2018,2019,2020, 
                                            2021,2022,2023)) +
  scale_color_manual(values = c("dodgerblue", "forestgreen")) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=14, angle=90),
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank(), 
         legend.position = "right")

ggplot(filter(prome.maiz, culti=="MAIZ"|culti=="ARROZ"))+ geom_line(aes (x=anho,y=produ, color=culti)) +
  geom_point(aes(x = anho, y = produ), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = anho, y = produ, colour = culti), shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste ('Año'))) +
  ylab (expression (paste ('Producción'~(ton)))) +
  theme_bw() + scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                                            2014,2015,2016,2017,2018,2019,2020)) +
  scale_color_manual(values = c("dodgerblue", "forestgreen")) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=14, angle=90),
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank(), 
         legend.position = "right")

## Tarea grafica doble eje Y

##Rendimiento##
ggplot(prome.anho) + geom_line(aes (x=ano,y=rendi))+
  geom_point(aes(x = ano, y = rendi), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = ano, y = rendi),colour = "blue", shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  xlab (expression (paste (''))) +
  ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme_bw() + scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                                            2014,2015,2016,2017,2018,2019,2020,2021, 
                                            2022, 2023)) +
  theme (strip.text.y = element_text (face="bold", size=12, angle=0), 
         strip.background = element_rect(fill="grey95"),
         axis.title = element_text (size=14), 
         axis.text = element_text(size=14, angle=90), legend.position="",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())


ggplot(cereales.1, aes(x=reorder_within (culti,rendi,culti,mean), 
                          y=rendi)) + ##no correr##
  geom_boxplot(alpha=1,colour="black",fill="blue") +
  scale_x_reordered() +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + 
  ylab (expression(paste('Rendimiento'~(Ton~x~ha^-1)))) + 
  xlab (expression(paste('Cultivo'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=12), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

## Dia 4
##calcular promedio##
ungroup(cereales.1)
cereales.1 <-cereales.1 %>% group_by(culti) #agrupaci?n por cultivo promedio##
prome.culti <- summarise_each(cereales.1,
                              funs (mean(.,na.rm=TRUE)))#promedio#
prome.culti$culti <- factor(prome.culti$culti, 
                    levels=prome.culti$culti[order(prome.culti$rendi)]) #crea de menor a mayor en orden##organiza los datos#


# Figura de rendimiento por ha

ggplot(prome.culti, aes(rendi, culti)) +
  geom_point(colour = "black", size = 4, shape=1, stroke=1) +
  geom_point(colour = "green4", shape=16, size = 4) + 
  #geom_vline(xintercept = 16.9, linetype="solid", 
  #color = "forestgreen", size=0.2) +
  ylab ("") + xlab (expression(paste('Rendimiento'~(Ton~x~ha^-1)))) + 
  theme_bw() +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=8), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())

ggplot(prome.culti, aes(culti, rendi)) +
  geom_point(colour = "black", size = 4, shape=1, stroke=1) +
  geom_point(colour = "green4", shape=16, size = 4) + 
  #geom_vline(xintercept = 16.9, linetype="solid", 
  #color = "forestgreen", size=0.2) +
  ylab ("") + xlab (expression(paste('Rendimiento'~(Ton~x~ha^-1)))) + 
  theme_bw() +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=8), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())

ggplot(prome.culti, aes(y=reorder(culti, -rendi), x=rendi)) +
  geom_point(colour = "black", size = 4, shape=1, stroke=1) +
  geom_point(colour = "green4", shape=16, size = 4) + 
  #geom_vline(xintercept = 16.9, linetype="solid", 
  #color = "forestgreen", size=0.2) +
  ylab ("") + xlab (expression(paste('Rendimiento'~(Ton~x~ha^-1)))) + 
  theme_bw() +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=8), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())

# Agrupación por departamento y por cultivo
ungroup(cereales.1)
cereales.1 <- cereales.1 %>% group_by(depa,culti)
promdptoculti<- summarise_each(cereales.1,funs (mean(.,na.rm=TRUE)))
ungroup(cereales.1)

source ("Function se.r")

se.ar.semb <- summarySE (cereales.1, measurevar = "ar.semb", 
                         groupvars = c("depa", "culti"), na.rm = TRUE)
se.ar.cose <- summarySE (cereales.1, measurevar = "ar.cose", 
                         groupvars = c("depa", "culti"), na.rm = TRUE)
se.produ <- summarySE (cereales.1, measurevar = "produ", 
                       groupvars = c("depa", "culti"), na.rm = TRUE)
se.rendi <- summarySE (cereales.1, measurevar = "rendi", 
                       groupvars = c("depa", "culti"), na.rm = TRUE)

##Funcion para cambiar de menor a mayor en los tipos de cultivos##
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}
####

levels (se.ar.semb$depa) <- c("AMAZ","ANTI","ARAU","ATLA","SAYP",               
                            "BOLI","BOYA","CALD","CAQU",                 
                            "CASA","CAUC","CESA","CHOC",                   
                            "CORD","CUNDI","GUAI","GUAV",                
                            "HUIL","GUAJ","MAGD","META",
                            "NARI","NO.SA","PUTU","QUIN",                 
                            "RISA","SAYP",
                            "SANT","SUCR","TOLI","VALL",         
                            "VAUP", "VICH")

# Figura sola comparación de promedio por departamento y cultivo
ggplot(se.ar.semb,aes(x=reorder_within(depa, ar.semb, culti, mean), 
                      y=ar.semb)) +
  scale_x_reordered() +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.8, 
           colour = "white", fill="blue") +
  geom_errorbar (aes(ymin=ar.semb-se, 
                     ymax=ar.semb+se), 
                 position = position_dodge (0.1), linetype ="solid", 
                 colour="black", width=0.5, size=0.3) +
  theme_bw() + facet_wrap(vars(culti), nrow=2, scales="free_x") +
  ylab (expression(paste('Área sembrada'~(ha)))) + 
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=9), 
         axis.text = element_text(size=7), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

ggplot(se.ar.semb,aes(x=reorder_within(culti, ar.semb, culti, mean), 
                      y=ar.semb)) +
  scale_x_reordered() +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.8, 
           colour = "white", fill="blue") +
  geom_errorbar (aes(ymin=se.ar.semb$ar.semb-se.ar.semb$se, 
                     ymax=se.ar.semb$ar.semb+se.ar.semb$se), 
                 position = position_dodge (0.1), linetype ="solid", 
                 colour="black", width=0.5, size=0.3) +
  theme_bw() + facet_wrap(vars(depa), nrow=2, scales="free_x") +
  ylab (expression(paste('Área sembrada'~(ha)))) + 
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=9), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

ggplot(se.ar.semb,aes(x=reorder_within(depa, ar.semb, culti, mean), 
                      y=ar.semb, fill=culti)) +
  scale_x_reordered() +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.8) +
  geom_errorbar (aes(ymin=se.ar.semb$ar.semb-se.ar.semb$se, 
                     ymax=se.ar.semb$ar.semb+se.ar.semb$se), 
                 position = position_dodge (0.1), linetype ="solid", 
                 colour="black", width=0.5, size=0.3) +
  scale_fill_brewer(name="Cultivo", palette = "Paired") +
  theme_bw() + 
  ylab (expression(paste('Área sembrada'~(ha)))) + 
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=8), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "bottom")

levels (se.rendi$depa) <- c("AMAZ","ANTI","ARAU","ATLA","SAYP",               
                              "BOLI","BOYA","CALD","CAQU",                 
                              "CASA","CAUC","CESA","CHOC",                   
                              "CORD","CUNDI","GUAI","GUAV",                
                              "HUIL","GUAJ","MAGD","META",
                              "NARI","NO.SA","PUTU","QUIN",                 
                              "RISA","SAYP",
                              "SANT","SUCR","TOLI","VALL",         
                              "VAUP", "VICH")

ggplot(se.rendi,aes(x=reorder_within(depa, rendi, culti, mean), 
                      y=rendi, fill=culti)) +
  scale_x_reordered() +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.8) +
  geom_errorbar (aes(ymin=se.rendi$rendi-se.rendi$se, 
                     ymax=se.rendi$rendi+se.rendi$se), 
                 position = position_dodge (0.1), linetype ="solid", 
                 colour="black", width=0.5, size=0.3) +
  scale_fill_brewer(name="Cultivo", palette = "Paired") +
  theme_bw() + 
  ylab (expression(paste('Rendimiento'~(ton~x~ha^-1)))) + 
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=8), 
         axis.text = element_text(size=8), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "bottom")

ggplot(se.produ,aes(x=reorder_within(depa, produ, culti, mean), 
                    y=produ, fill=culti)) +
  scale_x_reordered() +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.8) +
  geom_errorbar (aes(ymin=se.produ$produ-se.produ$se, 
                     ymax=se.produ$produ+se.produ$se), 
                 position = position_dodge (0.1), linetype ="solid", 
                 colour="black", width=0.5, size=0.3) +
  scale_fill_brewer(name="Cultivo", palette = "Paired") +
  theme_bw() + 
  ylab (expression(paste('Produccion'~(ton)))) + 
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=10), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "bottom")

## Figura separada por departamentos
#se.rendi$depa<-substring(se.rendi$depa, 1,4)
se.rendi <- filter (se.rendi, rendi!=0)

ggplot(se.rendi,aes(x=reorder_within(depa, rendi, culti, mean), 
                    y=rendi, fill=culti)) +
  scale_x_reordered() +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.8, 
           color="black") +
  geom_errorbar (aes(ymin=se.rendi$rendi-se.rendi$se, 
                     ymax=se.rendi$rendi+se.rendi$se), 
                 position = position_dodge (0.1), linetype ="solid", 
                 colour="black", width=0.5, size=0.3) +
  scale_fill_brewer(name="Cultivo", palette = "Blues") +
  theme_bw() + facet_wrap(vars(culti), scales = "free_x", nrow=3) +
  ylab (expression(paste('Rendimiento'~(ton~x~ha^-1)))) + 
  xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=9), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")


# Aplicación de análisis de varianza (ANOVA)
library("agricolae")
library ("car")

# inicialmente realizar ANOVA prueba parametrica
hist (cereales.1$rendi, breaks=200, xlab = "Rendimiento")
qqPlot (lm(rendi~culti, data=cereales.1), col.lines="blue2") # diagrama de cuantiles librería car
qqPlot (lm(rendi~depa, data=cereales.1), col.lines="blue2")

qqnorm (cereales.1$rendi) # diagrama de cuantiles librería predeterminada en R
qqline(cereales.1$rendi)

## creación de un conjunto de datos con distribución normal
## Explicacion de una dsitribución norma
normal <- rnorm(200, 50, 2)
hist (normal)
qqnorm (normal)
qqline (normal)
shapiro.test(normal) ## Explicar significado

cereales.aov <- aov (rendi ~ depa*ano*culti, data=cereales.1)
anova (cereales.aov)

#write.table (se.rendi, "rendimiento cereales.txt", sep="\t", dec=",")

## Dia 5
# Analisis por cultivo
arroz <-filter(cereales.1,culti== "ARROZ")
avena <-filter(cereales.1,culti== "AVENA")
cebada <-filter(cereales.1,culti== "CEBADA")
centeno <-filter(cereales.1,culti== "CENTENO")
maiz <-filter(cereales.1,culti== "MAIZ")
maiz.f <-filter(cereales.1,culti== "MAIZ FORRAJERO")
quinua <-filter(cereales.1,culti== "QUINUA")
sorgo <-filter(cereales.1,culti== "SORGO")
trigo <-filter(cereales.1,culti== "TRIGO")

# estadistica inferencial
library ("car")
library ("agricolae")
arroz.aov.a <- aov (ar.semb ~ depa*ano*tamanho_cul, data=arroz)
anova (arroz.aov.a)
HSD.test(arroz.aov.a, "depa",console=TRUE)

arroz.aov.r <- aov (rendi ~ depa*ano*tamanho_cul, data=arroz)
anova (arroz.aov.r)
HSD.test(arroz.aov.r, "depa",console=TRUE)
HSD.test(arroz.aov.r, "tamanho_cul",console=TRUE)

avena.aov.a <- aov (ar.semb ~ depa*ano, data=avena)
anova (avena.aov.a)
HSD.test(avena.aov.a, "depa",console=TRUE)
avena.aov.r <- aov (rendi ~ depa*ano, data=avena)
anova (avena.aov.r)
HSD.test(avena.aov.r, "depa",console=TRUE)

cebada.aov.a <- aov (ar.semb ~ depa*ano, data=cebada)
anova (cebada.aov.a)
HSD.test(cebada.aov.a, "depa",console=TRUE)
cebada.aov.r <- aov (rendi ~ depa*ano, data=cebada)
anova (cebada.aov.r)
HSD.test(cebada.aov.r, "depa",console=TRUE)

centeno.aov.a <- aov (ar.semb ~ depa*ano, data=centeno)
anova (centeno.aov.a)
HSD.test(centeno.aov.a, "depa",console=TRUE)
centeno.aov.r <- aov (rendi ~ depa*ano, data=centeno)
anova (centeno.aov.r)
HSD.test(centeno.aov.r, "depa",console=TRUE)

maiz.aov.a <- aov (ar.semb ~ depa*ano*tamanho_cul, data=maiz)
anova (maiz.aov.a)
HSD.test(maiz.aov.a, "depa",console=TRUE)
maiz.aov.r <- aov (rendi ~ depa*ano*tamanho_cul, data=maiz)
anova (maiz.aov.r)
HSD.test(maiz.aov.r, "depa",console=TRUE, unbalanced=T)

maiz.f.aov.a <- aov (ar.semb ~ depa*anho, data=maiz.f)
anova (maiz.f.aov.a)
HSD.test(maiz.f.aov.a, "depa",console=TRUE)
maiz.f.aov.r <- aov (rendi ~ depa*anho, data=maiz.f)
anova (maiz.f.aov.r)
HSD.test(maiz.f.aov.r, "depa",console=TRUE)

quinua.aov.a <- aov (ar.semb ~ depa*anho, data=quinua)
anova (quinua.aov.a)
HSD.test(quinua.aov.a, "depa",console=TRUE)
quinua.aov.r <- aov (rendi ~ depa*anho, data=quinua)
anova (quinua.aov.r)
HSD.test(quinua.aov.r, "depa",console=TRUE)

sorgo.aov.a <- aov (ar.semb ~ depa*anho, data=sorgo)
anova (sorgo.aov.a)
HSD.test(sorgo.aov.a, "depa",console=TRUE)
sorgo.aov.r <- aov (rendi ~ depa*anho, data=sorgo)
anova (sorgo.aov.r)
HSD.test(sorgo.aov.r, "depa",console=TRUE)

trigo.aov.a <- aov (ar.semb ~ depa*anho, data=trigo)
anova (trigo.aov.a)
HSD.test(trigo.aov.a, "depa",console=TRUE)
trigo.aov.r <- aov (rendi ~ depa*anho, data=trigo)
anova (trigo.aov.r)
HSD.test(trigo.aov.r, "depa",console=TRUE)

# mejores departamentos Huila y Tolima aporte municipios
mejor.arroz <- filter (arroz, depa=="TOLIMA"|depa=="HUILA")
ar.arroz.mejor <- aov(rendi~muni, data=mejor.arroz)
anova (ar.arroz.mejor)
HSD.test(ar.arroz.mejor, "muni",console=TRUE)

source ("Function se.R")
rendi.hui.tol <- summarySE (mejor.arroz, measurevar = "rendi", 
                           groupvars = c("muni","depa"), na.rm = TRUE) 

ggplot(rendi.hui.tol,aes(x=reorder(muni, rendi, mean), 
                      y=rendi, fill=depa)) +
  geom_bar(position = position_dodge(), stat = "identity", width = 0.8, 
           colour = "white") +
  geom_errorbar (aes(ymin=rendi-se, 
                     ymax=rendi+se), 
                 position = position_dodge (0.1), linetype ="solid", 
                 colour="black", width=0.5, size=0.3) +
  theme_bw() +
  scale_fill_brewer(name="Departamento", palette = "Set1") +
  ylab (expression(paste('Rendimiento'~(ton~x~ha^-1)))) + 
  xlab (expression(paste('Municipio'))) +
  theme (axis.title = element_text (size=9), 
         axis.text = element_text(size=7), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "right")

## Comparacion entre cultivos en el dpto preuba parametrica
cundinamarca <- filter (cereales.1, depa=="CUNDINAMARCA")
cundi.aov.a <- aov (ar.semb ~ culti*anho, data=cundinamarca)
anova (cundi.aov.a)
HSD.test(cundi.aov.a, "culti",console=TRUE) ## Test de Tukey

## Prueba no paramétrica
library("FSA")
kruskal.test(ar.semb ~ depa, data=arroz) 
dunnTest(ar.semb~depa, data=arroz, method="holm")

kruskal.test(rendi ~ depa, data=arroz) 
dunnTest(rendi~depa, data=arroz, method="holm")

## Dia 5
## Correlaciones ##
# Funcion correolograma
pairs.cor <- function (x,y,smooth=TRUE, digits=2,  ...)
{
  panel.cor <- function(x, y, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r.obj = cor.test(x, y,use="pairwise",...)
    r = as.numeric(r.obj$estimate)
    p = r.obj$p.value
    mystars <- ifelse(p < .05, "* ", " ")
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(txt, mystars, sep="")
    text(0.5, 0.5, txt)
  }
  panel.hist <- function(x)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan")
  }
  pairs(x,diag.panel=panel.hist,lower.panel=panel.cor,upper.panel=panel.smooth, ...)
} 
###

names (cereales.1)
pairs.cor(cereales.1[,7:10])

ungroup(cereales.1)
cereales.1 <- cereales.1 %>% group_by (depa, culti)
corr.matrix.sum <- summarise_each(cereales.1, 
                    funs(mean(.,na.rm = TRUE)))
pairs.cor(corr.matrix.sum[,7:10])

names (corr.matrix.sum)

# Figura de dispersión por variables usando todas las observaciones
ggplot (cereales.1, aes (x=ar.semb, y=ar.cose)) + 
  geom_point (colour="blue3", size=4, alpha=0.5) + theme_bw() +
  geom_smooth(method = lm,  se=T, linetype= "solid", size = 1, alpha=0.3) +
  xlab (expression (paste ('Area sembrada'~(ha)))) + 
  ylab (expression (paste ('Area cosechada'~(ha)))) +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())


# Figura de dispersión separada por cultivos usando todas las observaciones
ggplot (cereales.1, aes (x=ar.semb, y=rendi)) + 
  geom_point (aes (colour=culti), size=4, alpha=0.5) + theme_bw() +
  geom_smooth(method=lm, se=T, linetype= "dotted", size = 0.5, alpha=0.3) +
  scale_colour_brewer (name="Cultivo", palette="Paired") + 
  facet_wrap(vars(culti), scales="free_y") +
  xlab (expression (paste ('Area sembrada'~(ha)))) + 
  ylab (expression (paste ('rendimiento'~(Ton~x~ha^-1)))) +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())

ggplot (cereales.1, aes (x=ar.semb, y=rendi)) + 
  geom_point (aes (colour=culti), size=4, alpha=1) + theme_bw() +
  geom_smooth(method=lm, se=T, linetype= "dotted", size = 0.5, alpha=0.3) +
  scale_colour_brewer (name="Cultivo", palette="Paired") + 
  xlab (expression (paste ('Area sembrada'~(ha)))) + 
  ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())


# Figura de dispersión separada por cultivos usando promedios
ggplot (corr.matrix.sum, aes (x=ar.semb, y=rendi)) + 
  geom_point (colour="blue3", size=4, alpha=1) + theme_bw() +
  geom_smooth(method=lm, formula = y~log(x), se=T, linetype= "dotted", size = 0.5, alpha=0.3) +
  #scale_colour_brewer (name="Cultivo", palette="Paired") + 
  #facet_wrap(vars(culti), scales="free_y") +
  xlab (expression (paste ('Area sembrada'~(Ton)))) + 
  ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())


# Figura de dispersión separada por cultivos usando promedios
ggplot (corr.matrix.sum, aes (x=ar.semb, y=rendi)) + 
  geom_point (aes (colour=culti), size=4, alpha=1) + theme_bw() +
  geom_smooth(method=lm, se=T, linetype= "dotted", size = 0.5, alpha=0.3) +
  scale_colour_brewer (name="Cultivo", palette="Paired") + 
  facet_wrap(vars(culti), scales="free_y") +
  xlab (expression (paste ('Area sembrada'~(ha)))) + 
  ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())

ggplot (corr.matrix.sum, aes (x=ar.semb, y=produ)) + 
  geom_point (aes (colour=culti), size=4, alpha=1) + theme_bw() +
  geom_smooth(method=lm, se=T, linetype= "dotted", size = 0.5, alpha=0.3) +
  scale_colour_brewer (name="Cultivo", palette="Paired") + 
  facet_wrap(vars(culti), scales="free_y") +
  xlab (expression (paste ('Area sembrada'~(ha)))) + 
  ylab (expression (paste ('Produccion'~(Ton)))) +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())

## correlacion ar sembrada - rendimiento
arroz.prom <- filter (corr.matrix.sum, culti=="ARROZ") 
rcorr (cbind(arroz.prom$ar.semb, arroz.prom$rendi), 
       type=c("pearson"))
## correlacion ar sembrada - produccion
rcorr (cbind(arroz.prom$ar.semb, arroz.prom$produ), 
       type=c("pearson"))

## correlacion ar sembrada - rendimiento
sorgo.prom <- filter (corr.matrix.sum, culti=="SORGO")
rcorr (cbind(sorgo.prom$ar.semb, sorgo.prom$rendi), 
       type=c("pearson"))
## correlacion ar sembrada - produccion
rcorr (cbind(sorgo.prom$ar.semb, sorgo.prom$produ), 
       type=c("pearson"))

maizf.prom <- filter (corr.matrix.sum, culti=="MAIZ FORRAJERO")
rcorr (cbind(maizf.prom$ar.semb, maizf.prom$rendi), 
       type=c("pearson"))

#############
## heatmap ##
#############
library ("car")
library ("gplots")

names (corr.matrix.sum)
corr.matrix.sum$ciclo <- NULL
corr.matrix.sum$grupo <- NULL
corr.matrix.sum$depa <- NULL
corr.matrix.sum$culti <- NULL
corr.matrix.sum$muni <- NULL
corr.matrix.sum$gr.culti <- NULL
corr.matrix.sum$ano <- NULL
corr.matrix.sum$tamanho_cul <- NULL
#corr.matrix.sum <- corr.matrix.sum[complete.cases(corr.matrix.sum),]


corr.matrix <- rcorr(as.matrix (corr.matrix.sum), type="pearson")
corr.matrix
stars.corr <- Recode(corr.matrix$P, "lo:0.001 = '***';0.001:0.01 = '**'; 
                     0.01:0.05 = '*'; else = 'ns';")
stars.corr
pval.corr <- melt(stars.corr)
pval.corr
names (corr.matrix.sum)

ggplot(melt(round(cor(corr.matrix.sum),2)), aes(x=Var1,y=Var2, fill=value)) + 
  geom_tile (colour = "White") + 
  scale_fill_gradientn(colours = redblue(256), name=expression ("r"), 
                       breaks=seq(-1, 1, by = 0.5), limits = c(-1, 1)) + 
  scale_x_discrete(expand = c(0, 0), labels=c("Sembrada", "Cosechada",
                                              "Produccion", "Rendimiento"))  + 
  scale_y_discrete(expand = c(0, 0), labels=c("Sembrada", "Cosechada",
                                              "Produccion", "Rendimiento")) + 
  coord_equal() + theme(axis.title.x = element_text(colour="white"),  
                        axis.text.x  = element_text(angle=270, hjust = 0, 
                                                    colour = "black", size = 10), 
                        axis.title.y = element_text(colour="white"), 
                        axis.text.y = element_text (colour = "black", size =10))+ 
  geom_text (aes(label=value), vjust=-1, size=4) + 
  geom_text (aes(label=pval.corr$value), vjust=1, size=4) +
  theme(legend.text=element_text(size=10))

## Figura de burbujas
names(corr.matrix.sum)
ungroup(cereales.1)
cereales.1 <- cereales.1 %>% group_by (depa, culti)
corr.matrix.sum <- summarise_each(cereales.1, 
                                  funs(mean(.,na.rm = TRUE)))
names(corr.matrix.sum)

ggplot (corr.matrix.sum, aes (x=ar.semb, y=produ, size=rendi, color=culti)) + 
  geom_point (alpha=0.5) + theme_bw() +
  scale_colour_brewer (name="Cultivo", palette="Paired") + 
  scale_size(range = c(0,20), name="Rendimiento") +
  xlab (expression (paste ('Area sembrada'~(Ton)))) + 
  ylab (expression (paste ('Produccion'~(Ton~x~ha^-1)))) +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())

## Diagrama interactivo
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

ungroup(cereales.1)
cereales.1 <- cereales.1 %>% group_by (depa, culti)
corr.matrix.sum <- summarise_each(cereales.1, 
                                  funs(mean(.,na.rm = TRUE)))

figura <- corr.matrix.sum %>%
  mutate(ar.semb=round(ar.semb,2)) %>%
  mutate(produ=round(produ,2)) %>%
  mutate(rendi=round(rendi,2)) %>%
  mutate (text=paste("Departamento:", depa,
                                        "\nCultivo: ", culti,                    
                                       "\nRendimiento: ", rendi,
                                       "\nSembrada: ", ar.semb,
                                       "\nProduccion: ", produ))

fig <- ggplot (figura, aes (x=ar.semb, y=produ, size=rendi, 
                                       color=culti, text=text)) + 
  geom_point (alpha=0.5) + theme_bw() +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  scale_size(range = c(0,20), name="Rendimiento") +
  theme (legend.position="none")

# turn ggplot interactive with plotly
ggplotly(fig, tooltip="text")

## Ejemplo interactivo 2
fig2 <- ggplot(cereales.1, aes(x=reorder(depa, -ar.semb, FUN=mean), y=ar.semb)) +
  geom_boxplot(alpha=1,colour="black",fill="forestgreen") +
  stat_summary(fun.y=mean, geom="point", col="Black", shape = 10, size=5) +
  theme_bw() + #facet_grid(. ~ gr.culti, scales = "free", space = "free") +
  #ylab (expression(paste('Área sembrada'~(ha)))) + 
  #xlab (expression(paste('Departamento'))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=12), 
         axis.text.x = element_text(angle = 90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

ggplotly(fig2)

## Ejemplo interactivo 3
fig3 <- ggplot(prome.anho)+ geom_line(aes (x=anho,y=rendi))+
  geom_point(aes(x = anho, y = rendi), colour = "black", size = 4, shape=1, stroke=1) + #puntos y rellenos
  geom_point(aes(x = anho, y = rendi),colour = "blue", shape=16, size = 4) + #puntos y rellenos # est?tica de la grafica#
  #xlab (expression (paste (''))) +
  #ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme_bw() + scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                                            2014,2015,2016,2017,2018,2019,2020)) +
  theme (strip.text.y = element_text (face="bold", size=12, angle=0), 
         strip.background = element_rect(fill="grey95"),
         axis.title = element_text (size=14), 
         axis.text = element_text(size=14, angle=90), legend.position="",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplotly(fig3)

## Dia 6
####################################################
## Analisis de precios de los productos agricolas ##
####################################################
library ("Hmisc")
library ("dplyr")
library ("reshape2")
library("ggplot2")
library ("RColorBrewer")
library("tidyr")
library("grid") 
library("gridExtra") 

#####################################################
## Uniendo dos bases de datos de diferente origen ##
####################################################
agricola <- read.delim ("agricola.txt", dec=",") 
precios <- read.delim ("precios.txt", dec=",")
precios$culti = toupper(precios$culti) ## cambiar a mayusculas
precios <- precios[complete.cases(precios),]

agricola$depa <- as.factor (agricola$depa) # asignar categoria de factor a las columna
agricola$muni <- as.factor (agricola$muni) # asignar categoria de factor a las columna
agricola$gr.culti <- as.factor (agricola$gr.culti) # asignar categoria de factor a las columna
agricola$culti <- as.factor (agricola$culti) # asignar categoria de factor a las columna
agricola$ar.semb <- as.numeric (agricola$ar.semb) # asignar categoria de variable numerica a las columna
agricola$ar.cose <- as.numeric (agricola$ar.cose) # asignar categoria de variable numerica a las columna
agricola$produ <- as.numeric (agricola$produ) # asignar categoria de variable numerica a las columna
agricola$rendi <- as.numeric (agricola$rendi)

names (precios)

precios$culti <- as.factor (precios$culti) # asignar categoria de factor a las columna
precios$gr.culti <- as.factor (precios$gr.culti) # asignar categoria de factor a las columna
precios$muni <- as.factor (precios$muni) # asignar categoria de factor a las columna
precios$mes <- as.factor (precios$mes) # asignar categoria de factor a las columna
precios$precio <- as.numeric (precios$precio) # asignar categoria de variable numerica a las columna

levels (precios$culti) 
levels (precios$muni)
levels (precios$anho)

precios <- precios %>% group_by (muni,culti,anho) 
precios.prom <- summarise_each(precios, funs(mean(.,na.rm = TRUE)))

agricola.2 <- subset(agricola, culti=="AGUACATE"|culti== "AHUYAMA"|culti=="AJO"|
                    culti=="APIO"|culti=="ARRACACHA"|culti=="ARROZ"|culti=="ARVEJA"|      
                    culti=="BANANO"|culti=="CEBOLLA DE RAMA"|     
                    culti=="CEBOLLA DE BULBO"|              
                    culti=="MAIZ"|culti=="COCO"|                       
                    culti=="FRIJOL"|culti=="FRIJOL"|    
                    culti=="GARBANZO"|culti=="GRANADILLA"|                 
                    culti== "GUAYABA PERA"|culti=="HABICHUELA"|                 
                    culti== "LECHUGA"|culti=="LENTEJA"|          
                    culti== "LIMON COMUN"|culti=="LIMON TAHITI"|               
                    culti=="LULO"|              
                    culti=="MANDARINA"|                 
                    culti=="MANGO TOMMY"|      
                    culti=="MARACUYA"|culti=="MELON"|             
                    culti=="MORA"|culti=="NARANJA"|           
                    culti=="PAPA"|                 
                    culti== "PAPAYA"|culti== "PEPINO"|            
                    culti=="PERA"|culti=="PIMENTON"|                   
                    culti=="PINHA"|culti=="PLATANO"|            
                    culti=="REMOLACHA"|                  
                    culti== "REPOLLO BLANCO"|culti== "TOMATE"|                     
                    culti=="UVA ISABELA"|    
                    culti=="YUCA"|culti=="ZANAHORIA")

agricola.3 <- subset (agricola.2, muni=="BARRANQUILLA"|muni=="BOGOTA"|muni=="BUCARAMANGA"|
                        muni=="CALI"|muni=="CARTAGENA"|muni=="CUCUTA"|muni=="MEDELLIN"|
                        muni=="PEREIRA")
agricola.4 <- subset (agricola.3, anho=="2015"|anho=="2016"|anho=="2017"|
                        anho=="2018"|anho=="2019")


agricola.4 <- agricola.4 %>% group_by (muni,culti,anho) 
agricola.prom <- summarise_each(agricola.4, funs(mean(.,na.rm = TRUE)))
agricola.prom <- agricola.prom [-c(4,5)]
precios.prom <- precios.prom [-c(4,5)]
names (precios.prom)
completa.prom <- full_join(agricola.prom, precios.prom, 
                           by = c("muni", "culti", "anho"))


completa.prom <- completa.prom[complete.cases(completa.prom),]


pairs.cor <- function (x,y,smooth=TRUE, digits=2,  ...)
{
  panel.cor <- function(x, y, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r.obj = cor.test(x, y,use="pairwise",...)
    r = as.numeric(r.obj$estimate)
    p = r.obj$p.value
    mystars <- ifelse(p < .05, "* ", " ")
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(txt, mystars, sep="")
    text(0.5, 0.5, txt)
  }
  panel.hist <- function(x)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan")
  }
  pairs(x,diag.panel=panel.hist,lower.panel=panel.cor,upper.panel=panel.smooth, ...)
} 

names (completa.prom)
pairs.cor(completa.prom[,4:8])

nb.cols <- 14
mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)

ggplot (completa.prom, aes (x=rendi, y=precio, size=ar.semb, color=culti)) + 
  geom_point (alpha=0.5) + theme_bw() +
  scale_colour_manual(name="Cultivo", values=mycolors) + 
  scale_size(range = c(0,20), name="Rendimiento") +
  xlab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) + 
  ylab (expression (paste ('Precio'~(COP~x~kg^-1)))) +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())


names (completa.prom)

tabla <- completa.prom %>%
  mutate(ar.semb=round(ar.semb,2)) %>%
  mutate(ar.cose=round(ar.cose,2)) %>%
  mutate(produ=round(produ,2)) %>%
  mutate(rendi=round(rendi,2)) %>%
  mutate(precio=round(precio,2))  %>% 
  mutate (text=paste("Municipio:", muni,
                     "\nCultivo: ", culti,                    
                    "\nPrecio: ", precio,
                     "\nSembrada: ", ar.semb,
                     "\nProduccion: ", produ,
                     "\nRendimiento: ", rendi))

fig.4 <- ggplot (tabla, aes (x=rendi, y=precio, size=ar.semb, color=culti,
                                     text=text)) + 
  geom_point (alpha=0.5) + theme_bw() +
  scale_colour_manual(name="Cultivo", values=mycolors) + 
  scale_size(range = c(0,20), name="Rendimiento") +
  theme (legend.position="none", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())


# turn ggplot interactive with plotly
ggplotly(fig.4, tooltip = "text")

