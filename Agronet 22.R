############################################
## 1. Actualizacion base de datos agronet ##
############################################
library ("Hmisc")
library ("dplyr")
library ("reshape2")
library("ggplot2")
library ("RColorBrewer")
library("tidyr")
library("grid") 
library("gridExtra") 
library ("viridis")
library ("ggthemes")
library ("gplots")
library ("colorspace")

#getwd()
#setwd("~/Library/CloudStorage/OneDrive-Personal/Documentos/Sebastian/Academia/2. Data science/R projects/Agronet")
#prod.18 <- read.delim ("db.prod.18.txt", dec = ",") # cargar archivo con datos
#prod.23 <- read.delim ("db.prod.23.txt", dec=",")
#prod.23$depa = toupper(prod.23$depa)
#prod.23$muni = toupper(prod.23$muni)
#prod.23$grupo = toupper(prod.23$grupo)
#prod.23$ciclo = toupper(prod.23$ciclo)
#prod.23$culti = toupper(prod.23$culti)

#str(prod.23)
#str(prod.18)

#prod.all <- union_all (prod.18, prod.23)
#write.table (prod.all, "agronet.23.txt", sep="\t", dec=",",
#             row.names=F)
###########

## Actualizacion base de datos de insumos

#getwd()
#setwd("~/Library/CloudStorage/OneDrive-Personal/Documentos/Sebastian/Academia/2. Data science/R projects/Agronet")
#insumo.20 <- read.delim ("insumo.20.txt", dec = ",") # cargar archivo con datos
#insumo.24 <- read.delim ("insumo.24.txt", dec=",")
#names (insumo.20)
#names (insumo.24)

#insumo.20$ano <- as.numeric (insumo.20$ano) 
#insumo.20$precio <- as.numeric (insumo.20$precio)
#insumo.24$ano <- as.numeric (insumo.24$ano) 
#insumo.24$precio <- as.numeric (insumo.24$precio)

#insumo.20$mes = toupper(insumo.20$mes)
#insumo.20$depa = toupper(insumo.20$depa)
#insumo.20$muni = toupper(insumo.20$muni)
#insumo.20$tipo = toupper(insumo.20$tipo)
#insumo.20$nombre = toupper(insumo.20$nombre)

#insumo.24$mes = toupper(insumo.24$mes)
#insumo.24$depa = toupper(insumo.24$depa)
#insumo.24$muni = toupper(insumo.24$muni)
#insumo.24$tipo = toupper(insumo.24$tipo)
#insumo.24$nombre = toupper(insumo.24$nombre)

#str(insumo.20)
#str(insumo.24)

#insumo.all <- union_all (insumo.20, insumo.24)
#write.table (insumo.all, "insumos.all.txt", sep="\t", dec=",", 
#            row.names=F)

## Actualizacion base de datos de precios

#getwd()
#setwd("~/Library/CloudStorage/OneDrive-Personal/Documentos/Sebastian/Academia/2. Data science/R projects/Agronet")
#precio.13 <- read.delim ("precios.13.txt", dec = ",")
#precio.18 <- read.delim ("precios.18.txt", dec=",")
#precio.19 <- read.delim ("precios.19.txt", dec = ",")
#precio.20 <- read.delim ("precios.20.txt", dec=",")
#precio.21 <- read.delim ("precios.21.txt", dec = ",")
#precio.22 <- read.delim ("precios.22.txt", dec=",")
#precio.23 <- read.delim ("precios.23.txt", dec = ",")

#names (precio.13)
#names (precio.18)
#names (precio.19)
#names (precio.20)
#names (precio.21)
#names (precio.22)
#names (precio.23)


#precio.13$ano <- as.numeric (precio.13$ano) 
#precio.13$precio <- as.numeric (precio.13$precio)
#precio.18$ano <- as.numeric (precio.18$ano) 
#precio.18$precio <- as.numeric (precio.18$precio)
#precio.19$ano <- as.numeric (precio.19$ano) 
#precio.19$precio <- as.numeric (precio.19$precio)
#precio.20$ano <- as.numeric (precio.20$ano) 
#precio.20$precio <- as.numeric (precio.20$precio)
#precio.21$ano <- as.numeric (precio.21$ano) 
#precio.21$precio <- as.numeric (precio.21$precio)
#precio.22$ano <- as.numeric (precio.22$ano) 
#precio.22$precio <- as.numeric (precio.22$precio)
#precio.23$ano <- as.numeric (precio.23$ano) 
#precio.23$precio <- as.numeric (precio.23$precio)

#names (precio.13)

#precio.13$mes = toupper(precio.13$mes)
#precio.13$depa = toupper(precio.13$depa)
#precio.13$muni = toupper(precio.13$muni)
#precio.13$grupo = toupper(precio.13$grupo)
#precio.13$culti = toupper(precio.13$culti)

#precio.18$mes = toupper(precio.18$mes)
#precio.18$depa = toupper(precio.18$depa)
#precio.18$muni = toupper(precio.18$muni)
#precio.18$grupo = toupper(precio.18$grupo)
#precio.18$culti = toupper(precio.18$culti)

#precio.19$mes = toupper(precio.19$mes)
#precio.19$depa = toupper(precio.19$depa)
#precio.19$muni = toupper(precio.19$muni)
#precio.19$grupo = toupper(precio.19$grupo)
#precio.19$culti = toupper(precio.19$culti)

#precio.20$mes = toupper(precio.20$mes)
#precio.20$depa = toupper(precio.20$depa)
#precio.20$muni = toupper(precio.20$muni)
#precio.20$grupo = toupper(precio.20$grupo)
#precio.20$culti = toupper(precio.20$culti)

#precio.21$mes = toupper(precio.21$mes)
#precio.21$depa = toupper(precio.21$depa)
#precio.21$muni = toupper(precio.21$muni)
#precio.21$grupo = toupper(precio.21$grupo)
#precio.21$culti = toupper(precio.21$culti)

#precio.22$mes = toupper(precio.22$mes)
#precio.22$depa = toupper(precio.22$depa)
#precio.22$muni = toupper(precio.22$muni)
#precio.22$grupo = toupper(precio.22$grupo)
#precio.22$culti = toupper(precio.22$culti)

#precio.23$mes = toupper(precio.23$mes)
#precio.23$depa = toupper(precio.23$depa)
#precio.23$muni = toupper(precio.23$muni)
#precio.23$grupo = toupper(precio.23$grupo)
#precio.23$culti = toupper(precio.23$culti)

#str(precio.13)
#str(precio.18)
#str(precio.19)
#str(precio.20)
#str(precio.21)
#str(precio.22)
#str(precio.23)

#precio.1 <- union_all (precio.13, precio.18)
#precio.2 <- union_all (precio.19, precio.20)
#precio.3 <- union_all (precio.21, precio.22)
#precio.11 <- union_all (precio.1, precio.2)
#precio.22 <- union_all (precio.3, precio.23)
#precio.all <- union_all (precio.11, precio.22)

#write.table (precio.all, "precios.all.txt", sep="\t", dec=",", 
            #row.names=F)


#######################
## Datos agronomicos ##
#######################
#prod.22 <- read.delim ("agronet.22.txt", dec=",")
prod.23 <- read.delim ("agronet.23.txt", dec=",")
str (prod.23) 
names (prod.23) 
prod.23$depa <- as.factor (prod.23$depa) 
prod.23$muni <- as.factor (prod.23$muni) 
prod.23$ciclo <- as.factor (prod.23$ciclo) 
prod.23$grupo <- as.factor (prod.23$grupo) 
prod.23$culti <- as.factor (prod.23$culti) 
#
prod.23$ar.semb <- as.numeric (prod.23$ar.semb) 
prod.23$ar.cose <- as.numeric (prod.23$ar.cose) 
prod.23$produ <- as.numeric (prod.23$produ) 
prod.23$rendi <- as.numeric (prod.23$rendi) 

str (prod.23) 

levels (prod.23$grupo)
levels (prod.23$depa)
levels (prod.23$culti)

names (prod.23)
levels (prod.23$depa)
levels (prod.23$depa) <- c("AMAZ","ANTI","ARAU","SAYP","ATLA",               
                           "BOLI","BOYA","CALD","CAQU",                 
                           "CASA","CAUC","CESA","CHOC",                   
                           "CORD","CUNDI","GUAI","GUAV",                
                           "HUIL","GUAJ","MAGD","META",
                           "NARI","NOSA","PUTU","QUIN",                 
                           "RISA","SAYP",
                           "SANT","SUCR","TOLI","VALL",         
                           "VAUP", "VICH")


##############################################################
# Analisis general del Magdalena medio cultivos transitorios #
##############################################################

#magda.medio <- filter(prod.23, muni=="CARACOLI"| 
#                        muni=="MACEO"|muni=="PUERTO BERRIO"|muni=="PUERTO NARE"|
#                        muni=="PUERTO TRIUNFO"|muni=="YONDO"|muni=="ARENAL"| 
#                        muni=="CANTAGALLO"|muni=="MORALES"|muni=="SAN PABLO"|
#                        muni=="SANTA ROSA DEL SUR"|muni=="SIMITI"|muni=="AGUACHICA"| 
#                        muni=="GAMARRA"|muni=="LA GLORIA"|muni=="PELAYA"|
#                        muni=="SAN ALBERTO"|muni=="SAN MARTIN"|muni=="RIO DE ORO"| 
#                        muni=="BARRANCABERMEJA"|muni=="BETULIA"|
#                        muni=="BETULIA"|muni=="CIMITARRA"|muni=="EL CARMEN DE CHUCURI"|
#                        muni=="EL PEÑON"|muni=="LANDAZURI"|muni=="PUERTO PARRA"|muni=="PUERTO WILCHES"| 
#                        muni=="SAN VICENTE DE CHUCURI"|muni=="SIMACOTA"|muni=="PUERTO BOYACA")
#transi.magda <- filter (magda.medio, ciclo =="TRANSITORIO")

#magda.2023 <- filter (transi.magda, ano==2023)

#sembrada.2023 <- aggregate(magda.2023$ar.semb, by=list(cultivo=magda.2023$culti), 
#                           FUN=sum)
#toneladas.2023 <- aggregate(magda.2023$produ, by=list(cultivo=magda.2023$culti), 
#                            FUN=sum)
#rendimiento.2023 <- aggregate(magda.2023$rendi, by=list(cultivo=magda.2023$culti), 
#                              FUN=sum)

#######################################################
## Analisis de la diversidad de cultivos en el valle ##
#######################################################
names (prod.23)
valle <- filter(prod.23, depa=="VALLE DEL CAUCA")
valle.2023 <- filter (valle, ano==2023)
valle.2023$culti <- as.factor (valle.2023$culti)
levels (valle.2023$culti)
valle.2023 <- group_by()
ungroup(valle.2023)

valle.cant <- valle.2023 %>%
  group_by(muni) %>%
  summarise(count= n_distinct (culti))

#######################################################
## Analisis de la diversidad de cultivos en colombia ##
#######################################################
names (prod.23)
levels (prod.23$depa)
levels (prod.23$depa) <- c("AMAZ","ANTI","ARAU","SAYP","ATLA",               
                        "BOLI","BOYA","CALD","CAQU",                 
                        "CASA","CAUC","CESA","CHOC",                   
                        "CORD","CUNDI","GUAI","GUAV",                
                        "HUIL","GUAJ","MAGD","META",
                        "NARI","NOSA","PUTU","QUIN",                 
                        "RISA","SAYP",
                        "SANT","SUCR","TOLI","VALL",         
                        "VAUP", "VICH")
col.cant <- prod.23 %>%
  group_by(depa, ano) %>%
  summarise(count= n_distinct (culti))

ungroup (prod.23)
col.prom <-prod.23 %>%
  group_by(depa, ano) %>%
  summarise_each(funs(mean(.,na.rm = TRUE)))

col.tot <- aggregate(prod.23$produ, by=list(depa=prod.23$depa, ano=prod.23$ano), 
                            FUN=sum)

names (col.prom)
col.prom <- col.prom [-c(3:6)]
names (col.prom)


diversidad <- full_join(col.prom, col.cant, 
                           by = c("depa", "ano"))

diversidad.2 <- full_join(diversidad, col.tot, 
                        by = c("depa", "ano"))

#############
## heatmap ##
#############
library (car)
library (gplots)

names (diversidad)
diversidad$depa <- NULL
diversidad$ano <- NULL

diversidad <- diversidad[complete.cases(diversidad),]


corr.matrix <- rcorr(as.matrix (diversidad), type="pearson")
corr.matrix
stars.corr <- Recode(corr.matrix$P, "lo:0.001 = '***';0.001:0.01 = '**'; 
                     0.01:0.05 = '*'; else = 'ns';")
stars.corr
pval.corr <- melt(stars.corr)
pval.corr

names(diversidad)

ggplot(melt(round(cor(diversidad),2)), aes(x=Var1,y=Var2, fill=value)) + 
  geom_tile (colour = "White") + 
  scale_fill_gradientn(colours = redblue(256), name=expression ("r"), 
                       breaks=seq(-1, 1, by = 0.5), limits = c(-1, 1)) + 
  scale_x_discrete(expand = c(0, 0), labels=c("Sembrada", "Cosechada",
                                              "Produccion", "Rendimiento", "Cantidad"))  + 
  scale_y_discrete(expand = c(0, 0), labels=c("Sembrada", "Cosechada",
                                              "Produccion", "Rendimiento", "Cantidad")) + 
  coord_equal() + theme(axis.title.x = element_text(colour="white"),  
                        axis.text.x  = element_text(angle=270, hjust = 0, 
                                                    colour = "black", size = 10), 
                        axis.title.y = element_text(colour="white"), 
                        axis.text.y = element_text (colour = "black", size =10))+ 
  geom_text (aes(label=value), vjust=-1, size=4) + 
  geom_text (aes(label=pval.corr$value), vjust=1, size=4) +
  theme(legend.text=element_text(size=10))


## Figura 1 cantidad de especies sembradas por departamento
diversidad <- full_join(col.prom, col.cant, 
                        by = c("depa", "ano"))
names (diversidad)
ggplot(diversidad, aes(ano, depa)) +
  geom_tile(aes(fill = count)) + 
  scale_fill_gradientn(colours = rev(sequential_hcl (120, "YlGnBu")), 
                       name=expression ("Number of crops"), 
                       breaks=seq(0, 120, by = 20), limits = c(1, 120)) +
  coord_equal() + theme_minimal () +
  scale_x_continuous(expand = c(0, 0), breaks=c(2006,2007,2008,2009,
                                                2010,2011,2012,2013,2014,2015,2016,2017,
                                                2018,2019,2020, 2021,2022,2023))  + 
  scale_y_discrete(expand = c(0, 0)) + 
  theme(axis.title.x = element_text(colour="white"),  
        axis.text.x  = element_text(angle=270, hjust = 0, 
                                    colour = "black", size = 10), 
        axis.title.y = element_text(colour="white"), 
        axis.text.y = element_text (colour = "black", size =10),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


## Figura 2 correlacion cantidad de especies productividad

names (diversidad)

# Correlacion diversidad rendimiento promedio
rcorr (cbind(diversidad$count, diversidad$rendi), 
       type=c("pearson"))
f1 <- ggplot (diversidad, aes (x=count, y=rendi)) + 
  geom_point (alpha=0.5, colour="dodgerblue4", size=3) + theme_bw() +
  geom_smooth(method=lm, se=F, linetype= "solid", size = 0.5, 
              alpha=0.5, colour="grey40") +
  xlab (expression (paste ('Number of crops per location'))) + 
  ylab (expression (paste ('Yield'~(Ton~x~ha^-1)))) +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank()) +
  annotate (geom="text", x=95, y=13,
            label = "r = 0.55***", 
            size = 4, fontface="italic")

# Correlacion diversidad total toneladas producidas
diversidad.3 <- filter (diversidad.2, x < 16614225) 
rcorr (cbind(diversidad.3$count, diversidad.3$x), 
       type=c("pearson"))

f2 <- ggplot (diversidad.3, aes (x=count, y=x/1000000)) + 
  geom_point (alpha=0.4, colour="dodgerblue4", size=3) + theme_bw() +
  geom_smooth(method=lm, se=F, linetype= "solid", size = 0.5, 
              alpha=0.5, colour="grey40") +
  xlab (expression (paste ('Number of crops per location'))) + 
  ylab (expression (paste ('Total Crop Production'~(Million~Tons)))) +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())+ 
  annotate (geom="text", x=105, y=3.8,
            label = "r = 0.72***", 
            size = 4, fontface="italic")


grid.arrange(f1,f2, 
             layout_matrix = rbind(c(1,2)))

## Mejores departamentos

best <- filter (diversidad.2, depa=="BOYA"| depa=="CUNDI"|
                depa=="ANTI"| depa=="SANT"|depa=="NARI"| 
                  depa=="VALL")

ggplot(best, aes(x=reorder(depa, rendi, mean), 
                 y=rendi)) + 
  geom_boxplot(color="black", fill="dodgerblue3", alpha=1, 
               outlier.size = 1) +
  stat_summary(fun.y=mean, geom="point", col="Black", 
               shape = 10, size=3, position=position_dodge(width = 0.7)) +
  theme_bw() + 
  ylab (expression(paste('Yield'~(Ton~x~ha^-1)))) + 
  xlab (expression(paste(''))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=12), 
         axis.text.x = element_text(size=12, angle=90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

ggplot(diversidad.2, aes(x=reorder(depa, rendi, mean), 
                 y=rendi)) + 
  geom_boxplot(color="black", fill="dodgerblue3", alpha=1, 
               outlier.size = 1) +
  stat_summary(fun.y=mean, geom="point", col="Black", 
               shape = 10, size=3, position=position_dodge(width = 0.7)) +
  theme_bw() + 
  ylab (expression(paste('Yield'~(Ton~x~ha^-1)))) + 
  xlab (expression(paste(''))) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=12), 
         axis.text.x = element_text(size=12, angle=90),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")


######################
## Datos de insumos ##
######################
insumos.all <- read.delim ("insumos.all.txt", dec=",")
str (insumos.all)
names (insumos.all) 

insumos.all$ano <- as.factor (insumos.all$ano)
insumos.all$mes <- as.factor (insumos.all$mes)
insumos.all$depa <- as.factor (insumos.all$depa) 
insumos.all$muni <- as.factor (insumos.all$muni) 
insumos.all$tipo <- as.factor (insumos.all$tipo) 
insumos.all$nombre <- as.factor (insumos.all$nombre) 
insumos.all$precio <- as.numeric (insumos.all$precio) 

levels (insumos.all$depa)
levels (insumos.all$tipo)

levels (insumos.all$depa) <- c("ANTI","ARAU","ATLA", "CUNDI",               
                           "BOLI","BOYA","CALD","CAQU",                 
                           "CASA","CAUC","CESA",                   
                           "CORD","CUNDI","",                
                           "HUIL","MAGD","META",
                           "NARI","NOSA","PUTU","QUIN",                 
                           "RISA",
                           "SANT","SUCR","TOLI","VALL")

arren <- filter (insumos.all, tipo=="ARRENDAMIENTO")
coad <- filter (insumos.all, tipo=="COADYUVANTE")
elem <- filter (insumos.all, tipo=="ELEMENTOS")
ferti <- filter (insumos.all, tipo=="FERTILIZANTE")
fungi <- filter (insumos.all, tipo=="FUNGICIDA")
herbi <- filter (insumos.all, tipo=="HERBICIDA")
insecti <- filter (insumos.all, tipo=="INSECTICIDA")
riego <- filter (insumos.all, tipo=="RIEGO")

source ("Function outliers.r")

## arrendamiento
names(arren)
precios.a <- as.data.frame(remove_outliers (arren$precio))
arren.f <- select (arren, ano,mes,depa,muni,tipo, nombre)
arren.1 <- bind_cols (arren.f, precios.a)

names (arren.1)
names (arren.1) [7] <- paste ("precio")

arren.1 <- arren.1[complete.cases(arren.1),] 

ggplot (arren.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## coadyuvantes
names(coad)
precios.c <- as.data.frame(remove_outliers (coad$precio))
coad.f <- select (coad, ano,mes,depa,muni,tipo, nombre)
coad.1 <- bind_cols (coad.f, precios.c)

names (coad.1)
names (coad.1) [7] <- paste ("precio")

coad.1 <- coad.1[complete.cases(coad.1),] 

ggplot (coad.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## elementos
names(elem)
precios.e <- as.data.frame(remove_outliers (elem$precio))
elem.f <- select (elem, ano,mes,depa,muni,tipo, nombre)
elem.1 <- bind_cols (elem.f, precios.e)

names (elem.1)
names (elem.1) [7] <- paste ("precio")

elem.1 <- elem.1[complete.cases(elem.1),] 

ggplot (elem.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## fertilizantes
names(ferti)
precios.f <- as.data.frame(remove_outliers (ferti$precio))
ferti.f <- select (ferti, ano,mes,depa,muni,tipo, nombre)
ferti.1 <- bind_cols (ferti.f, precios.f)

names (ferti.1)
names (ferti.1) [7] <- paste ("precio")

ferti.1 <- ferti.1[complete.cases(ferti.1),] 

ggplot (ferti.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## fungicidas
names(fungi)
precios.fu <- as.data.frame(remove_outliers (fungi$precio))
fungi.f <- select (fungi, ano,mes,depa,muni,tipo, nombre)
fungi.1 <- bind_cols (fungi.f, precios.fu)

names (fungi.1)
names (fungi.1) [7] <- paste ("precio")

fungi.1 <- fungi.1[complete.cases(fungi.1),] 

ggplot (fungi.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## herbicidas
names(herbi)
precios.h <- as.data.frame(remove_outliers (herbi$precio))
herbi.f <- select (herbi, ano,mes,depa,muni,tipo, nombre)
herbi.1 <- bind_cols (herbi.f, precios.h)

names (herbi.1)
names (herbi.1) [7] <- paste ("precio")

herbi.1 <- herbi.1[complete.cases(herbi.1),] 

ggplot (herbi.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## insecti
names(insecti)
precios.i <- as.data.frame(remove_outliers (insecti$precio))
insecti.f <- select (insecti, ano,mes,depa,muni,tipo, nombre)
insecti.1 <- bind_cols (insecti.f, precios.i)

names (insecti.1)
names (insecti.1) [7] <- paste ("precio")

insecti.1 <- insecti.1[complete.cases(insecti.1),] 

ggplot (insecti.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## riego
names(riego)
precios.r <- as.data.frame(remove_outliers (riego$precio))
riego.f <- select (riego, ano,mes,depa,muni,tipo, nombre)
riego.1 <- bind_cols (riego.f, precios.r)

names (riego.1)
names (riego.1) [7] <- paste ("precio")

riego.1 <- riego.1[complete.cases(riego.1),] 

ggplot (riego.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

#############################
## Datos precios commodity ##
#############################
precios.all <- read.delim ("precios.all.txt", dec=",")
str (precios.all)
names (precios.all) 

precios.all$ano <- as.factor (precios.all$ano)
precios.all$mes <- as.factor (precios.all$mes)
precios.all$depa <- as.factor (precios.all$depa) 
precios.all$muni <- as.factor (precios.all$muni) 
precios.all$grupo <- as.factor (precios.all$grupo) 
precios.all$culti <- as.factor (precios.all$culti) 
precios.all$precio <- as.numeric (precios.all$precio) 

levels (precios.all$depa)
levels (precios.all$grupo)
levels (precios.all$culti)

levels (precios.all$depa) <- c("ANTI","ARAU","ATLA",               
                               "BOLI","BOYA","CALD","CAQU",                 
                               "CASA","CAUC","CESA",                   
                               "CORD","CUNDI",                
                               "HUIL","MAGD","MAGD","META",
                               "NARI","NOSA","QUIN",                 
                               "RISA",
                               "SANT","SUCR","TOLI","VALL")

pre.fru <- filter (precios.all, grupo=="FRUTAS")
pre.cer <- filter (precios.all, grupo=="GRANOS Y CEREALES")
pre.rai <- filter (precios.all, grupo=="TUBERCULOS, RAICES Y PLATANOS")
pre.hor <- filter (precios.all, grupo=="VERDURAS Y HORTALIZAS")

source ("Function outliers.r")

## precio frutas
names(pre.fru)
precios.pf <- as.data.frame(remove_outliers (pre.fru$precio))
pre.fru.f <- select (pre.fru, ano,mes,depa,muni,grupo,culti)
pre.fru.1 <- bind_cols (pre.fru.f, precios.pf)

names (pre.fru.1)
names (pre.fru.1) [7] <- paste ("precio")

pre.fru.1 <- pre.fru.1[complete.cases(pre.fru.1),] 

ggplot (pre.fru.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## precio cereales
names(pre.cer)
precios.pc <- as.data.frame(remove_outliers (pre.cer$precio))
pre.cer.f <- select (pre.cer, ano,mes,depa,muni,grupo,culti)
pre.cer.1 <- bind_cols (pre.cer.f, precios.pc)

names (pre.cer.1)
names (pre.cer.1) [7] <- paste ("precio")

pre.cer.1 <- pre.cer.1[complete.cases(pre.cer.1),] 

ggplot (pre.cer.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## precio raices
names(pre.rai)
precios.pr <- as.data.frame(remove_outliers (pre.rai$precio))
pre.rai.f <- select (pre.rai, ano,mes,depa,muni,grupo,culti)
pre.rai.1 <- bind_cols (pre.rai.f, precios.pr)

names (pre.rai.1)
names (pre.rai.1) [7] <- paste ("precio")

pre.rai.1 <- pre.rai.1[complete.cases(pre.rai.1),] 

ggplot (pre.rai.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())


## precio hortalizas
names(pre.hor)
precios.ph <- as.data.frame(remove_outliers (pre.hor$precio))
pre.hor.f <- select (pre.hor, ano,mes,depa,muni,grupo,culti)
pre.hor.1 <- bind_cols (pre.hor.f, precios.ph)

names (pre.hor.1)
names (pre.hor.1) [7] <- paste ("precio")

pre.hor.1 <- pre.hor.1[complete.cases(pre.hor.1),] 

ggplot (pre.hor.1, aes(x="", y=precio)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('precio'~(COP)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

#################################################
## Analisis exploratorio del conjunto de datos ##
#################################################

names (prod.23)
levels (prod.23$grupo)
levels (prod.23$grupo) <- c("CERE","ARO.CON.MED","TROP.TRAD","FIBRAS", 
                            "FLOR.FOLL","FOREST","FRUTA","HONGOS",
                            "HORTA","LEGU","OLEA","O.PERM","O.TRANS",
                            "ARO.CON.MED","RAI.TUBER", "RAI.TUBER")

## Distribucion de las variables en el conjunto de datos completos
ggplot (prod.23, aes(x=grupo, y=ar.semb)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot (prod.23, aes(x=grupo, y=rendi)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

 
cere <- filter (prod.23, grupo=="CERE")
aroma <- filter (prod.23, grupo=="ARO.CON.MED")
tropico <- filter (prod.23, grupo=="TROP.TRAD")
fibras <- filter (prod.23, grupo=="FIBRAS")
flor <- filter (prod.23, grupo=="FLOR.FOLL")
forest <- filter (prod.23, grupo=="FOREST")
fruta <- filter (prod.23, grupo=="FRUTA")
hongos <- filter (prod.23, grupo=="HONGOS")
horta <- filter (prod.23, grupo=="HORTA")
legum <- filter (prod.23, grupo=="LEGU")
olea <- filter (prod.23, grupo=="OLEA")
perma <- filter (prod.23, grupo=="O.PERM")
transi <- filter (prod.23, grupo=="O.TRANS")
raiz <- filter (prod.23, grupo=="RAI.TUBER")

## Set de datos eliminando outliers ##

source ("Function outliers.r") ## outliers function

## cereales
names(cere)
ar.semb.c <- as.data.frame(remove_outliers (cere$ar.semb))
ar.cose.c <- as.data.frame(remove_outliers (cere$ar.cose))
produ.c <- as.data.frame(remove_outliers (cere$produ))
rendi.c <- as.data.frame(remove_outliers (cere$rendi))

cere.f <- select (cere, depa,muni,grupo,culti,ano)
cere.1 <- bind_cols (cere.f, ar.semb.c, 
                         ar.cose.c, produ.c, rendi.c)

names (cere.1)
names (cere.1) [6] <- paste ("ar.semb")
names (cere.1) [7] <- paste ("ar.cose")
names (cere.1) [8] <- paste ("produ")
names (cere.1) [9] <- paste ("rendi")

cere.1 <- cere.1[complete.cases(cere.1),] 

ggplot (cere.1, aes(x="", y=ar.semb)) + 
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  stat_summary(fun.y=mean, geom="point", col="black", shape = 10, size=4) +
  theme_bw() + ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot (cere.1, aes(x="", y=rendi)) + 
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.5) + 
  stat_summary(fun.y=mean, geom="point", col="black", shape = 10, size=4) +
  theme_bw() + ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## frutales
names(fruta)
ar.semb.f <- as.data.frame(remove_outliers (fruta$ar.semb))
ar.cose.f <- as.data.frame(remove_outliers (fruta$ar.cose))
produ.f <- as.data.frame(remove_outliers (fruta$produ))
rendi.f <- as.data.frame(remove_outliers (fruta$rendi))

fruta.f <- select (fruta, depa,muni,grupo,culti,ano)
fruta.1 <- bind_cols (fruta.f, ar.semb.f, 
                     ar.cose.f, produ.f, rendi.f)

names (fruta.1)
names (fruta.1) [6] <- paste ("ar.semb")
names (fruta.1) [7] <- paste ("ar.cose")
names (fruta.1) [8] <- paste ("produ")
names (fruta.1) [9] <- paste ("rendi")

fruta.1 <- fruta.1[complete.cases(fruta.1),] 

ggplot (fruta.1, aes(x="", y=ar.semb)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot (fruta.1, aes(x="", y=rendi)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## hortalizas
names(horta)
ar.semb.h <- as.data.frame(remove_outliers (horta$ar.semb))
ar.cose.h <- as.data.frame(remove_outliers (horta$ar.cose))
produ.h <- as.data.frame(remove_outliers (horta$produ))
rendi.h <- as.data.frame(remove_outliers (horta$rendi))

horta.f <- select (horta, depa,muni,grupo,culti,ano)
horta.1 <- bind_cols (horta.f, ar.semb.h, 
                      ar.cose.h, produ.h, rendi.h)

names (horta.1)
names (horta.1) [6] <- paste ("ar.semb")
names (horta.1) [7] <- paste ("ar.cose")
names (horta.1) [8] <- paste ("produ")
names (horta.1) [9] <- paste ("rendi")

horta.1 <- horta.1[complete.cases(horta.1),] 

ggplot (horta.1, aes(x="", y=ar.semb)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot (horta.1, aes(x="", y=rendi)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## leguminosas
names(legum)
ar.semb.l <- as.data.frame(remove_outliers (legum$ar.semb))
ar.cose.l <- as.data.frame(remove_outliers (legum$ar.cose))
produ.l <- as.data.frame(remove_outliers (legum$produ))
rendi.l <- as.data.frame(remove_outliers (legum$rendi))

legum.f <- select (legum, depa,muni,grupo,culti,ano)
legum.1 <- bind_cols (legum.f, ar.semb.l, 
                      ar.cose.l, produ.l, rendi.l)

names (legum.1)
names (legum.1) [6] <- paste ("ar.semb")
names (legum.1) [7] <- paste ("ar.cose")
names (legum.1) [8] <- paste ("produ")
names (legum.1) [9] <- paste ("rendi")

legum.1 <- legum.1[complete.cases(legum.1),] 

ggplot (legum.1, aes(x="", y=ar.semb)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot (legum.1, aes(x="", y=rendi)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## leguminosas
names(legum)
ar.semb.l <- as.data.frame(remove_outliers (legum$ar.semb))
ar.cose.l <- as.data.frame(remove_outliers (legum$ar.cose))
produ.l <- as.data.frame(remove_outliers (legum$produ))
rendi.l <- as.data.frame(remove_outliers (legum$rendi))

legum.f <- select (legum, depa,muni,grupo,culti,ano)
legum.1 <- bind_cols (legum.f, ar.semb.l, 
                      ar.cose.l, produ.l, rendi.l)

names (legum.1)
names (legum.1) [6] <- paste ("ar.semb")
names (legum.1) [7] <- paste ("ar.cose")
names (legum.1) [8] <- paste ("produ")
names (legum.1) [9] <- paste ("rendi")

legum.1 <- legum.1[complete.cases(legum.1),] 

ggplot (legum.1, aes(x="", y=ar.semb)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot (legum.1, aes(x="", y=rendi)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

## oleaginosas
names(olea)
ar.semb.o <- as.data.frame(remove_outliers (olea$ar.semb))
ar.cose.o <- as.data.frame(remove_outliers (olea$ar.cose))
produ.o <- as.data.frame(remove_outliers (olea$produ))
rendi.o <- as.data.frame(remove_outliers (olea$rendi))

olea.f <- select (olea, depa,muni,grupo,culti,ano)
olea.1 <- bind_cols (olea.f, ar.semb.o, 
                      ar.cose.o, produ.o, rendi.o)

names (olea.1)
names (olea.1) [6] <- paste ("ar.semb")
names (olea.1) [7] <- paste ("ar.cose")
names (olea.1) [8] <- paste ("produ")
names (olea.1) [9] <- paste ("rendi")

olea.1 <- olea.1[complete.cases(olea.1),] 

ggplot (olea.1, aes(x="", y=ar.semb)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot (olea.1, aes(x="", y=rendi)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())


## raices
names(raiz)
ar.semb.r <- as.data.frame(remove_outliers (raiz$ar.semb))
ar.cose.r <- as.data.frame(remove_outliers (raiz$ar.cose))
produ.r <- as.data.frame(remove_outliers (raiz$produ))
rendi.r <- as.data.frame(remove_outliers (raiz$rendi))

raiz.f <- select (raiz, depa,muni,grupo,culti,ano)
raiz.1 <- bind_cols (raiz.f, ar.semb.r, 
                     ar.cose.r, produ.r, rendi.r)

names (raiz.1)
names (raiz.1) [6] <- paste ("ar.semb")
names (raiz.1) [7] <- paste ("ar.cose")
names (raiz.1) [8] <- paste ("produ")
names (raiz.1) [9] <- paste ("rendi")

raiz.1 <- raiz.1[complete.cases(raiz.1),] 

ggplot (raiz.1, aes(x="", y=ar.semb)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Area sembrada'~(ha)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot (raiz.1, aes(x="", y=rendi)) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 16, size=4) +
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  theme_bw() + ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())


#######################################
## Evaluacion por grupos de cultivos ##
######################################
## Capitulo Cereales

ggplot (cere.1, aes(x=reorder(depa, ar.semb, mean), y=ar.semb)) + 
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  stat_summary(fun.y=mean, geom="point", col="black", shape = 10, size=3) +
  theme_bw() + ylab (expression (paste ('Area sembrada'~(ha)))) +
  xlab (expression (paste ('Departamento'))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

ggplot (cere.1, aes(x=reorder(depa, rendi, mean), y=rendi)) + 
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  stat_summary(fun.y=mean, geom="point", col="black", shape = 10, size=3) +
  theme_bw() + ylab (expression (paste ('Rendimiento'~(Ton~x~ha^-1)))) +
  xlab (expression (paste ('Departamento'))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

c1 <- ggplot (cere.1, aes(x=as.factor(cere.1$ano), y=ar.semb)) + 
  geom_boxplot(colour="black", fill="dodgerblue", alpha=0.8) + 
  stat_summary(fun.y=mean, geom="point", col="black", shape = 10, size=3) +
  theme_bw() + ylab (expression (paste ('Cultivated area'~(ha)))) +
  xlab (expression (paste (''))) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=0), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

c2 <- ggplot (cere.1, aes(x=as.factor(cere.1$ano), y=rendi)) + 
  geom_boxplot(colour="black", fill="dodgerblue", alpha=0.8) + 
  stat_summary(fun.y=mean, geom="point", col="black", shape = 10, size=3) +
  theme_bw() + ylab (expression (paste ('Yield'~(Ton~x~ha^-1)))) +
  xlab (expression (paste (''))) +
  theme (axis.title = element_text (size=0), 
         axis.text = element_text(size=10, angle=0), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

c3 <- ggplot (cere.1, aes(x=reorder(culti, ar.semb, mean), y=ar.semb)) + 
  geom_boxplot(colour="black", fill="dodgerblue", alpha=0.8) + 
  stat_summary(fun.y=mean, geom="point", col="black", shape = 10, size=3) +
  theme_bw() + ylab (expression (paste ('Cultivated area'~(ha)))) +
  xlab (expression (paste ('Crop'))) +
  scale_x_discrete (breaks=c("ARROZ", "AVENA", "CEBADA", "CENTENO", 
                             "MAIZ", "MAIZ FORRAJERO", "MILLO", "QUINUA", "SORGO",
                             "TRIGO"),
                    labels=c("Rice", "Oat", "Barley", "Rye", 
                             "Maize", "Forage maize", "Millet", "Quinoa", "Sorghum",
                             "Wheat")) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=0), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

c4 <- ggplot (cere.1, aes(x=reorder(culti, rendi, mean), y=rendi)) + 
  geom_boxplot(colour="black", fill="dodgerblue", alpha=0.8) + 
  stat_summary(fun.y=mean, geom="point", col="black", shape = 10, size=3) +
  theme_bw() + ylab (expression (paste ('Yield'~(ton~x~ha^-1)))) +
  xlab (expression (paste ('Crop'))) +
  scale_x_discrete (breaks=c("ARROZ", "AVENA", "CEBADA", "CENTENO", 
                             "MAIZ", "MAIZ FORRAJERO", "MILLO", "QUINUA", "SORGO",
                             "TRIGO"),
                    labels=c("Rice", "Oat", "Barley", "Rye", 
                             "Maize", "Forage maize", "Millet", "Quinoa", "Sorghum",
                             "Wheat")) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=0), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())



grid.arrange(c1,c2, layout_matrix = rbind(c(1,1), c(2,2)))
grid.arrange(c3,c4, layout_matrix = rbind(c(1,1), c(2,2)))

summary (cere.1$ar.semb)
summary (cere.1$rendi)
aggregate(cere.1$rendi, by=list(anho=cere.1$ano), 
                              FUN=mean)

aggregate(cere.1$ar.semb, by=list(anho=cere.1$ano), 
          FUN=mean)

aggregate(cere.1$rendi, by=list(cultivo=cere.1$culti), 
          FUN=mean)

aggregate(cere.1$ar.semb, by=list(anho=cere.1$culti), 
          FUN=quantile)


## Aporte de los departamentos 
source ("Function se.r")
source ("Figure function.r")

names (cere.1)

## Area sembrada
cere.arsem <- summarySE (cere.1, measurevar = "ar.semb", 
                         groupvars = c("depa"), na.rm = TRUE)

names (cere.arsem)

c5 <- ggplot(cere.arsem, aes(x=reorder(depa, ar.semb, mean), 
                             y=ar.semb)) + 
  geom_bar(position = position_dodge(), stat = "identity", 
           width = 0.7, colour = "black", fill="dodgerblue") +
  geom_errorbar (aes(ymin=cere.arsem$ar.semb-cere.arsem$se, 
                     ymax=cere.arsem$ar.semb+cere.arsem$se), 
                 position = position_dodge (0.1), 
                 linetype ="solid", colour="black",
                 size=0.3, width=0.5) +
  theme_bw() + 
  ylab (expression(paste('Cultivated area'~(ha)))) + 
  xlab (expression(paste('Location'))) +
  theme (axis.title = element_text (size=10), 
         axis.text.x = element_text(size=10, angle=90),
         axis.text.y = element_text(size=10),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")


## Rendimiento
cere.rendi <- summarySE (cere.1, measurevar = "rendi", 
                         groupvars = c("depa"), na.rm = TRUE)

names (cere.rendi)

c6 <- ggplot(cere.rendi, aes(x=reorder(depa, rendi, mean), 
                             y=rendi)) + 
  geom_bar(position = position_dodge(), stat = "identity", 
           width = 0.7, colour = "black", fill="dodgerblue") +
  geom_errorbar (aes(ymin=cere.rendi$rendi-cere.rendi$se, 
                     ymax=cere.rendi$rendi+cere.rendi$se), 
                 position = position_dodge (0.1), 
                 linetype ="solid", colour="black",
                 size=0.3, width=0.5) +
  theme_bw() + 
  ylab (expression(paste('Yield'~(ton~x~ha^-1)))) + 
  xlab (expression(paste('Location'))) +
  theme (axis.title = element_text (size=10), 
         axis.text.x = element_text(size=10, angle=90),
         axis.text.y = element_text(size=10),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")


grid.arrange(c5,c6, layout_matrix = rbind(c(1,1), c(2,2)))

cere.depa.arsem <- summarySE (cere.1, measurevar = "ar.semb", 
                         groupvars = c("depa", "culti"), na.rm = TRUE)

## Representatividad de cereales por depa
## Elaboración de treemap
library (treemap)
names (cere.1)
table <- as.data.frame (xtabs(~culti+depa, cere.1))
table$Freq <- as.numeric (table$Freq)
vec <- table$Freq!=0
table <- table[vec,]
dev.new(width=10, height=10)
treemap(table,
        index=c("culti","depa"), 
        vSize="Freq",
        type="index", border.col=c("black","white"),
        overlap.labels = 0.2, 
        palette="YlGnBu",
        fontsize.labels = 7,
        align.labels=list(
          c("center", "center"), 
          c("right", "top"))) 
dev.off()


## Aportes totales por cada tipo de cereal  mejores cereales

cere.best <- filter (cere.1, culti=="ARROZ"|culti=="MAIZ"|culti=="SORGO")
cere.ton <- aggregate(cere.best$produ, by=list(anho=cere.best$ano, 
                                      culti=cere.best$culti), FUN=sum)


names (cere.ton)

ggplot(cere.ton) +
  geom_line(aes(x = anho, y = x/1000, colour=culti),
            alpha=1, size=0.8) +
  geom_point(aes(x = anho, y = x/1000), colour = "black", size = 4, shape=1, 
             stroke=1) +
  geom_point(aes(x = anho, y = x/1000, colour=culti), shape=16, size = 4) +
  scale_colour_brewer(name="Cultivo", palette= "YlGnBu") +
  xlab (expression (paste (''))) +
  ylab (expression (paste ('Total production'~(thousand~tons)))) +
  theme_bw() + 
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                               2014,2015,2016,2017,2018,2019,2020,2021,
                               2022,2023)) +
  theme (axis.title = element_text (size=14), 
         axis.text = element_text(size=14, angle=90), 
         legend.position="right",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

c7 <- ggplot(cere.ton, aes(x = anho, y = x/1000, fill=culti)) +
  geom_bar(stat="identity",
            alpha=1, width=0.7) +
  xlab (expression (paste (''))) +
  ylab (expression (paste ('Total production'~(thousand~ton)))) +
  theme_bw() + 
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                               2014,2015,2016,2017,2018,2019,2020,2021,
                               2022,2023)) +
  scale_fill_brewer(name="Crop", palette= "YlGnBu", 
                    breaks=c("ARROZ", "MAIZ", "SORGO"), 
                    labels=c("Rice", "Maize", "Sorghum")) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="none",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank(),
         panel.grid.major.x =element_blank())

cere.prom <-cere.1 %>%
  group_by(depa, ano, culti) %>%
  summarise_each(funs(mean(.,na.rm = TRUE)))

## calculo de calorías aportadas por cada cultivo

names (cere.ton)
cere.ton <- mutate(cere.ton, kcal.b = case_when(culti=="ARROZ" ~ x*3.59/1000000,
                                       culti=="MAIZ" ~ x*3.65/1000000,
                                       culti=="SORGO" ~ x*3.29/1000000))

cere.ton <- mutate(cere.ton, kcal.d.p = case_when(culti=="ARROZ" ~ (x*3.59*1000000)/(365*50000000),
                                              culti=="MAIZ" ~ (x*3.65*1000000)/(365*50000000),
                                              culti=="SORGO" ~ x*3.29*1000000/(365*50000000)))
cere.ton <- mutate(cere.ton, prot.d.p = case_when(culti=="ARROZ" ~ (x*0.0694*1000000)/(365*50000000),
                                                  culti=="MAIZ" ~ (x*0.0756*1000000)/(365*50000000),
                                                  culti=="SORGO" ~ x*0.102*1000000/(365*50000000)))


## Calculo de calorias per capita
ggplot(cere.ton) +
  geom_area(aes(x = anho, y = kcal.d.p, fill=culti),
            alpha=1, size=0.8) +
  xlab (expression (paste (''))) +
  ylab (expression (paste ('Calories'~(Kcal~day^-1~pers^-1)))) +
  theme_bw() + 
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                               2014,2015,2016,2017,2018,2019,2020,2021,
                               2022,2023)) +
  scale_fill_brewer(name="Crop", palette= "YlGnBu", 
                    breaks=c("ARROZ", "MAIZ", "SORGO"), 
                    labels=c("Rice", "Maize", "Sorghum")) +
  theme (axis.title = element_text (size=10), 
         axis.text = element_text(size=10, angle=90), 
         legend.position="right",
         panel.grid.minor.x=element_blank(),
         panel.grid.minor.y=element_blank())

c8 <- ggplot(cere.ton, aes(x = anho, y = kcal.d.p, fill=culti)) + 
  geom_bar(stat = "identity", 
           alpha=1, width = 0.7) +
  theme_bw() + 
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                               2014,2015,2016,2017,2018,2019,2020,2021,
                               2022,2023)) +
  scale_fill_brewer(name="Crop", palette= "YlGnBu", 
                    breaks=c("ARROZ", "MAIZ", "SORGO"), 
                    labels=c("Rice", "Maize", "Sorghum")) +
  ylab (expression(paste('Calories'~(kcal~x~day^-1~x~pers^-1)))) + 
  xlab (expression(paste(''))) +
  theme (axis.title = element_text (size=10), 
         axis.text.x = element_text(size=10, angle=90),
         axis.text.y = element_text(size=10),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

c9 <- ggplot(cere.ton, aes(x = anho, y = prot.d.p, fill=culti)) + 
  geom_bar(stat = "identity", 
           alpha=1, width = 0.7) +
  theme_bw() + 
  scale_x_continuous (breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,
                               2014,2015,2016,2017,2018,2019,2020,2021,
                               2022,2023)) +
  scale_fill_brewer(name="Crop", palette= "YlGnBu", 
                    breaks=c("ARROZ", "MAIZ", "SORGO"), 
                    labels=c("Rice", "Maize", "Sorghum")) +
  ylab (expression(paste('Protein'~(g~x~day^-1~x~pers^-1)))) + 
  xlab (expression(paste(''))) +
  theme (axis.title = element_text (size=10), 
         axis.text.x = element_text(size=10, angle=90),
         axis.text.y = element_text(size=10),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

grid.arrange(c7,c8,c9, ncol=3)

## Correlaciones variables agronomicas 
source("cor figure.R")
names (cere.best)
pairs.cor(cere.best[,6:9])

prom.cere <-cere.best %>%
  group_by(depa, ano, culti) %>%
  summarise_each(funs(mean(.,na.rm = TRUE)))

names (prom.cere)
pairs.cor(prom.cere[,6:9])

maiz.prom <- filter (prom.cere, culti=="MAIZ")
arroz.prom <- filter (prom.cere, culti=="ARROZ")
sorgo.prom <- filter (prom.cere, culti=="SORGO")

rcorr(maiz.prom$ar.semb, maiz.prom$produ, type="pearson")
rcorr(arroz.prom$ar.semb, arroz.prom$produ, type="pearson")
rcorr(sorgo.prom$ar.semb, sorgo.prom$produ, type="pearson")

c9 <- ggplot (prom.cere, aes (x=ar.semb, y=produ, fill=culti)) + 
  #geom_point (alpha=1, size=3) + 
  stat_binhex(color="white") +
  geom_smooth(aes(colour=culti),method=lm, se=F, linetype= "solid", size = 0.5, 
              alpha=0.8) +
  scale_fill_brewer(name="Crop", 
                     palette = "YlGnBu", 
                     breaks=c("ARROZ", "MAIZ", "SORGO"), 
                     labels=c("Rice", "Maize", "Sorghum")) +
  scale_colour_brewer(name="Crop", 
                    palette = "YlGnBu", 
                    breaks=c("ARROZ", "MAIZ", "SORGO"), 
                    labels=c("Rice", "Maize", "Sorghum")) +
  xlab (expression (paste ('Cultivated area'~(ha)))) + 
  ylab (expression (paste ('Production'~(ton)))) +
  annotate (geom="text", x=350, y=1300, label = "0.76***", size = 3, 
            fontface = "italic") +
  annotate (geom="text", x=450, y=800, label = "0.82***", size = 3, 
            fontface = "italic") +
  annotate (geom="text", x=600, y=600, label = "0.85***", size = 3, 
            fontface = "italic") +
  theme_bw() +
  theme (legend.position="right", axis.title = element_text (size=12), 
         axis.text = element_text(size=11), 
         axis.text.x = element_text(angle = 0),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())


names (cere.best)
cere.best.rendi <- summarySE (cere.best, measurevar = "rendi", 
           groupvars = c("depa", "culti"), na.rm = TRUE)


c10 <- ggplot(cere.best.rendi, aes(x=reorder_within(depa, rendi,culti, mean), 
                       y=rendi)) + 
  geom_bar(position = position_dodge(), stat = "identity", 
           width = 0.7, colour = "black", aes (fill=culti)) +
  geom_errorbar (aes(ymin=cere.best.rendi$rendi-cere.best.rendi$se, 
                     ymax=cere.best.rendi$rendi+cere.best.rendi$se), 
                 position = position_dodge (0.1), 
                 linetype ="solid", colour="black",
                 size=0.3, width=0.5) +
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(vars(culti), scales="free_x") +
  theme_bw() + 
  scale_x_reordered() +
  ylab (expression(paste('Yield'~(ton~x~ha^-1)))) + 
  xlab (expression(paste(''))) +
  theme (strip.text.x = element_text (size=0, angle=0, color="grey95"), 
         strip.background = element_rect(fill="grey95"),
         axis.title = element_text (size=10), 
         axis.text.x = element_text(size=8, angle=90),
         axis.text.y = element_text(size=10),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         legend.position = "none")

grid.arrange(c9,c10, layout_matrix = rbind(c(1,1), c(2,2)))

## importaciones maiz y arroz en Colombia
import <- read.delim ("imports.txt")
str (import)
names (import)
import$Country <- as.factor (import$Country) 
import$crop <- as.factor (import$crop)
import$Imports <- as.numeric (import$Imports) 

impor.cal <- mutate(import, kcal.d.p = case_when(crop=="Rice" ~ (Imports*3.59*1000000)/(365*50000000),
                                                  crop=="Maize" ~ (Imports*3.65*1000000)/(365*50000000)))

impor.cal <- mutate(impor.cal, prot.d.p = case_when(crop=="Rice" ~ (Imports*0.0694*1000000)/(365*50000000),
                                                  crop=="Maize" ~ (Imports*0.0756*1000000)/(365*50000000)))

#write.table (impor.cal, "import.calories.txt", sep="\t", dec=",",
#             row.names=F)

library ("reactable")
reactable(impor.cal)
##############################
## Tablas para multivariada ##
##############################

crops.all <- bind_rows(cere.1,fruta.1,horta.1,legum.1,olea.1,raiz.1,  
                       id=NULL)
insu.all <- bind_rows(arren.1,ferti.1,fungi.1,herbi.1,insecti.1,riego.1,  
                      id=NULL)
comm.all <- bind_rows(pre.cer.1,pre.fru.1,pre.hor.1,pre.rai.1,  
                      id=NULL)

names(crops.all)
prom.prod <-crops.all %>%
  group_by(depa, ano, culti) %>%
  summarise_each(funs(mean(.,na.rm = TRUE)))

names(insu.all)
prom.insu <-insu.all %>%
  group_by(depa, ano, tipo) %>%
  summarise_each(funs(mean(.,na.rm = TRUE)))

names(comm.all)
levels (comm.all$culti) <- c( "ACELGA","AGUACATE",                     
                              "AGUACATE","AGUACATE",                 
                              "AHUYAMA","AHUYAMIN",                    
                              "AJI","AJO",                                
                              "AJO IMPORTADO","APIO",                               
                              "ARRACACHA","ARRACACHA",                   
                              "ARROZ BLANCO IMPORTADO","ARROZ",                   
                              "ARROZ","ARROZ",                      
                              "ARROZ","ARVEJA AMARILLA SECA IMPORTADA",     
                              "ARVEJA ENLATADA","ARVEJA",              
                              "ARVEJA","ARVEJA VERDE SECA IMPORTADA",        
                              "BADEA","BANANITO",                   
                              "BANANO","BANANO",                       
                              "BERENJENA","BOROJO",                             
                              "BREVA","BROCOLI",                            
                              "CALABACIN","CALABAZA",                           
                              "CEBOLLA DE BULBO","CEBOLLA DE BULBO",   
                              "CEBOLLA CABEZONA BLANCA ECUATORIANA","CEBOLLA CABEZONA BLANCA IMPORTADA",  
                              "CEBOLLA DE BULBO","CEBOLLA CABEZONA BLANCA PERUANA",    
                              "CEBOLLA DE BULBO","CEBOLLA CABEZONA ROJA ECUATORIANA",  
                              "CEBOLLA CABEZONA ROJA IMPORTADA","CEBOLLA DE BULBO",      
                              "CEBOLLA CABEZONA ROJA PERUANA","CEBOLLA DE RAMA",                      
                              "CEBOLLA DE RAMA","CEBOLLA JUNCA BERLIN",               
                              "CEBOLLA DE RAMA","CEBOLLA JUNCA TENERIFE",             
                              "PUERRO","CEBOLLIN",                     
                              "CHOCOLO MAZORCA","CIDRA",                              
                              "CILANTRO","CIRUELA IMPORTADA",                  
                              "CIRUELA NEGRA CHILENA","CIRUELA NEGRA IMPORTADA",            
                              "CIRUELA","COCO",                               
                              "COL","COLIFLOR",                           
                              "CUCHUCO DE CEBADA","CUCHUCO DE MAIZ",                    
                              "CURUBA","CURUBA",                     
                              "DURAZNO IMPORTADO","DURAZNO",                   
                              "ESPINACA","FEIJOA",                             
                              "FRESA","FRIJOL",                       
                              "FRIJOL CABEZA NEGRA IMPORTADO","FRIJOL",       
                              "FRIJOL","FRIJOL",           
                              "FRIJOL","FRIJOL ENLATADO",                    
                              "FRIJOL","FRIJOL PALOMITO IMPORTADO",          
                              "FRIJOL","FRIJOL",                
                              "FRIJOL","FRIJOL",            
                              "FRIJOL","FRIJOL",                    
                              "GARBANZO IMPORTADO","GRANADILLA",                         
                              "GUANABANA","GUAYABA",                      
                              "GUAYABA","GUAYABA",                      
                              "GUAYABA MANZANA","GUAYABA PERA",                       
                              "GUAYABA PERA VALLUNA","GULUPA",                             
                              "HABA","HABICHUELA",                         
                              "HABICHUELA","HIGO",                               
                              "KIWI","LECHUGA",                    
                              "LECHUGA","LECHUGA",               
                              "LENTEJA IMPORTADA","LENTEJA",                   
                              "LIMON","LIMON",                
                              "LIMON COMUN ECUATORIANO","LIMON",                
                              "LIMON","LIMON",                       
                              "LULO","MAIZ",              
                              "MAIZ AMARILLO CASCARA IMPORTADO","MAIZ",           
                              "MAIZ","MAIZ",                
                              "MAIZ","MAIZ",               
                              "MAIZ ENLATADO","MAIZ",                          
                              "MANDARINA","MANDARINA",                    
                              "MANDARINA","MANGO",                        
                              "MANGO","MANGO",                    
                              "MANGO","MANGO",                      
                              "MANGO","MANGO",                        
                              "MANGO","MANGO",                       
                              "MANZANA","MANZANA ROJA IMPORTADA",             
                              "MANZANA ROYAL GALA IMPORTADA","MANZANA VERDE IMPORTADA",            
                              "MARACUYA","MARACUYA",                
                              "MARACUYA","MARACUYA",             
                              "MARACUYA","MELON",                     
                              "MORA","NAME",                       
                              "NAME","NAME",                        
                              "NARANJA","NARANJA",                      
                              "NARANJA","PAPA",                        
                              "PAPA","PAPA",                
                              "PAPA","PAPA",                 
                              "PAPA","PAPA",                     
                              "PAPA","PAPA",                 
                              "PAPA","PAPA",                    
                              "PAPA","PAPA",                  
                              "PAPA","PAPA",                      
                              "PAPA","PAPA",                      
                              "PAPA","PAPA",                        
                              "PAPA","PAPA",                         
                              "PAPAYA","PAPAYA",                     
                              "PAPAYA","PAPAYA",                     
                              "PAPAYA","PAPAYA",                     
                              "PATILLA","PATILLA",                       
                              "PEPINO COHOMBRO","PEPINO GUISO",                 
                              "PERA IMPORTADA","PERA",                      
                              "PEREJIL","PIMENTON",                           
                              "PIMENTON VERDE","PINA",                        
                              "PINA","PINA",                       
                              "PINA","PITAHAYA",                           
                              "PLATANO","PLATANO",     
                              "PLATANO","PLATANO",             
                              "PLATANO","PLATANO",              
                              "PLATANO","PLATANO HARTON VERDE ECUATORIANO",   
                              "PLATANO","PLATANO",       
                              "PLATANO HARTON VERDE VENEZOLANO","RABANO",                        
                              "REMOLACHA","REMOLACHA",                 
                              "REMOLACHA","REPOLLO",                     
                              "REPOLLO","REPOLLO",             
                              "REPOLLO","REPOLLO",                      
                              "TANGELO","TOMATE",                      
                              "TOMATE","TOMATE",              
                              "TOMATE DE ARBOL","TOMATE",                  
                              "TOMATE","TOMATE",               
                              "TOMATE","TOMATE",         
                              "TOMATE","UCHUVA",                 
                              "ULLUCO","UVA IMPORTADA",                     
                              "UVA ISABELA","UVA NEGRA",                          
                              "UVA RED GLOBE NACIONAL","UVA ROJA",                           
                              "UVA VERDE","YUCA",                       
                              "YUCA","YUCA",                           
                              "YUCA","ZANAHORIA",                          
                              "ZANAHORIA","ZANAHORIA",               
                              "ZAPOTE")
prom.comm <-comm.all %>%
  group_by(depa, ano, culti) %>%
  summarise_each(funs(mean(.,na.rm = TRUE)))

prom.comm$ano <- as.factor (prom.comm$ano)
prom.prod$ano <- as.factor (prom.prod$ano)

join <- full_join (prom.comm, prom.prod, by=c("ano","culti", "depa"))
names (join)
join <- join[,c(1:3,7,10:13)]
join <- join[complete.cases(join),]

names (prom.insu)
insu.melt <-prom.insu %>%
  spread(tipo, precio)

join.2 <- full_join (join, insu.melt, by=c("ano", "depa"))
names (join.2)
join.2 <- join.2[,c(1:8,12:17)]
join.2 <- join.2[complete.cases(join.2),]

## correlaciones ##
names (join.2)
levels (join.2$culti)
cere.all <- filter (join.2, culti=="ARROZ"|culti=="MAIZ")

source("cor figure.R")
pairs.cor(cere.all[,4:14])

## componentes principales ##
library(factoextra)
library (FactoMineR)
names (join.2)
pca.selected <- PCA (cere.all[,4:14], graph=F)

fviz_pca_biplot(pca.selected, 
                geom.ind = "point",
                pointshape = 20,
                col.ind = cere.all$culti, 
                palette = "Paired",
                addEllipses = T, label = "var",
                ellipse.type = "confidence",
                gradient.cols = "Spectral",
                col.var = "grey20", repel = T,
                legend.title = "Origen",
                ggtheme = theme_minimal(), 
                axes.linetype = "dotted", xlim=c(-3,6),
                ylim=c(-3, 3))
