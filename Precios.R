## Análisis de precios ##
library ("Hmisc")
library ("dplyr")
library ("reshape2")
library("ggplot2")
library ("RColorBrewer")
library("tidyr")
library("grid") 
library("gridExtra") 

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


## Remover outliers
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
  geom_boxplot(colour="black", fill="lightblue1", alpha=0.8) + 
  stat_summary(fun.y=mean, geom="point", col="blue2", shape = 10, size=4) +
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
