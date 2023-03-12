setwd("C:\\Users\\amari\\Desktop\\Mineria_de_Datos_R\\T_final/")
eucaliptos_dataset <- read.csv(file="eucalyptus\\dataset_194_eucalyptus.csv", stringsAsFactors = F, header = T)
#Observar dataset
View(eucaliptos_dataset)
#Imprimir primeras 40 filas 
head(eucaliptos_dataset, n = 40)
#Imprimir últimas 40 filas 
tail(eucaliptos_dataset, n = 40)
#Tipos de atributos disponibles
str(eucaliptos_dataset)
class(eucaliptos_dataset$Ht)
#Tenemos varias columnas determinadas como character en cambio de interger. Hay que convertirlas

DBH_int <-as.numeric(eucaliptos_dataset$DBH)
Ht_int <-as.numeric(eucaliptos_dataset$Ht)
Surv_int <-as.numeric(eucaliptos_dataset$Surv)
Vig_int <-as.numeric(eucaliptos_dataset$Vig)
Ins_res_int <-as.numeric(eucaliptos_dataset$Ins_res)
Stem_Fm_int <-as.numeric(eucaliptos_dataset$Stem_Fm)
Crown_Fm_int <-as.numeric(eucaliptos_dataset$Crown_Fm)
Brnch_Fm_int <-as.numeric(eucaliptos_dataset$Brnch_Fm)
eucaliptos <-data.frame(eucaliptos_dataset[,c(1:11,20)],DBH_int, Ht_int, Surv_int, Vig_int, Ins_res_int, Stem_Fm_int, Crown_Fm_int, Brnch_Fm_int)

#Probar si funcionó usando uno de los atributos que previamente era character
class(eucaliptos$Ht_int)
str(eucaliptos)
View(eucaliptos)
#Ahora tenemos la información en el formato correcto para hacer los análisis y transformaciones pertinentes
#Ya se puede observar que hay casos NA en el dataset. Hay que eliminarlos/imputarlos

#eliminarlos: 
nas <- is.na(eucaliptos$Utility)
table(nas)
nas <-is.na(eucaliptos$DBH)
table(nas)
nas<-is.na(eucaliptos$Ht_int)
table(nas)
nas<-is.na(eucaliptos$Surv_int)
table(nas)
#Siendo que estas características son la de mayor interés, perderíamos 94 imputs si eliminamos las entradas incompletas
eucaliptos_completos <-complete.cases(eucaliptos)
table(eucaliptos_completos)
eucaliptos_imputs_completos <- na.omit(eucaliptos)
dim(eucaliptos_imputs_completos)
100 - (dim(eucaliptos_imputs_completos)/dim(eucaliptos)*100)
#Se perderían 12% de las imputs. Es aceptable si están distribuidos equitativamente entre las zonas de medición
eucaliptos_nas <- is.na(eucaliptos$Surv_int)
table(eucaliptos_nas)
new_DF <- subset(eucaliptos, is.na(eucaliptos$Surv_int))
table(new_DF$Locality)
table(eucaliptos$Locality)
#Perderíamos todos los imputs de "Central_Poverty_Bay". Es mejor mantener esas 6 entradas. 

#Imputar valores de "Surv_Int" usando la media
Surv_int_imputados_por_media <- eucaliptos$Surv_int
nas <- is.na(Surv_int_imputados_por_media)
Surv_int_imputados_por_media[nas] <- mean(Surv_int_imputados_por_media, na.rm = T)

#Chequear que la imputación haya funcionado
any(is.na(Surv_int_imputados_por_media))
eucaliptos_imputados <-data.frame(eucaliptos, Surv_int_imputados_por_media)
View(eucaliptos_imputados)

#Ahora hay que detectar valores atípicos (outliers) y eliminarlos de las variables que nos importan
#En el trabajo original se tomaron en cuenta los atributos "DBH", "Ht" (height) y "Surv" (survival) como criterios
#de evaluación de utilidad. Estas son los atributos que van a ser limpiados de outliers.

#Estadísticas descriptivas
summary(eucaliptos_imputados$DBH_int)
summary(eucaliptos_imputados$Ht_int)
summary(eucaliptos_imputados$Surv_int_imputados_por_media)
#Histogramas
hist(eucaliptos_imputados$DBH_int,main="DBH", xlab="Distribución de valores DBH")
hist(eucaliptos_imputados$Ht_int, main="Height", xlab="Distribución de valores de altura de árboles")
hist(eucaliptos_imputados$Surv_int_imputados_por_media, main="Survival rate imputado", xlab="Distribución de valores de supervivencia")
#Boxpltos
boxplot(eucaliptos_imputados$DBH_int, main="DBH")
boxplot(eucaliptos_imputados$Ht_int, main="altura de árboles")
boxplot(eucaliptos_imputados$Surv_int_imputados_por_media, main="supervivencia imputada")

#Hay que eliminar un valor atípico en DBH
#"Ht" muestra valores atipicos en los gráficos y en la estadística hay valores mínimos y máximos lejanos a los demás.
#Hay una entrada NA en DBH y Ht. Se va a eliminar para evitar problemas en próximos pasos.
eucaliptos_imputados_completos <- na.omit(eucaliptos_imputados)
summary(eucaliptos_imputados_completos$DBH_int)
hist(eucaliptos_imputados_completos$DBH_int, main ="DBH")
#View(eucaliptos_imputados_completos)
eucaliptos_imputados_comp_no_outliers <- eucaliptos_imputados_completos[eucaliptos_imputados_completos$DBH_int < 100, ]
hist(eucaliptos_imputados_comp_no_outliers$DBH_int, main="DBH sin outliers", xlab="valores de DBH")
View(eucaliptos_imputados_comp_no_outliers)
#Criterio IQR boxplot
iqr <- IQR(eucaliptos_imputados_comp_no_outliers$Ht_int)
iqr
q   <- quantile(eucaliptos_imputados_comp_no_outliers$Ht_int)
q

#Usamos el criterio del boxplot
casos_extremos_iqr <- eucaliptos_imputados_comp_no_outliers$Ht_int > q[4] + iqr | eucaliptos_imputados_comp_no_outliers$Ht_int < q[2] - iqr
table(casos_extremos_iqr)

#Se eliminan los valores que quedan fuera del rango IQR
Ht_criterio_iqr <- eucaliptos_imputados_comp_no_outliers$Ht_int[!casos_extremos_iqr]
summary(Ht_criterio_iqr)
hist(Ht_criterio_iqr)
boxplot(eucaliptos_imputados_comp_no_outliers$Ht_int, main="eucaliptos_imputados        iqr", Ht_criterio_iqr)
#Se eliminaron 26 entradas. Quedan 616 para utilizar
eucaliptos_iqr_Ht <-as.data.frame(eucaliptos_imputados_comp_no_outliers, )
#-- 
#Analisis de exploratorio de relacion entre variables y entradas

Eucalyptus_subset = eucaliptos_imputados_comp_no_outliers[, c(13:15)]
summary(Eucalyptus_subset)
Eu_pca <- prcomp(Eucalyptus_subset, center = T, scale. = T)
summary(Eu_pca)
#install.packages("ggfortify")
library("ggfortify")
autoplot(Eu_pca, data = Eucalyptus_subset, colour= "red")
Eu_pca$rotation

k <- 8
clusters <- kmeans(x = Eucalyptus_subset, centers = k)
clusters
#table(clusters$cluster)

#install.packages(c( "factoextra", "fpc", "cluster"))
library("cluster")
library("factoextra")
fviz_cluster(clusters, data = Eucalyptus_subset)

wss <- c()
ks <- 2:10
for(k in ks){
  clusters <- kmeans(x = Eucalyptus_subset, centers = k)
  wss      <- c(wss, clusters$tot.withinss)
}
plot(ks, wss, type="b", xlab = "k", ylab="Suma de la varianza intra-cluster")

distancia <- dist(Eucalyptus_subset)
arbol <- hclust(d = distancia, method = "average")
plot(arbol, hang = -1)
abline(h = 25, col = "red", lty="dashed")
clusters_25 <-cutree(arbol, h = 25)
plot(clusters_25, hang = -1)
#View(eucaliptos_imputados_comp_no_outliers)

s <- silhouette(clusters$cluster, dist(Eucalyptus_subset))
plot(s)

#eucaliptos_imputados_comp_no_outliers$Utility2<- recode(eucaliptos_imputados_comp_no_outliers$Utility, "2 == 5; 3 == 4; 1 == 3; 4 == 2; 5 == 1")
#View(eucaliptos)
#euca_ut_conv <-eucaliptos_imputados_comp_no_outliers$Utility("best"=5, "good"=4, "average"= 3, "low"=2, "none"=1)
#View(euca_ut_conv)
#Utility_conversion <-as.numeric(as.factor(eucaliptos_imputados_comp_no_outliers$Utility))


View(eucaliptos_imputados_comp_no_outliers)
eucaliptos_imputados_comp_no_outliers[,12] <- ifelse(eucaliptos_imputados_comp_no_outliers[,12]== "none", 1, ifelse(eucaliptos_imputados_comp_no_outliers[,12] == "low", 2, ifelse(eucaliptos_imputados_comp_no_outliers[,12] == "average", 3, ifelse(eucaliptos_imputados_comp_no_outliers[,12] == "best", 5, 4))))
eucaliptos_imputados_comp_no_outliers$Sp_Numero <-as.numeric(as.factor(eucaliptos_imputados_comp_no_outliers$Sp))
View(eucaliptos_imputados_comp_no_outliers)


#plots scattering

#1)
#A fin de encontrar los lotes con mayor utilidad de semilla para conservar los suelos se graficó la variable lote (PMCno) vs (Utility). Antes se paso la variable utility a un valor numérico según el orden alfabetico ( average, 1; best, 2;  good, 3; low, 4; none, 5)
plot(eucaliptos_imputados_comp_no_outliers$PMCno, eucaliptos_imputados_comp_no_outliers$Utility, main= "Nro de lote vs Utilidad")

#Resultados y discusion de esto: entre las 5 categorías de utilidad El único que es diferente es average que presenta un lote cercano a 0

#2)
plot(eucaliptos_imputados_comp_no_outliers$DBH_int, eucaliptos_imputados_comp_no_outliers$Utility, main= "DBH vs Utilidad", xlab = "DBH", ylab="Utilidad")
plot(eucaliptos_imputados_comp_no_outliers$Surv_int, eucaliptos_imputados_comp_no_outliers$Utility, main= "Supervivencia vs Utilidad", xlab = "Supervivencia", ylab="Utilidad")



plot(eucaliptos_imputados_comp_no_outliers$Sp_Numero, eucaliptos_imputados_comp_no_outliers$Utility, main= "Especies vs Utilidad", xlab = "Nro. de especie", ylab="Utilidad")
table(eucaliptos_imputados_comp_no_outliers$Sp)
table(eucaliptos$Utility_conversion)
