        ##################################
        #    CASO DE ESTUDIO CLUSTERING  #
        ##################################

#INTEGRANTES:
            # Arcineiga Edison
            # Picho Carlos 
            # Quelal Fidel


library(readr)
segmentation_data <- read_csv("~/N.- 2020/CARLOS PICHO/N.- UPS/MSc (Master of Science)/08 HERRAMIENTAS PARA EL CONTROL DE LA PRODUCCION/10 Tareas/TAREA 01/segmentation_data.csv")
View(segmentation_data)

customers=data.frame(segmentation_data[,2:4]) #eliminamos la primera columna por que es una variable sin importancia

#####Escalamiento Multidimensional######
d = dist(customers, method = "euclidean")#se demora en correr
c=cor(customers)#matriz de correlaciones:
# se puede notar en la matriz de correlaciones que existe mayor correlacion
# entre la variable frecuencia y capacidad adquisitiva
d1=dist(customers[,c(1,3)])
library(corrplot)
corrplot(c)
fit = cmdscale(d,eig=TRUE, k=2) # k es el numero de dimensiones
#reduccion de 3 a dos dimensiones, escalamiento multidimencional para poder
#graficar en 2 dimencioneS
fit1 = cmdscale(d1,eig=TRUE, k=1)
x1 = fit1$points
y1 = customers$recency
plot(x1,y1,col="blue")

#########Crear Grupos: K-Means########

grupos = kmeans(customers,4)
g1 = grupos$cluster
g2 = grupos$size

plot(x1,y1,col=c("red","green3","blue","purple")[g1], main = "Customers Dataset K-Means")

# Etiqueta de la clasificación.
# Estos codigos condiconales nos ayudan a cambiar las nomenclaruras de las etiquetas segun lo indicado en el taller de clusetering.
i=1
w = c()
g22=sort(g2)
if(g2[i]==g22[1]){
  w[i]=0
} else if(g2[i]==g22[2]){
  w[i]=1
} else if(g2[i]==g22[3]){
  w[i]=2
} else if(g2[i]==g22[4]){
  w[i]=3}
print(i)

i=2
if(g2[i]==g22[1]){
  w[i]=0
} else if(g2[i]==g22[2]){
  w[i]=1
} else if(g2[i]==g22[3]){
  w[i]=2
} else if(g2[i]==g22[4]){
  w[i]=3}
i=3
if(g2[i]==g22[1]){
  w[i]=0
} else if(g2[i]==g22[2]){
  w[i]=1
} else if(g2[i]==g22[3]){
  w[i]=2
} else if(g2[i]==g22[4]){
  w[i]=3}
i=4
if(g2[i]==g22[1]){
  w[i]=0
} else if(g2[i]==g22[2]){
  w[i]=1
} else if(g2[i]==g22[3]){
  w[i]=2
} else if(g2[i]==g22[4]){
  w[i]=3}

# Nos ayuda a colocar las nuevas etiquetas segun los datos de entrada originales.
gt=g1   
for (n in 1:length(gt)) {
  if(gt[n]==1){
    gt[n]=w[1]
  } else if(gt[n]==2){
    gt[n]=w[2]
  } else if(gt[n]==3){
    gt[n]=w[3]
  } else if(gt[n]==4){
    gt[n]=w[4]}
}

customers1= customers
customers1$tag1<-gt # se crea una nueva variable para insertar los datos de etiqueta del cluster K-means


#########Crear Grupos: Jerarquicos#########

library("dendextend")
hc = hclust(d, method = "complete" )
clus3 = cutree(hc, 4)
plot(x1,y1,col=c("red","green3","blue","purple")[clus3], main = "Customers Dataset Jerarquico")

gt=clus3
for (n in 1:length(gt)) {
  gt[n]=gt[n]-1
}

customers1$tag2<-gt
View(customers1)  

#Pregunta 2 #

#Validacion
library(cluster)
library(clValid)
du1 = dunn(d1,customers1$tag1)
du2 = dunn(d,customers1$tag2)
#segun el indice de dumm el algorithmo de clustering de dhc es mucho mejor
#que el kmeans
sil1 = silhouette(customers1$tag1,d1)
plot(sil1,col=1:4, border=NA)

sil2 = silhouette(customers1$tag2,d)
plot(sil2,col=1:4, border=NA)

#####Viendo las 4 siluetas parece más adecuado elegir los k=4 grupos 
#####porque son más homogéneos. De todos modos procede un análisis del tamaño 
#####de los grupos porque a la vista de las siluetas 
#####parece que algunas observaciones distorsionan el agrupamiento



library(aricode)
library(plyr)
ARI1= ARI(customers1$tag1,customers1$tag2)
AMI1= AMI(customers1$tag1,customers1$tag2)
NMI1= NMI(customers1$tag1,customers1$tag2,variant = c("joint"))


par(mfrow = c(2,2))
plot(customers[,1:2], col = "black","gree","red","pink", main = "Datos de Clientes")
plot(customers[,1:2], col = customers1$tag1+1, main = "K-MEANS")
plot(customers[,1:2], col = customers1$tag2+1, main = "JERARQUICO")

par(mfrow = c(1,1))

