##Ejercicio de An�lisis de Componentes Principales.
###### Ana Karen Mart�nez Mar�n ###### 
##1.- Se instala el paquete de datos para utlizar una base de datos.

#install.packages("datos")
library(datos)
#### Se selecciona la base de datos (una base que tenga datos cuantitativos)
datos::atmosfera
x<-datos::atmosfera
x

#Se reducir� la base de datos, seleccionando s�lo los datos m�s recientes hasta ese momento, que es en
#el mes de diciembre del a�o 2000 solamente.
x<-x[40897:41472,1:11]

##Visualizamos la base de datos:

#La dimensi�n
dim(x)
dim

#El tipo de variables
str(x)


#Se reduce el n�mero de variables. Y s�lo quedarenos con las variables cuantitativas.
x$anio <- NULL
x$mes <- NULL
x$latitud <- NULL
x$longitud <- NULL
x1=x
# Visualizaci�n de los datos nulos.
anyNA(x)
# Parecer si hay datos nulos que est�n en la variable nube_baja. Por lo que tambi�n la quitaremos de nuestra matriz.
x$nube_baja <- NULL
View(x)

# Nuetra matriz la convertimos a un data.frame
x1=x
x1<-as.data.frame(x1)
# Se definen n (n�mero de registros) y p (variables)
dim(x1)

n<-dim(x1)[1]
p<-dim(x1)[2]

#Generaci�n de un scatterplot de las variables originales
pairs(x1,col="lightpink3", pch=21,bg = "lightblue", cex = 0.9, lwd=0.9,
      main="Variables originales")

# Obtenci�n de los componentes principales
# con base en la matriz de covarianza muestral
mu<-colMeans(x1)
mu
S<-cov(x1)
S
#Obtenci�n de los componentes principales 
# con base a la matriz de covarianza muestral
es<-eigen(S)
es

#Matriz de auto-valores
eigen.val<-es$values
eigen.val

#Separaci�n de la matriz de valores propios.
#Matriz de auto-vectores
eigen.vec<-es$vectors
eigen.vec

#Proporci�n de variabilidad para cada valor.
pro.var<-eigen.val/sum(eigen.val)
pro.var

# Proporci�n de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum

#----------------------------------
# Obtencion de los componentes principales con
# base en la matriz de correlaciones muestrales
#---------------------------------------

R<-cor(x1)
R
eR<-eigen(R)
eR

# Obtenci�n de auto-valores
eigen.val.R<-eR$values
eigen.val.R
# Obtenci�n de auto-vectores
eigen.vec.R<-eR$vectors
eigen.vec.R
# Proporcion de variablidad
pro.var.R<-eigen.val/sum(eigen.val.R)
pro.var.R

# Proporcion de variabilidad acumulada
pro.var.acum.R<-cumsum(eigen.val.R)/sum(eigen.val.R)
pro.var.acum.R

# Media de los auto-valores
mean(eigen.val.R)

#-------------------------------------------------
# Obtencion de los coeficientes (nuevas variables)
# 
#-------------------------------------------------

# 1.- Centrar los datos con respecto a la media
ones<-matrix(rep(1,n),nrow=n, ncol=1)
ones
# 2.- Construccion de la matriz centrada
X.cen<-as.matrix(x1)-ones%*%mu
X.cen

# 3.- Construccion de la matriz diagonal de las 
# covarianzas
Dx<-diag(diag(S))
Dx

# 4.- Construccion de la matriz centrada multiplicada
# por Dx^1/2

Y<-X.cen%*%solve(Dx)^(1/2)
Y #datos normalizados

# 5.- Construccion de los coeficientes o scores
# eigen.vec matriz de autovectores
scores<-Y%*%eigen.vec
scores
# Nombramos las columnas PC1...PC8
colnames(scores)<-c("PC1","PC2","PC3","PC4","PC5",
                    "PC6")

# visualizamos
scores

# Generacion del grafico de los scores
pairs(scores, main="scores", col="blue", pch=19)


