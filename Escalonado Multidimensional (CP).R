
#______ Escalado multidimensional M�trico______

# Cargamos la matriz de datos eurodist

data.dist<-eurodist
dim(data.dist)
str(data.dist)
View(data.dist)

# Transformamos los datos en matriz
data.dist<-as.matrix(data.dist)

#-----------------------------------
#  Extracci�n de las filas de la matriz
#-----------------------------------

# N�mero de ciudades
n<-nrow(data.dist)

#------------------------------------
#  Escalado multidimensional cl�sico
#------------------------------------
# 1.- c�lculo de autovalores
# Dentro del objeto mds.cities se encuentran
# almacenado los valores propios (eigenvalues) en
# mds.cities$eig

mds.cities<-cmdscale(data.dist, eig = TRUE)
mds.cities

# 2.- Generacion del grafico
plot(mds.cities$eig, pch=19, col="blue", 
     xlab="N�meros", ylab="Valores Propios",
     type="o")
abline(a=0, b=0, col="red")

# Interpretaci�n: se identifican autovalores negativos
# Se considera como soluci�n el seleccionar
# r=2 coordenadas principales.


# 3.- Medidas de precisi�n

m<-sum(abs(mds.cities$eig[1:2]))/sum(abs(mds.cities$eig))
m

#4.- Obtenci�n de coordenadas principales fijando
# k=2 y se realice con los dos primeros autovalores.
mds.cities<-cmdscale(data.dist, eig=TRUE, k=2)

x1<-mds.cities$points[,1]
x2<-mds.cities$points[,2]

# 5.- Generaci�n del gr�fico en dos dimensiones de los
# datos con las coordenadas obtenidas
plot(x1,x2,pch=19, col="blue", 
     xlim = range(x1)+c(0,600))
text(x1,x2, pos=4, labels = rownames(data.dist),
     col="black")

# Se invierten los ejes del plot
x2<--x2

plot2<-plot(x1,x2,pch=19, col="purple", 
     xlim = range(x1)+c(0,600))
text(x1,x2, pos=4, labels = rownames(data.dist),
     col="black")
