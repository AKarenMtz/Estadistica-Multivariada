
#________ ANALISIS DISCRIMINANTE LINEAL_____

library(MASS)

# Se cargan los datos iris
Z<-as.data.frame(iris)
colnames(Z)
z1<-Z[71,]
summary(Z)
z1

# Se define la matriz de datos y la variable
# respesta con las clasificaciones.
x<-Z[,1:4]
y<-Z[,5]

# Definir como n y p el n�mero de flores y
# variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el An�lisis discriminante lineal (LDA)
# Cross validation (cv): clasificación optima
lda.iris<-lda(y~.,data=x, CV=TRUE)

# lda.iris$class contiene las clasificaciones hechas
# por CV usando LDA.
lda.iris$class

# Creacion de la tabla de clasificaciones buenas y malas
table.lda<-table(y,lda.iris$class)
table.lda

# Proporci�n de errores
mis.lda<- n-sum(y==lda.iris$class)
mis.lda/n


# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.iris<-c("indianred1","black")[1*(y==lda.iris$class)+1]
pairs(x,main="Buena clasificaci�n (negro), mala clasificaci�n (rojo)",
      pch=19,col=col.lda.iris)

# Probabilidad de pertenencia a uno de los tres grupos
lda.iris$posterior

# Grafico de probabilidades

plot(1:n, lda.iris$posterior[,1],
     main="Probabilidades a posteriori",
     pch=20, col="blue",
     xlab="N�mero de observaciones", ylab="Probabilidades")
points(1:n,lda.iris$posterior[,2],
       pch=20, col="green")
points(1:n,lda.iris$posterior[,3],
       pch=20, col="orange")
#----------------------------------------
  #Ejemplo de penguis
  
# Se cargan los datos iris
p<-as.data.frame(penguins)
colnames(p)


# Se define la matriz de datos y la variable
# respesta con las clasificaciones.
x<-p[,4:7]
y<-p[,8]

# Definir como n y p el n�mero de flores y
# variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el An�lisis discriminante lineal (LDA)
# Cross validation (cv): clasificación optima
lda.p<-lda(y~.,data=x, CV=TRUE)

# lda.iris$class contiene las clasificaciones hechas
# por CV usando LDA.
lda.p$class

# Creacion de la tabla de clasificaciones buenas y malas
table.p<-table(y,lda.p$class)
table.p

# Proporci�n de errores
mis.p<- n-sum(y==lda.p$class)
mis.p/n


# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.p<-c("indianred1","black")[1*(y==lda.p$class)+1]
pairs(x,main="Buena clasificaci�n (negro), mala clasificaci�n (rojo)",
      pch=19,col=col.lda.p)

# Probabilidad de pertenencia a uno de los tres grupos
lda.p$posterior

# Grafico de probabilidades

plot(1:n, lda.p$posterior[,1],
     main="Probabilidades a posteriori",
     pch=20, col="blue",
     xlab="N�mero de observaciones", ylab="Probabilidades")
points(1:n,lda.p$posterior[,2],
       pch=20, col="green")
#----------------------------------------
#Ejemplo de penguis por g�nero

# Se cargan los datos iris
pen<-as.data.frame(penguins)
colnames(pen)


# Se define la matriz de datos y la variable
# respesta con las clasificaciones.
x<-p[,4:7]
y<-p[,2]

# Definir como n y p el n�mero de flores y
# variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el An�lisis discriminante lineal (LDA)
# Cross validation (cv): clasificación optima
lda.p<-lda(y~.,data=x, CV=TRUE)

# lda.iris$class contiene las clasificaciones hechas
# por CV usando LDA.
lda.p$class

# Creaci�n de la tabla de clasificaciones buenas y malas
table.p<-table(y,lda.p$class)
table.p

# Proporci�n de errores
mis.p<- n-sum(y==lda.p$class)
mis.p/n


# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.p<-c("indianred1","black")[1*(y==lda.p$class)+1]
pairs(x,main="Buena clasificaci�n (negro), mala clasificaci�n (rojo)",
      pch=19,col=col.lda.p)

# Probabilidad de pertenencia a uno de los tres grupos
lda.p$posterior

# Grafico de probabilidades

plot(1:n, lda.p$posterior[,1],
     main="Probabilidades a posteriori",
     pch=20, col="blue",
     xlab="N�mero de observaciones", ylab="Probabilidades")
points(1:n,lda.p$posterior[,2],
       pch=20, col="green")
points(1:n,lda.p$posterior[,3],
       pch=20, col="orange")
