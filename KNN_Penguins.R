#______ kNN_______
#K-vecinos próximos

# Cargar los datos de penguins

library(readxl)
penguins <- read_excel("Estadística Multivariada/penguins.xlsx")
Z<-penguins
colnames(Z)
Z<-data.frame(Z)
# Definir la matriz de datos y la variable respuesta
# Con las clasificaciones
x<-Z[,4:7]
x
y<-Z[,2]
y
# Se definen las variables y observaciones
n<-nrow(x)
p<-ncol(x)

# Grafico scatter plot
# Creacion de un vector de colores
col.iris<-(c("blue","green","orange","red")[y])
col.iris

pairs(x, main="Data set penguins, largo_pico(azul), grosor_pico(verde), largo_aleta(naranja), masa_corporal(rojo)", pch=19,col=col.iris)

#-----------------------
#         kNN
#-----------------------

library(class)
summary(x)
sqrt(x)
# Se fija una "semilla" para tener valores iguales
set.seed(1500)

# creacion de los ciclos
# para k=1 hasta k=30
# Selecciona el valor de k que tenga el error
# mas bajo.
# InicializaciÃ³n de una lista vacia de tamaño 30
knn.class<-vector(mode="list",length=30)
knn.tables<-vector(mode="list", length=30)

# Clasificaciones erroneas
knn.mis<-matrix(NA, nrow=30, ncol=1)
knn.mis
for(k in 1:30){
  knn.class[[k]]<-knn.cv(x,y,k=k)
  knn.tables[[k]]<-table(y,knn.class[[k]])
  # la suma de las clasificaciones menos las correctas
  knn.mis[k]<- n-sum(y==knn.class[[k]])
}

knn.mis

# Numero optimo de k-vecinos
which(knn.mis==min(knn.mis))

knn.tables[[1]]

# el mas eficiente es k=10
# se seÃ±ala el k mas eficiente
k.opt<-1

knn.cv.opt<-knn.class[[k.opt]]
knn.cv.opt

# tabla de contingencia con las clasificaciones buenas y malas
knn.tables[[k.opt]]

# cantidad de observaciones mal clasificadas
knn.mis[k.opt]

# Error de clasificacion (MR)
knn.mis[k.opt]/n

# Grafico de clasificaciones correctas y erroneas
col.knn.iris<-c("indianred1","black")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificación kNN de Penguins por género",
      pch=19, col=col.knn.iris)
