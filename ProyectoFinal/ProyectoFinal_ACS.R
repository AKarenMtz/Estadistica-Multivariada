#################################################################
#----------------- ANÁLISIS DE CORRESPONDENCIA SIMPLE------------
#______________________ Ana Karen Martínez Marín ________________
#################################################################

#Librerias que se utilizarán
library(gplots)
library(FactoMineR)
library(factoextra)
library(graphics)

# Base de datos
CJM <- read_csv("Estadística Multivariada/Prácticas/Proyecto/CJM.csv")
# Dimensión
dim(CJM)

# Nombre de las variables
colnames(CJM)

# Tipo de variables
str(CJM)

# Presencia de NA's
anyNA(CJM)

# Resumen de la base de datos
summary(CJM)

# 1. Convierte la matriz como una tabla
base=table( CJM$Estados, CJM$TipoViolencia)
base

# 2. Graph
balloonplot(t(base), main ="Incidencias de violencia atendidas en el CJM del centro y noreste del país", 
            xlab ="Tipos de Violencia", ylab="Estados", label = FALSE, show.margins = FALSE)

# 3. Prueba Chi-cuadrardo
chisq.test(base)

# Construcción del Análisis de Correspondencia
AC<-CA(base, graph =FALSE)
AC

# Vectores propios
eig.val <- get_eigenvalue(AC)
eig.val

# Visualización de las dimensiones 
fviz_eig(AC, addlabels = TRUE, col= "lightpink3", ylim = c(0,70))

# Gráfico CA-Biplot 
fviz_ca_biplot(AC, repel = TRUE)