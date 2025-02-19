#################################################################
#----------------- AN�LISIS DE CORRESPONDENCIA SIMPLE------------
#______________________ Ana Karen Mart�nez Mar�n ________________
#################################################################

#Librerias que se utilizar�n
library(gplots)
library(FactoMineR)
library(factoextra)
library(graphics)

# Base de datos
CJM <- read_csv("Estad�stica Multivariada/Pr�cticas/Proyecto/CJM.csv")
# Dimensi�n
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
balloonplot(t(base), main ="Incidencias de violencia atendidas en el CJM del centro y noreste del pa�s", 
            xlab ="Tipos de Violencia", ylab="Estados", label = FALSE, show.margins = FALSE)

# 3. Prueba Chi-cuadrardo
chisq.test(base)

# Construcci�n del An�lisis de Correspondencia
AC<-CA(base, graph =FALSE)
AC

# Vectores propios
eig.val <- get_eigenvalue(AC)
eig.val

# Visualizaci�n de las dimensiones 
fviz_eig(AC, addlabels = TRUE, col= "lightpink3", ylim = c(0,70))

# Gr�fico CA-Biplot 
fviz_ca_biplot(AC, repel = TRUE)