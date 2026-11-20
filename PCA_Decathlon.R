########################
########################
###Ejercicio 1
########################
########################


rm(list=ls())
library(factoextra)
library(FactoMineR)

#Leemos los datos
datos<-decathlon2
data<-datos[1:10]
head(data)


test.pca<-PCA(data,scale.unit=T)#Realizamos el Análisis de componentes principales
#Escalamos los datos pq están en diferente escala.
#Sin estandarizar los datos, como están en diferente escala habría que usar la matriz de correlaciones,
#sin embargo, como los estamos estandarizando podemos usar la matriz de covarianzas, pues usar la matriz de 
#covarianzas de los datos estandarizados es equivalente a usar la matriz de correlaciones en los datos sin estandarizar
#[De no estandarizar los datos, la componente principal sería la variable con la mayor varianza] - comprobar
test.pca$eig#Sacamos los eigenvalores, el porcentaje de varianza y el porcentaje de varianza acumulado
fviz_eig(test.pca)#Representamos el % de varianza que explica cada componente
#54,951 % de variabilidad explicada con 2 dimensiones, es poco pero hay que tener en cuenta
#Que el ACP se hace en 10 dimensiones

#Calculamos los loadings
loadings<-sweep(test.pca$var$coord,2,sqrt(test.pca$eig[1:ncol(test.pca$var$coord),1]),FUN="/");loadings
barplot(loadings,beside = T) #peso de cada vble en cada componente/ sus coeficientes
round(test.pca$var$coord,2)
#Interpretación
# - 100m y long jump están correlacionadas negativamente, un atleta que corre rápido 
#los 100m, también saltara mucho. Es decir, tiene una buena actuación en ambas modalidades
# - 100m, 100m.hurdle y 400m tienen correlación positiva, atletas que actuén bien en una de ellas, 
#por lo general tendrán buenas actuaciones en las otras y viceversa
# - Pasa lo mismo con variables como High. jump y Shot put. Un atleta que actúe bien en High.Jump, también
#lo hará en Shot.put y viceversa
##Gráficos más bonitos####
fviz_pca_var(test.pca, col.var="cos2")
############################
# - [Al ver la longitud de los vectores, vemos como variables como Long jump, X100m, High jump o Pole Vault
#están bien representadas en el primer plano]
# - Por lo general, las variables relacionadas con pruebas de velocidad (a excepción de x1500m) y aquella 
#relacionada con Pole.vault tienen un efecto negativo en la primera componente principal

#####Gráfico con atletas
fviz_pca_ind(test.pca, col.ind="coord") # Proyección de los datos con color en función de la distancia al origen
##############
# - Karpov y Bourguignon tienen perfiles muy distintos, están opuestos en el eje de las abscisas
#(en el que se representa la primera componente principal), algo similar pasa con YURKOV y Noo, que 
#son opuestos en el eje de las ordenadas (eje en el que representamos la 2 componente principal)
# - Atletas como McMullen y Hernu o Schoenbeck y WARNERS tienen perfiles similares, pues están cercanos en ambos componentes
