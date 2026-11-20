
library(MASS)
library(klaR)
datos<-Pima.tr
head(datos);attach(datos)
test<-Pima.te
head(test)
######
#Análisis lineal discriminante
######
lda.fit<-lda(type~npreg+glu+bp+skin+bmi+ped+age,data=datos) #Establecemos en función de que queremos estimar la diabetes
lda.fit$prior #probabilidades a priori de ser diabética o no
lda.fit$means #Medias de mujer dependiendo de si tiene diabetes o no en cada variable predictora
lda.pred<-predict(lda.fit,newdata = test[,1:7]) #Hacemos las predicciones
errores.lda<-table(lda.pred$class, test$type);errores.lda #mostramos las predicciones en una tabla
#Vemos como para las mujeres que realmente no tienen diabetes 198 han sido clasificadas correctamente mientras que 25 no.
#Por otro lado, para las mujeres que realmente si la tienen, 67 están correctamente clasificadas y 42 no.
#Calculamos las tasas de error reales
e1_lda<-errores.lda[2,1]/sum(test$type=="No");e1_lda
e2_lda<-errores.lda[1,2]/sum(test$type=="Yes");e2_lda

######
#Análisis cuadrático discriminante
######
qda.fit<-qda(type~npreg+glu+bp+skin+bmi+ped+age,data=datos) #Establecemos en función de que queremos estimar la diabetes
qda.fit$prior #probabilidades a priori de ser diabética o no
qda.fit$means #Medias de mujer dependiendo de si tiene diabetes o no en cada variable predictora
qda.pred<-predict(qda.fit,newdata = test[,1:7]) #Hacemos las predicciones
errores.qda<-table(qda.pred$class, test$type);errores.qda #mostramos las predicciones en una tabla
#Vemos como para las mujeres que realmente no tienen diabetes 194 han sido clasificadas correctamente mientras que 29 no.
#Por otro lado, para las mujeres que realmente si la tienen, 62 están correctamente clasificadas y 47 no.
#Calculamos las tasas de error reales
e1_qda<-errores.qda[2,1]/sum(test$type=="No");e1_qda
e2_qda<-errores.qda[1,2]/sum(test$type=="Yes");e2_qda

######
#Discriminación logística
######


glm.fit <- glm(type ~ npreg+glu+bp+skin+bmi+ped+age, data = datos, family = "binomial")#Construimos el modelo logístico
glm.pred<-predict(glm.fit,type="response",newdata = test[,1:7])#Realizamos la predicción
#Representamos el ajuste logístico
clas.pred<-glm.pred;#Guardamos los resultados de la predicción en otro objeto
clas.pred[glm.pred<0.5]<-"No";clas.pred[glm.pred>=0.5]<-"Yes"#Clasificamos los resultados de la predicción en "Si" o "No".
#Cuando la predicción es menor de 0.5 se clasifica como no diabetes, mientras que si es superior a 0.5 se clasifica como si diabetes.
errores.glm<-table(clas.pred, test$type); errores.glm #Mostramos en una tabla los errores de predicción
#Vemos como para las mujeres que realmente no tienen diabetes 200 han sido clasificadas correctamente mientras que 23 no.
#Por otro lado, para las mujeres que realmente si la tienen, 66 están correctamente clasificadas y 43 no.
#Calculamos las tasas de error reales
e1_glm<-errores.glm[2,1]/sum(test$type=="No");e1_glm
e2_glm<-errores.glm[1,2]/sum(test$type=="Yes");e2_glm

#Los tres métodos dan un resultado similar, el análisis cuadrático discriminante algo peor quizas.
error1<-cbind(e1_lda,e1_qda,e1_glm)
error2<-cbind(e2_lda,e2_qda,e2_glm)
errores<-rbind(error1,error2)
colnames(errores)<-c("LDA","QDA","GLM")
rownames(errores)<-c("e1,re","e2,re")
errores
