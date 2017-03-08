library(ISLR)
library(MASS)

#Cargamos el dataset:

data("Smarket")  # Smarket es un dataframe que consta de 1250 observaciones y 9 variables

#Usaremos la tecnica Holdout para muestreo

tr<-sample(1:1250,625)# 50% de los datos
tr2<-sample(c(1:nrow(Smarket))[-tr],312)#25% de los datos
tr3<-sample(c(1:nrow(Smarket))[-c(tr2,tr)],313)#25% restante

#Creamos el training,validation,testing

training_set<-Smarket[tr,]
validation_set<-Smarket[tr2,]
testing_set<-Smarket[tr3,]

#Entrenamos los modelos glm, lda 

modeloglm<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = 'binomial',data = training_set)
modelolda<-lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = training_set)

pre<-predict.glm(modeloglm,newdata = testing_set,type = 'response')
predict(modelolda,testing_set)$class

# Matriz de confusion

confussionmatrix<-table(pre>0.5,testing_set[,"Direction"])

#Evaluamos el modelo usando la sensibilidad:

sensitivity<-confussionmatrix[2,2]/(confussionmatrix[2,2]+confussionmatrix[1,1]) ## =0.54


