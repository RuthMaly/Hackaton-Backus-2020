#Carga librerias
#Regresión Logistica
if(!require(caret)){install.packages("caret")}
if(!require(sas7bdat)){install.packages("sas7bdat")}
if(!require(pROC)){install.packages("pROC")} 
if(!require(glmnet)){install.packages("glmnet")}

library(caret)
library(sas7bdat)
library(pROC)
library(glmnet)


#Funcion tasa de acierto y otras medidas
compararLog<-function(modelo,dd,nombreVar,evento){
  probs <-predict(modelo, newdata=dd,type="response")
  cm<-confusionMatrix(data=factor(ifelse(probs>0.5,1,0)), dd[,nombreVar],positive=evento)
  c(cm$overall[1],cm$byClass[1:2])
}

compararLogLASSO<-function(modelo,dd,nombreVar,evento){
  probs<-predict(modelo, model.matrix(as.formula(paste0(nombreVar,"~.")), data=dd)[,-1],s="lambda.1se",type="response")
  cm<-confusionMatrix(data=factor(ifelse(probs>0.5,1,0)), dd[,nombreVar],positive=evento)
  c(cm$overall[1],cm$byClass[1:2])
}

#Para evaluar el pseudo-R2 en regr. logística en cualquier conjunto de datos
pseudoR2<-function(modelo,dd,nombreVar){
  pred.out.link <- predict(modelo, dd, type = "response")
  mod.out.null <- glm(as.formula(paste(nombreVar,"~1")), family = binomial, data = dd)
  pred.out.linkN <- predict(mod.out.null, dd, type = "response")
  1-sum((dd[,nombreVar]==1)*log(pred.out.link)+log(1 -pred.out.link)*(1-(dd[,nombreVar]==1)))/
    sum((dd[,nombreVar]==1)*log(pred.out.linkN)+log(1 -pred.out.linkN)*(1-(dd[,nombreVar]==1)))
}

pseudoR2LASSO<-function(modelo,dd,nombreVar){
  pred.out.link <- predict(modelo, model.matrix(as.formula(paste0(nombreVar,"~.")), data=dd)[,-1],s="lambda.1se",type="response")
  mod.out.null <- glm(as.formula(paste(nombreVar,"~1")), family = binomial, data = dd)
  pred.out.linkN <- predict(mod.out.null, dd, type = "response")
  1-sum((dd[,nombreVar]==1)*log(pred.out.link)+log(1 -pred.out.link)*(1-(dd[,nombreVar]==1)))/
    sum((dd[,nombreVar]==1)*log(pred.out.linkN)+log(1 -pred.out.linkN)*(1-(dd[,nombreVar]==1)))
}

#Gr???fico con la importancia de las variables en regr. log???stica
impVariablesLog<-function(modelo,nombreVar,dd=data_train){
  null<-glm(as.formula(paste(nombreVar,"~1")),data=dd,family=binomial)
  aux2<-capture.output(aux<-step(modelo, scope=list(lower=null, upper=modelo), direction="backward",k=0,steps=1))
  aux3<-read.table(textConnection(aux2[grep("-",aux2)]))[,c(2,5)]
  aux3$V5<-(aux3$V5-modelo$deviance)/modelo$null.deviance
  barplot(aux3$V5,names.arg = aux3$V2,las=2,horiz=T,main="Importancia de las variables (Pseudo-R2)")
}

#Lectura de datos
#read.csv('C:/Users/Ruth/OneDrive/curso de Master I ciclo/SEMMA/Trabajo N1/Agrobanco_Depurado.csv')
#datos<-read.sas7bdat('C:/Users/Ruth/OneDrive/Cuso de Master II ciclo/Machine Learning/Practica2/incident_final.sas7bdat')libary
library(readxl)
datos<- read_excel("C:/Users/Ruth/Desktop/20 marca.xlsx")

#datos<-datos[,-c(1,2)] Ruth:cuando quieres omitir las primeras columnas
summary(datos)

#eliminar columnas
datos$Marca<-NULL
datos$Cupo<-NULL
datos$FechaAltaCliente<-NULL
datos$Cliente<-NULL
datos$Gerencia<-NULL
datos$SubCanal<-NULL

#datos$num_dscto<NULL

#transformar variables cuantitativas a cualitativas como binarias
datos$Estrato<-factor(datos$Estrato)
datos$objetvo<-factor(datos$objetvo)
datos$Region<-factor(datos$Region)
#datos$Gerencia<-factor(datos$Gerencia)
#datos$SubCanal<-factor(datos$SubCanal)
datos$TipoPoblacion<-factor(datos$TipoPoblacion)


#datos$numMissing<-factor(datos$numMissing)

summary(datos)
#View(datos)
#Vemos el reparto de 0s y 1s
prop.table(table(datos$objetvo))

# cambiar var depediente por Yes/NO
#datos$objetvo<-ifelse(datos$objetvo=="Yes",1,0)

## Partici???n de datos
set.seed(12345)
partitionIndex <- createDataPartition(datos$objetvo, p=0.8, list=FALSE)
data_train <- datos[partitionIndex,]
data_test <- datos[-partitionIndex,]

#Modelo inicial sin transf
modeloInicial<-glm(objetvo ~.,data_train[,1:9],family=binomial) #num de columnas
summary(modeloInicial)
compararLog(modeloInicial,data_train,"objetvo",'1') #la variable binaria objetivo tiene q estar en 0 y 1, si no el codigo no corre
compararLog(modeloInicial,data_test,"objetvo",'1')
pseudoR2(modeloInicial,data_train,"objetvo")
pseudoR2(modeloInicial,data_test,"objetvo")

#Importancia de las variables
par(mar=c(5.1, 15, 4.1, 2.1))#ajustar el segundo valor si no cabe el nombre de las variables
impVariablesLog(modeloInicial,"objetvo")

#ROC
(curvaRocTrain<-roc(data_train$objetvo, predict(modeloInicial,data_train,type = "response"), direction="<"))
(curvaRocTest<-roc(data_test$objetvo, predict(modeloInicial,data_test,type = "response"), direction="<"))
plot(curvaRocTrain)
plot(curvaRocTest,add=T,col=2)

#Selecci???n manuaal
modeloManual<-glm(objetvo~accede_prom+num_prom_ofre+num_compras+prom_Nr+Region+TipoPoblacion+Estrato+EF,data_train,family=binomial)#aqui poner solo las variables de interes, arriba de la aleatoria
summary(modeloManual)
compararLog(modeloManual,data_train,"objetvo",'1')
compararLog(modeloManual,data_test,"objetvo",'1')
pseudoR2(modeloManual,data_train,"objetvo")
pseudoR2(modeloManual,data_test,"objetvo")

#Importancia de las variables
impVariablesLog(modeloManual,"objetvo")

#ROC
(curvaRocTrain<-roc(data_train$objetvo, predict(modeloManual,data_train,type = "response"), direction="<"))
(curvaRocTest<-roc(data_test$objetvo, predict(modeloManual,data_test,type = "response"), direction="<"))
plot(curvaRocTrain)
plot(curvaRocTest,add=T,col=2)

modeloManual$rank
modeloInicial$rank

#Seleccion variables
#aic
null<-glm(objetvo~1,data=data_train,family=binomial)
full<-glm(objetvo~.,data=data_train,family=binomial)
modeloForwardAIC<-step(null, scope=list(lower=null, upper=full), direction="forward")
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")

roc(data_test$objetvo, predict(modeloForwardAIC,data_test,type = "response"), direction="<")
roc(data_test$objetvo, predict(modeloStepAIC,data_test,type = "response"), direction="<")
compararLog(modeloForwardAIC,data_test,"objetvo",'1')
compararLog(modeloStepAIC,data_test,"objetvo",'1')
pseudoR2(modeloForwardAIC,data_test,"objetvo")
pseudoR2(modeloStepAIC,data_test,"objetvo")

modeloForwardAIC$rank
modeloStepAIC$rank

#bic
modeloForBIC<-step(null, scope=list(lower=null, upper=full), direction="forward",k=log(nrow(data_train)))
modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))

roc(data_test$objetvo, predict(modeloForBIC,data_test,type = "response"), direction="<")
roc(data_test$objetvo, predict(modeloStepBIC,data_test,type = "response"), direction="<")
compararLog(modeloForBIC,data_test,"objetvo",'1')
compararLog(modeloStepBIC,data_test,"objetvo",'1')
pseudoR2(modeloForBIC,data_test,"objetvo")
pseudoR2(modeloStepBIC,data_test,"objetvo")

modeloStepBIC$rank
modeloForBIC$rank

## Regresion LASSO
y <- as.double(as.matrix(data_train[,3])) # indicar la columna de la Variable Objetivo
x<-model.matrix(objetvo~., data=data_train)[,-1]

set.seed(12345)
cv.lasso <- cv.glmnet(x,y,family = "binomial", type.measure="auc")#class para MISC
plot(cv.lasso)
coef(cv.lasso, s=cv.lasso$lambda.1se)

#modificamos los datos test para obtener su AUC y usamos otra funci???n para el resto de estad???sticos
x_test<-model.matrix(objetvo~., data=data_test)[,-1]
roc(data_test$objetvo, as.numeric(predict(cv.lasso,x_test,type = "response",s="lambda.1se")), direction="<")
compararLogLASSO(cv.lasso,data_test,"objetvo",'1')

pseudoR2LASSO(cv.lasso,data_train,"objetvo")
pseudoR2LASSO(cv.lasso,data_test,"objetvo")

#Interacciones
formulaInteracciones<-function(data,posicion){
  listaFactores<-c()
  lista<-paste(names(data)[posicion],'~')
  nombres<-names(data)
  for (i in (1:length(nombres))[-posicion]){
    lista<-paste(lista,nombres[i],'+')
    if (class(data[,i])=="factor"){
      listaFactores<-c(listaFactores,i)
      for (j in ((1:length(nombres))[-c(posicion,listaFactores)])){
        lista<-paste(lista,nombres[i],':',nombres[j],'+')
      }
    }
  }
  lista<-substr(lista, 1, nchar(lista)-1)
  lista
}

formInt<-formulaInteracciones(data_train,3) #1 indica el numero de columna donde se ubica la variable objetivo en la base de datos
fullInt<-glm(formInt, data=data_train, family=binomial)
#no hacemos back porque no se puede ajustar el modelo completo
#AIC #Ruth:no lo corro por que demora muchisimo
modeloForwIntAIC<-step(null, scope=list(lower=null, upper=fullInt), direction="forward")
modeloStepIntAIC<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
roc(data_test$resuelto_menos_4h, predict(modeloForwIntAIC,data_test,type = "response"), direction="<")
roc(data_test$resuelto_menos_4h, predict(modeloStepIntAIC,data_test,type = "response"), direction="<")
compararLog(modeloForwIntAIC,data_test,"resuelto_menos_4h",'1')
compararLog(modeloStepIntAIC,data_test,"resuelto_menos_4h",'1')
pseudoR2(modeloForwIntAIC,data_test,"resuelto_menos_4h")
pseudoR2(modeloStepIntAIC,data_test,"resuelto_menos_4h")
#BIC
modeloForwIntBIC<-step(null, scope=list(lower=null, upper=fullInt), direction="forward",k=log(nrow(data_train)))
modeloStepIntBIC<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
roc(data_test$objetvo, predict(modeloForwIntBIC,data_test,type = "response"), direction="<")
roc(data_test$objetvo, predict(modeloStepIntBIC,data_test,type = "response"), direction="<")
compararLog(modeloForwIntBIC,data_test,"objetvo",'1')
compararLog(modeloStepIntBIC,data_test,"objetvo",'1')
pseudoR2(modeloForwIntBIC,data_test,"objetvo")
pseudoR2(modeloStepIntBIC,data_test,"objetvo")


modeloForwIntBIC$rank
modeloStepIntBIC$rank


data_train$robjetvo <-NULL
#Validaci???n cruzada repetida
## Validaci???n cruzada repetida
total<-c()
#modelos<-list(modeloInicial,modeloManual,modeloStepAIC,modeloStepIntAIC,modeloStepBIC,modeloStepIntBIC)
modelos<-list(modeloInicial,modeloManual,modeloStepAIC,modeloStepBIC,modeloForwardAIC,modeloForBIC,modeloForwIntBIC,modeloStepIntBIC)
formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
             method = "glm",family="binomial",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  auxVarObj<-data_train$objetvo
  data_train$robjetvo<-make.names(data_train$objetvo) #formateo la variable objetivo para que funcione el codigo
  set.seed(1712)
  vcr_roc <- train(as.formula(formulaModelos[[i]]), data = data_train,
                   method = "glm", family="binomial", metric = "ROC",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 5, repeats = 20,
                                            summaryFunction=twoClassSummary,
                                            classProbs=TRUE,
                                            returnResamp="all"
                   )
  )
  data_train$objetvo<-auxVarObj #recupero la variable objetivo en su formato
  total<-rbind(total,data.frame(accuracy=vcr$resample[,1],roc=vcr_roc$resample[,1],modelo=rep(paste("Modelo",i),
                                                                                              nrow(vcr$resample))))
}
#falta el lasso
set.seed(12345)
vcr<-train(objetvo~., data = data_train,
           method = "glmnet",family="binomial",
           tuneGrid=expand.grid(.alpha=1,.lambda=cv.lasso$lambda.1se),
           trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                    returnResamp="all")
)
auxVarObj<-data_train$objetvo
data_train$objetvo<-make.names(data_train$objetvo) #formateo la variable objetivo para que funcione el codigo
set.seed(12345)
vcr_roc <- train(objetvo~., data = data_train,
                 method = "glmnet", family="binomial", metric = "ROC",
                 tuneGrid=expand.grid(.alpha=1,.lambda=cv.lasso$lambda.1se),
                 trControl = trainControl(method = "repeatedcv",
                                          number = 5, repeats = 20,
                                          summaryFunction=twoClassSummary,
                                          classProbs=TRUE,
                                          returnResamp="all"
                 )
)
data_train$objetvo<-auxVarObj #recupero la variable objetivo en su formato
total<-rbind(total,data.frame(accuracy=vcr$resample[,1],roc=vcr_roc$resample[,1],modelo=rep("LASSO",nrow(vcr$resample))))

par(mar=c(5.1,5.1 , 6, 2.1)) #ajusto el margen superior
boxplot(accuracy~modelo,data=total,main="Tasa de acierto")  #Ruth: volver a ejecutar desde validación cruzada
axis(3, at=1:length(modelos), labels=sapply(modelos,function(x) length(coef(x))), cex.axis=1)
boxplot(roc~modelo,data=total,main="área bajo la curva ROC")
axis(3, at=1:length(modelos), labels=sapply(modelos,function(x) length(coef(x))), cex.axis=1)
aggregate(roc~modelo, data = total, mean) 
aggregate(roc~modelo, data = total, sd)

#vemos la fórmula y las variables más importantes en el modelo ganador
formula(modeloStepIntBIC)
par(mar=c(5.1, 18, 4.1, 2.1))#ajustar el segundo valor si no cabe el nombre de las variables
impVariablesLog(modeloStepIntBIC,"resuelto_menos_4h")
compararLog(modeloStepIntBIC,data_test,"resuelto_menos_4h",'1')

modeloStepIntBIC$coefficients

#Busqueda del mejor punto de corte para el ganador
test_roc<-roc(data_test$resuelto_menos_4h, predict(modeloStepIntBIC,data_test,type = "response"), direction="<")
plot(test_roc,print.thres="best") #punto de corte maximiza youden
plot(test_roc,print.thres=c(0.482,0.5)) #comparo con 0.5 el que he observado antes
#se pueden a???adir otros puntos de corte que se desee comparar


###pREDICT:
Datos_Nuevos <- read_xlsx("C:/Users/Ruth/Downloads/test.29.xlsx")
Datos_Nuevos <- Datos_Nuevos[,-2]
Datos_Nuevos <- Datos_Nuevos[,-3]

clase.pred <- predict(modeloManual,Datos_Nuevos,type = c("link", "response", "terms"))#class
proba.pred <- predict(modeloManual,Datos_Nuevos,type = "prob")
proba.pred <- proba.pred[,2]
head(cbind(Datos_Nuevos,clase.pred,proba.pred))

###
predict(object, newdata = NULL,
        type = c("link", "response", "terms"),
        se.fit = FALSE, dispersion = NULL, terms = NULL,
        na.action = na.pass, ...)

