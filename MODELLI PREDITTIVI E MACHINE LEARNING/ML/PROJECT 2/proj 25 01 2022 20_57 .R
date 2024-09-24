---
  title: "PROGETTO DATA MINING MILLONE ROSSI ZANINI PEDRINI"
author: "millone"
date: "28/12/2021"
output: word_document
---
#write.csv2(d, "export_dataset.csv")
#importiamo il dataset modificato da Excel e togliamo notazione scientifica
options(scipen=999,digits = 3)
library(readr)
s <- read_delim("dataset_ok_prova3.csv", 
                delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(s)
table(s$MIS_Status)#dataset non sembra rappresentativo 
prop.table(table(s$MIS_Status))
 summary(s)
##RICODIFICA VARIABILI
#alcune variabili sono classificate incorrettamente da R, ricodifichiamo le variabili e 
#binarizziamo quelle necessarie.
library(car)
s$NewExist <- as.factor(s$NewExist)
s$FranchiseCode <- as.factor(s$FranchiseCode)
s$UrbanRural <- as.factor(s$UrbanRural)
s$RevLineCr <- as.factor(s$RevLineCr)

s$MIS_Status <- recode(s$MIS_Status, recodes=" 0='c0';1='c1'")
s$MIS_Status <- as.factor(s$MIS_Status)

table(s$MIS_Status)

MIS_Status<-recode(s$MIS_Status, recodes="'c0'=0; else=1")
table(MIS_Status)

#s$MIS_Status <- NULL#CANCELLIAMO TARGET INIZIALE ,PERCHE CREA PERFETTA SEPARSTION
#head(s)
   




#DATI MANCANTI
#Prima di tutto verifichiamo quali variabili hanno troppi dati mancanti --> eliminarle!
library(funModeling)
library(dplyr)
require(funModeling)
status=df_status(s, print_results = F);status
head(status%>% arrange(type))
head(status%>% arrange(unique))
head(status%>% arrange(-p_na))
#eliminiamo RevLineCr e ChgOffDate  e disurbementdate(indica solo una data) perch? hanno una percentuale di dati mancanti elevata
s <- s[,-c(12,13,14)]

# divide data as factor and numeric
isfactor <- sapply(s, function(x) is.factor(x))
isfactor

# select factor var in a dataframe
factordata <- s[, isfactor]
str(factordata)

numeric <- sapply(s, function(x) is.numeric(x))
numeric <-s[, numeric]
str(numeric)
mis_status<-factordata$MIS_Status
numeric_CBIND <-cbind(numeric, mis_status)


#DENSITA GRAFICI 
library(caret)
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=numeric_CBIND[,-12], y=numeric_CBIND$mis_status, plot="density", scales=scales)
#                              METTERE TARGET 
#library(PerformanceAnalytics)
#chart.Correlation(numeric , histogram=TRUE, pch=22)
numeric2<-numeric[,-c(8)]
require(corrgram)
corrgram(numeric2, lower.panel = panel.cor, cex=1, cex.labels = 1)

#library(PerformanceAnalytics)
#chart.Correlation(s , histogram=TRUE, pch=22)

#library(PerformanceAnalytics)
#chart.Correlation(numeric , histogram=TRUE, pch=22)


library(VIM)
missingness<- aggr(s, col=c('navyblue','yellow'),
                   numbers=TRUE, sortVars=TRUE,labels=names(s), cex.axis=.7,gap=2)



# 1 count missing data####
sapply(numeric, function(x)(sum(is.na(x)))) # NA counts

# impute with bagging (we will treat it as model)



#'CARET FA DUE TIPI DI IMPUTAZIONE O CON BAGIMPUTATE O NAREST NEIGHBOUR
#'(VIENE USATO PER IMPUTARE PER IL CONCETTO DI APPAIAMNETO,NON PIU CON IL
#' CONCETTO DI MODELLO CLASSIFICATOVO,UNICO SVANTAGGIO E CHE SE APPLICHIAMO 
#' CODESTO NELL IMPUTAZIONE LUI VI GENERA DELLE COVARIATE STD (IL NEAREST NEIGHBOUR
#'  CUOLE VARIBILI STD O NORMALIZZA)) 

#COMANDO PREPROCESSING 


#install.packages("caret", dependencies = c("Depends", "Suggests"))    
library(caret)
set.seed(1234)             
preProcVal <- preProcess(numeric, method = "bagImpute")#BAGGING METODO ROBUSTO E SVOLGE IMPUTAZIONE COME ALBERI (TANTI ALBERI METODO ANSAMBLE CHE USANO AGGREGAZIONE DI TANTI ALBERI ),PRENDE OGNI COVARIATA CON DATI MANCANTI E LA PONE COME TARGET CI RILANCIA UNA SERIE DI ALBERI PER CUI ALLA FINE IL VALORE IMPUTATO E IL VALORE REVISTO DEL TARGET DI QUESTA VARIBILE 
preProcVal#PREPROCESSING IN 2 STEP PRIMA GENERO FUNZIONE E POI GENERO I DATI (CARETE IMPUTA SOLO VARIBILI NUMERICHE ,CATEGORIALI NON HANNO BISOGNO ->DOVREMMO DUMMYZZARLE E POI IMPUTARLE ,SE NO METODO NAIVE OPPURE MICE )
# NB se target non ? factor levarlo dal datset del proprocessing...non si imputa usando il target , solo le X


#Applichiamo ai nostri dati il preprocessing svolto sopra
imputed=predict(preProcVal, newdata = numeric) #DT IMPUTATO DEL BLOCCO NUMERIHE 


#VERIFICHIAMO SE CARET HA IMPUTATO TUTTO
sapply(imputed, function(x)(sum(is.na(x))))
library(funModeling)
library(dplyr)
status=df_status(imputed, print_results = F);status  

#PROF VUOLE CHE CI SIANO DUE DT UNO CON TUTTE LE COVARIATE E LALTRO CON 
#SOLO LE COVARIATE SELEZIONATE DA ALBERO BAGGING

#DT UNO CON TUTTE LE COVARIATE####################################################################
nuovo <- cbind(imputed, factordata)
sapply(nuovo, function(x)(sum(is.na(x))))
#Ora che abbiamo svolto l'imputazione, dividiamo il dataset in training e validation
split <- createDataPartition(y=nuovo$MIS_Status, p = 0.60, list = FALSE)
train <- nuovo[split,]#dt che ha tutte le covariate 
test <- nuovo[-split,]
#togliamo variabilie chgoffprinpr ,perche e una covariata che e simile alla target 
train_chgoff<-train[,-c(9)]
test_chgoff<-test[,-c(9)]
table(train_chgoff$MIS_Status)/nrow(train_chgoff)
table(test_chgoff$MIS_Status)/nrow(test_chgoff)
#Controllare che la distribuzione del target sia simile tra train e test
table(train$MIS_Status)/nrow(train)
table(test$MIS_Status)/nrow(test)
#perfetto, target ugualmente distribuito nei due dataset
#c0    c1 
#0.176 0.824

#####################################################################


table(s$ChgOffPrinGr,s$MIS_Status)
plot(s$MIS_Status,s$ChgOffPrinGr);ChgOffPrinGr 





#############################################################################################
#ALBERO NORMALE,selezionatore di varibli  #############################################################################

#VOLGIAMO NEAREST NEIGHBOUR ,COSA RICHIEDE PREPROCESSING ->COLLINEARITA MODEL SELCTION E NORMALIZZARE COVARIATE 
#_QUINIDI DOBBIAMO ELEIMNARE LE VRIBILI UNA ALLA VOLTA MA USEREMO UN MRTODO PIU VELOCE CHE E ALBERO
# 2. using tree, tree not require preprocess
# tune a tree with Cv on training  
#PER FARE MODEL SELECTION USIAMO METRICA DI ACCURACY CROS VALIDATA 

set.seed(1234)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", classProbs = TRUE,summaryFunction = twoClassSummary)

rpartTuneCvA <- train(MIS_Status ~ ., data = train_chgoff, method = "rpart",
                      tuneLength =5,
                      trControl = cvCtrl,na.action = na.pass)#na.action=na.pass
rpartTuneCvA
# best accuracy using best cp

# var impp of the tree
varImp(object=rpartTuneCvA)#GUARDIAMO VARBILI IMPORTANTI ,FORTE TRA CONTRAPPOSIZIONE 
#TRA VARIBILI IMPORTANTI E VARIBILI NULLE ,QUINIDI PORTIAM AVANT LE VARIBILI CHE HANNO VALORE MAGGIORE DI 0
plot(varImp(object=rpartTuneCvA),main="train tuned - Variable Importance")
#LE VARIBILI DA ELIMNARE POTREBBERO ESSERE SURROGATE(NON SOLO ELIMNATE TOGLIENOLE ),
#CIOE AVREBBERO DATO STESSA IMPURITA ,ALBERO ELIMINA VARIBILI SURROGATE GI ADI DEFAULT 

# summary performance (best cp)
getTrainPerf(rpartTuneCvA)

#ESWTRIAMO VARIBILI IMPOTANTI IN UN DT 
vi=as.data.frame(rpartTuneCvA$finalModel$variable.importance);vi
head(vi)

pastelist=cat(paste(shQuote(rownames(vi), type="cmd"), collapse=", "));pastelist
viname=row.names(vi)
#"Term", "ApprovalFY", "RetainedJob", "NAICS", "UrbanRural1", "SBA_Appv", "GrAppv", "CreateJob", "DisbursementGross"
# cut and paste and create a list of x+ target var
NameList <- c("Term", "ApprovalFY", "RetainedJob", "NAICS", 
              "UrbanRural1", "SBA_Appv", "GrAppv", "CreateJob", 
              "DisbursementGross", "MIS_Status")
training2 <- train_chgoff[,colnames(train_chgoff) %in% NameList] #dt train con variabili importanti 
#testing2 <- train[,colnames(train) %in% NameList] 
#train2=train[,viname]
#???train2=cbind(train$MIS_Status, train2)#dt train con variabili importanti 
#train2 mi crea una variabile in piu ,quindi uso training2
#names(train2)[1] <- "MIS_Status"
#head(train2)
prop.table(table(training2$MIS_Status))



###########################################################################################



#LOGISTICO#############################################################################

#'IL MODELLO LOGISTICO NON GODE DI UNA CROSS VALIDATION PERCHE NON 
#'HA NESSUN PARAMETRO DI TUNING 
#'QUINDI NON FARA TUNING ,QUINDI CI DARA QUEL MODELLO 
#'LOGISTICO APPLICATO ALLE VARIBILI PREPROCESSATE 

#SVOLGIAMO MODELLO LOGISTICO FADEDO MODEL SELECTION 
#(INNAZITUTTO LOGISTICO NONRICHIEDE OBBLIGATORIO MODEL SELECTION ),
#POI UNO STEP SUCCESSIVO IN UN DT CHE PRESENTA MOLTE COVARIATE 

# scale and boxcox not required in logistic equation


#logit=caretModelSpec (method="glm"                     , preProcess=c("corr","nzv","BoxCox"))

metric="ROC"
set.seed(1234)
Control=trainControl(method= "cv",number=10,classProbs = TRUE,
                     summaryFunction = twoClassSummary)
glmPP=train(MIS_Status~. ,data=training2, method = "glm", preProcess=c("corr", "nzv"), 
            trControl = Control, tuneLength=5, trace=TRUE, metric=metric, 
            na.action=na.pass)
glmPP

# 1.2 tune different model in caret #######
#     stabilire quale metrica usare per i vari models e tunarli tutti con questa metrica#####

# fate tenfold validation k=10, qui con pochi dati k=5

confusionMatrix(glmPP)#Accuracy (average) : 0.9829
summary(glmPP)

P_Pred <- predict(glmPP, training2, type = "prob")#CALCOLIAMO LE PREVISTE SU TRAINNING ,SALVA COME POST IRIOR ,CI DA PROBABILITA POSTERIRORI DI ESSERE SIA DI CLASSE 1 O 0 PER ENTRAMBE LE CLASSI DEL TARGET 
head(P_Pred)

#' predicted target,QUINDI ANDREMO A RICAVARE I VALORI DEL TARGET PREVISTO SUL DT DI
#'  TRAINNG ANDANDO A INVOCARE LA FUNZIONE RAW CHE APPLICA SOGLIA DI 0.5 PER PASSARE
#'   DALLA POST IRIOR AL TARGET PREVISTO 
y_Pred <- predict(glmPP, training2, type = "raw")
head(y_Pred)


confusionMatrix(y_Pred, training2$MIS_Status)#funziona :)
length(y_Pred)
length(training2$MIS_Status)

##############################################################################


#PLS############################################################################
set.seed(1234)
#pls  on selected covariates
Control=trainControl(method= "cv",number=10, classProbs=TRUE,
                     summaryFunction = twoClassSummary)
pls=train(MIS_Status~.,data=training2 , method = "pls", 
          trControl = Control, tuneLength=5,metric=metric,na.action=na.pass)
pls
plot(pls)
confusionMatrix(pls)#Accuracy (average) : 0.9829
#summary(pls) non va 

P_Pred_pls <- predict(pls, training2, type = "prob")#CALCOLIAMO LE PREVISTE SU TRAINNING ,SALVA COME POST IRIOR ,CI DA PROBABILITA POSTERIRORI DI ESSERE SIA DI CLASSE 1 O 0 PER ENTRAMBE LE CLASSI DEL TARGET 
head(P_Pred_pls)

# predicted target,QUINDI ANDREMO A RICAVARE I VALORI DEL TARGET PREVISTO SUL DT DI TRAINNG ANDANDO A INVOCARE LA FUNZIONE RAW CHE APPLICA SOGLIA DI 0.5 PER PASSARE DALLA POST IRIOR AL TARGET PREVISTO 
y_Pred_pls <- predict(pls, training2, type = "raw")
head(y_Pred_pls)


confusionMatrix(y_Pred_pls, training2$MIS_Status)#accuracy bassa 0.80 e kappa molto basso 0.193
length(y_Pred_pls)
length(training2$MIS_Status)
#############################################################################




#LASSO############################################################################

#glmnet=caretModelSpec(method="glmnet", tuneGrid=Grid3  , preProcess=c("scale","nzv")       )


library(caret)
set.seed(1234)
metric <- "ROC"
grid = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
Control=trainControl(method= "cv",number=10, classProbs=TRUE,
                     summaryFunction = twoClassSummary)
lasso_PP=train(MIS_Status~., data=train_chgoff, method = "glmnet", family ="binomial",
               trControl = Control, tuneLength=5, tuneGrid=grid, 
               na.action=na.pass , metric=metric,preProcess=c("scale","nzv"))
lasso_PP
plot(lasso_PP)
#summary(lasso_PP)
#na.action=na.pass
confusionMatrix(lasso_PP)#Accuracy fa schifo 0.81 
#summary(lasso_PP)
#Cross-Validated (10 fold) Confusion Matrix 
#(entries are percentual average cell counts across resamples)
#Reference
#Prediction   c0   c1
#c0  0.0  0.0
#c1 18.2 81.8
#Accuracy (average) : 0.8179
P_Pred_lasso <- predict(lasso_PP, train, type = "prob")#CALCOLIAMO LE PREVISTE SU TRAINNING ,SALVA COME POST IRIOR ,CI DA PROBABILITA POSTERIRORI DI ESSERE SIA DI CLASSE 1 O 0 PER ENTRAMBE LE CLASSI DEL TARGET 
head(P_Pred_lasso)

# predicted target,QUINDI ANDREMO A RICAVARE I VALORI DEL TARGET PREVISTO SUL DT DI TRAINNG ANDANDO A INVOCARE LA FUNZIONE RAW CHE APPLICA SOGLIA DI 0.5 PER PASSARE DALLA POST IRIOR AL TARGET PREVISTO 
y_Pred_lasso <- predict(lasso_PP, train, type = "raw")
head(y_Pred_lasso)


confusionMatrix(y_Pred_lasso, train$MIS_Status)#funziona :)
length(y_Pred_lasso)
length(train$MIS_Status)
#############################################################################


#RANDOM FOREST###############################################################################
#seleziono variabili importanti con rf 

# AIM: fit and  tuning random forest using a metric of interest ######

# RF is the most used algoritmh, simple, well performing, no preprocessing require.
# mainly very useful to extract var importance...being a model selector for oter models requiring it######



# normal tuning with caret: mtry number of var maximizing a metric of interest....######

# nb event=positive class is the first: 0 vs 1  no vs yes
# then Spec is the prob to capture true "yes" (marketing campaign)

#'STIAMIMO DT CON RF,VISTO CHE E SBILANCIATO DOBBIAMO CORREGERE 
#'USEREMO METRICA DI SESITIVITY/SPECIFICITY PIUTTOSTO CHE PRECISION
#'FITTIAMO MODELLO CON RF,SPECIFICHIAMO METRICA SENS ,SPECIFICHIAMO PARAMETRO CHE NON E PARTICOLARMENTE SIGNIFICATIVO 
#' DOBBIMAO DIRGLI DI TUNARCI IL NUMERO DI PREDITTORI DA CONSERVARE IN OGNI CAMPIONE BOOT
#' MASSIMIZZANDO LA SPECIFITY summaryFunction = twoClassSummary ANDARE A SPECIFICARE 
#' QUESTO COMANDO SE NO ANDRA A TUNARCI USANDO ACCURACY ,
#' SE VOLESSIMO TUNARE ATTRAVERSO KAPPA DOVREI SPECIFICARLO 
#' POIDOVREMO RICORDARCI DI FISSARE UN MESE CASUALE PER AVERE DATI RIPRODUCIBILI
#' IL SEME CI DICE CHE STIAMO APPLICANDO UN BOOTSTRAP DENTRO OGNI CAMPIONE 
#' 
#' 

# OR SELECT using random forest#####

library(caret)
library(caretEnsemble)
#####albero che ha tunegrid e tunelength ,non vanno messi assieme
#set.seed(1234)
#control <- trainControl(method="cv", number=10, search="grid", 
#                        summaryFunction = twoClassSummary,classProbs = TRUE)
# <- expand.grid(.mtry=c(1:5))#PER BRAIMAN IL NUMERO DI COVARIATE ADATTE E LA 
#RADICE QUADRATE DEL NUMERO INIZIALE RAD QUADRATA DII 60 ,NEL CODICE METTERE
#IL TUNAGGIO SU UNA GRIGLIA CON VALORE CENTRATO SUGGERITO DA BRAIMAN(IMPORTANTE CHE VALORE SIA COMPRESO) 

#rfTune <- train(MIS_Status~., data=train, method="rf", metric=metric, tuneLength = 5, tuneGrid=tunegrid, 
#   ntree=250, trControl=control,na.action=na.pass)

#rfTune


#y_Pred_rf <- predict(rfTune, train, type = "raw")
#head(y_Pred_rf)


#confusionMatrix(y_Pred_rf, train$MIS_Status)#funziona :)



#useremo questo 
set.seed(1234)
control <- trainControl(method="cv", number=10, search="grid", 
                        summaryFunction = twoClassSummary,classProbs = TRUE)
tunegrid <- expand.grid(.mtry=c(1:5))
rfTune1 <- train(MIS_Status~., data=train_chgoff, method="rf", metric=metric, tuneGrid=tunegrid, 
                ntree=250, trControl=control)
rfTune1
y_Pred_rf1 <- predict(rfTune1, train, type = "raw")
confusionMatrix(y_Pred_rf1, train$MIS_Status)#funziona :)
confusionMatrix(rfTune1)







#####
#set.seed(1234)
#control <- trainControl(method="cv", number=10, search="grid", 
#                        summaryFunction = twoClassSummary,classProbs = TRUE)
#rfTune2 <- train(MIS_Status~., data=train, method="rf", metric=metric, tuneLength=5, 
#                ntree=250, trControl=control,na.action=na.pass)
#rfTune2
#######

#il miglior modello (usando come metrica la specificity) Ã¨ quello con mtry=1

plot(rfTune1)
#PLOTTIAMO LE MISURE DI TUNING 


# performance by mtry
rfTune1$results #metriche di confronto per ogni modello

# final model and performance
getTrainPerf(rfTune1) #Mostra modello vincente, attenzione al valore basso di sensitivity


# final model and performance....which is the mean of 10 folds
mean(rfTune1$resample$ROC)
mean(rfTune1$resample$Spec)

# want save metric of resamples?????
z=data.frame(rfTune1$resample)
head(z)


#Matrice di confusione per valutare la bontÃ  classificativa del modello
confusionMatrix(rfTune1)
#Modello buono, accuracy=0.888


# VAR IMPORTANCE######,IMPOTANZE VARIABILI 
#'SICCOME MOLTE COVARIATE SONO IMPORTANTI,CIOE SIGNIFICA CHE (PERCHE HO GENERATO 150 ALBERI 
#'MA SOPRATUTTO PERCHE HO SELEZIONATO 
#'QUASI TUTTE LE COVARIATE )#QUASI MAI AVREMO VARIBILI CON IMPORTANZA ZERO ,LUNICO 
#'ASPETTO SOGGETTIVO DI UNA RANDOM FOREST A CHE LIVELLO TAGLIARE IMPORTANZA 

#Plottiamo importanza delle variabili
Vimportance <- varImp(rfTune1)
plot(Vimportance)
#Si nota che le prime tre variabili siano le piÃ¹ importanti, le rimanenti sono poco importanti




################################################################################

#GRADIENT BOOST############################################################################

# altro esempio gradient boosting con parametri fissi######
set.seed(1234)
control <- trainControl(method="cv", number=10, summaryFunction = twoClassSummary, classProbs = TRUE)
gbmFit4 <- train(MIS_Status ~ ., data = train_chgoff, 
                 method = "gbm", 
                 trControl = control, 
                 verbose = FALSE, 
                 tuneGrid = data.frame(interaction.depth = 4,
                                       n.trees = 100,
                                       shrinkage = .1,
                                       n.minobsinnode = 20),
                 metric = "ROC",na.action=na.pass)

gbmFit4

y_Pred_gbm <- predict(gbmFit4, train, type = "raw")
head(y_Pred_gbm)


confusionMatrix(y_Pred_gbm, train$MIS_Status)#funziona :)
confusionMatrix(gbmFit4)

#trellis.par.net(caretTtheme())
#plot(gbmFit4,metric="ROC")
#ggplot(gbmFit4)
#RETI NEURALI #########################################################################################################################################################
#'QUINDI IN SOSTANZA FACCIO STEPAIC(O CON ALBERI ) POI FACCIO COLLINEARITA 
# moreover this is an optimistic metric on train...tune using cv

# we missed scaling inputs...

# use caret...more tuning and preporcessing######

# Try the following:
# 1. preprocess the data AND
# 2. ESPECIALLY tune the architecture (size=#hidden neurons) and decay using cv
# tuning  decay and neurons

# size=(number of units in the hidden layer)
# Weight decay= lambda, penalizes C(wij), the sum of squares of the weights wij
# scale inputs
library(caret)
set.seed(1234)
metric <- "ROC"
ctrl = trainControl(method="cv", number=10, search = "grid",summaryFunction = twoClassSummary, classProbs = TRUE)
# remember THIS: insted of using  y ~ ??????. data=train
# if you use    x, y, from x group remove the target!!!! 
nnetFit_glm <- train(training2[-1], training2$MIS_Status,
                     method = "nnet",
                     preProcess = c("scale","corr","nzv"), 
                     metric=metric, trControl=ctrl,tuneLength = 5,
                     trace = FALSE, # use true to see convergence
                     maxit = 100)#maxit mettere 100
nnetFit_glm
plot(nnetFit_glm)
getTrainPerf(nnetFit_glm) 
#"> getTrainPerf(nnetFit_glm) 
#TrainROC TrainSens TrainSpec method
#1        1         1         1   nnet

require(ModelMetrics) #library to calculate performance measures

#overall performance
#measure logloss (the lower the better)
y_Pred_nnet <- predict(nnetFit_glm, training2, type = "raw")

confusionMatrix(y_Pred_nnet,training2$MIS_Status)
confusionMatrix(nnetFit_glm)

#tuneGrid=tunegrid
#tunegrid <- expand.grid(size=c(1:5), decay = c(0.001, 0.01, 0.05 , .1, .3))
#method="nnet",   tuneGrid=Grid0  , preProcess=c("scale","corr","nzv"), trace=FALSE)




#Cross-Validated (10 fold) Confusion Matrix 

#(entries are percentual average cell counts across resamples)

#Reference
#Prediction   c0   c1
#c0 17.6  0.0
#c1  0.0 82.4

#Accuracy (average) : 1



# tuning strategy 3): grid search attorno ai default parameters ######
# of caret with a tunelength (5 combination size*decay)


# it did not converge before...amen!!


##########################################################################################################################################################
#KNN####################################
#Grid1 = expand.grid(.k      =seq(1,20, by=3)               )

set.seed(1234)
ctrl =trainControl(method="cv", number = 10, classProbs = T,
                   summaryFunction=twoClassSummary)
grid = expand.grid(k=seq(5,20,3))#MIGLIOR VALORE KAPPA TUNATO IN UN A GRILIA DA 5 E 20 CON PASSO DI 3 (QUINDI 1°:5 ,2°:8 ,3°:11,ECC)
knn=train(MIS_Status~.,
          data=training2,method = "knn",
          trControl = ctrl, tuneLength=5, na.action = na.pass,
          tuneGrid=grid, preProcess=c("scale","corr"),metric=metric)
knn
plot(knn)
y_Pred_knn <- predict(knn, training2, type = "raw")
head(y_Pred_knn)


confusionMatrix(y_Pred_knn, training2$MIS_Status)


confusionMatrix(knn)
######################################


#NB######################################
set.seed(1234)
ctrl =trainControl(method="cv", number = 10, classProbs = T,
                   summaryFunction=twoClassSummary)
naivebayes=train(MIS_Status~.,
                 data=training2,method = "naive_bayes",
                 trControl = ctrl, tuneLength=5,metric=metric ,na.action = na.pass) 
naivebayes
y_Pred_nb <- predict(naivebayes, training2, type = "raw")
head(y_Pred_nb)


confusionMatrix(y_Pred_nb, training2$MIS_Status)
confusionMatrix(naivebayes)
######################################


##########################################################################################################################################################
#comparazione e soglia ,bisogna mettere stesse numero di fold e numero parametri di tuning
#CREA VETTORE CON TUTTI GLI ALGORITMI PER CONFRONTARLI GRAFICAMENTE CON BOXPLOT
#step2  compare cross validation results  (this is not strictly assessment) ####

library(caret)
library(caretEnsemble)
results <- resamples(list(glm_PreProc=glmPP, plsPP=pls, lasso_PP=lasso_PP, rfTune=rfTune1, rpartTuneCvA=rpartTuneCvA,gradientBoost=gbmFit4,nnet=nnetFit_glm,knn=knn,naivebayes=naivebayes))
?resamples
#knn=knn,naivebayes=naivebayes
bwplot(results,scales = list(relation = "free"))
# plot only range 0.5-1
#bwplot(results,scales = list(relation = "free"),xlim = list(c(.9,1), c(0,3)))
#bwplot(results,scales = list(relation = "free"),xlim = list(c(.95,1), c(0,3)))
#'ALTRO GRAFICVO INTERESSANTE 
#'
#'
splom(results)

#bwplot(results,scales = list(relation = "free"),xlim = list(c(.5,1), c(0,7)))
# test difference of accuracy using bonferroni adjustement
Diffs <- diff(results)
summary(Diffs)
?diff
diff?
#Error in t.test.default(x, ...) : data are essentially constant 
#'CREDO VOGLIA DIRE CHE NON CI SONO DIFFERENZE SIGNIFIOCATIVE DI 
#'CONFRONTO TRA GRUPPI SECONDO BONFERRONI
#'> Diffs
#Error: object 'Diffs' not found


# step2 true assessment (ROC on test set )######
# estimate probs P([,1])-> corrisponde alla probabilita di essere debitore,non restituisce debito 
test_chgoff$glm = predict(glmPP       , test_chgoff, "prob")[,1]
test_chgoff$pls = predict(pls         , test_chgoff, "prob")[,1]
test_chgoff$lasso = predict(lasso_PP    , test_chgoff, "prob")[,1]
test_chgoff$gb = predict(  gbmFit4    , test_chgoff, "prob")[,1]
test_chgoff$rpart = predict(rpartTuneCvA, test_chgoff, "prob")[,1]
test_chgoff$nnet = predict(nnetFit_glm, test_chgoff, "prob")[,1]
test_chgoff$rf = predict(rfTune1      , test_chgoff, "prob")[,1]
test_chgoff$knn = predict(knn      , test_chgoff, "prob")[,1]
test_chgoff$nb = predict(    naivebayes  , test_chgoff, "prob")[,1]

library(pROC)
# roc values
roc.glm=roc(MIS_Status ~ glm, levels=c("c1", "c0"), data = test_chgoff)
roc.pls=roc(MIS_Status ~ pls,levels=c("c1", "c0"), data = test_chgoff)
roc.lasso=roc(MIS_Status ~ lasso,levels=c("c1", "c0"), data = test_chgoff)
roc.gb=roc(MIS_Status ~ gb,levels=c("c1", "c0"), data = test_chgoff)
roc.rpart=roc(MIS_Status ~ rpart,levels=c("c1", "c0"), data = test_chgoff)
roc.nnet=roc(MIS_Status ~ nnet,levels=c("c1", "c0"), data = test_chgoff)
roc.rf=roc(MIS_Status ~ rf,levels=c("c1", "c0"), data = test_chgoff)
roc.knn=roc(MIS_Status ~ knn,levels=c("c1", "c0"), data = test_chgoff)
roc.naivebayes=roc(MIS_Status ~ nb,levels=c("c1", "c0"), data = test_chgoff)
#library(pROC)
#s=roc(LOSSFRQ ~ pred_probrO, levels=c("r1", "r0"), data = test)



plot(roc.glm)#NERA  LOGISTICO                   NO6
plot(roc.pls,add=T,col="red")#PLS               NO 7 
plot(roc.lasso,add=T,col="blue")#LASSO           5
plot(roc.gb,add=T,col="yellow")#GRADIENT BOOST 2(PUO ESSERE ANCHE TERZO )
plot(roc.rpart,add=T,col="violet")#RPART          4
plot(roc.nnet,add=T,col="green")#RETI NEURALI     1
plot(roc.rf,add=T,col="orange")#RANDOM FOREST 3(PUO ESSERE ANCHE SECONDO )
plot(roc.knn,add=T,col="brown")
plot(roc.naivebayes,add=T,col="pink")

plot(roc.knn,col="brown")

plot(r6,col="green")#RETI NEURALI     1
plot(r7,add=T,col="orange")#RANDOM FOREST 3(PUO ESSERE ANCHE SECONDO )
plot(r4,add=T,col="yellow")#GRADIENT BOOST 2(PUO ESSERE ANCHE TERZO )
plot(r5,add=T,col="violet")#RPART          4



# step2 assessment using lift: #####
# funmodeling model the higher class of target thus R
#DA plot roc deduco quale sia migliore modello e uso codesto nelle righe successive 
###
#qua bisogna mettere [,1] perche e la probabilita che ci interessa 
#quella di azzeccare i debitori (ABBIAMO GIA CREATO QUELLE PROBABILITA SONO SOPRA )
###


library(funModeling)
gain_lift(data = test_chgoff, score = 'nnet', target = 'MIS_Status',q_segments=20)
Population  Gain Lift Score.Point
1           5  28.5 5.69   0.9997653
2          10  56.9 5.69   0.9997572
3          15  85.4 5.69   0.9997477
4          20 100.0 5.00   0.0000546
5          25 100.0 4.00   0.0000488
6          30 100.0 3.33   0.0000472

library(funModeling)
gain_lift(data = test_chgoff, score = 'gb', target = 'MIS_Status',q_segments=20)
Population  Gain Lift Score.Point
1           5  26.8 5.37      0.8453
2          10  52.0 5.20      0.7205
3          15  72.2 4.81      0.5007
4          20  84.6 4.23      0.3116#
5          25  89.9 3.59      0.1617
6          30  93.6 3.12      0.1017

library(funModeling)
gain_lift(data = test_chgoff, score = 'rf', target = 'MIS_Status',q_segments=20)
Population  Gain Lift Score.Point
1           5  27.2 5.45       0.896
2          10  52.9 5.29       0.756
3          15  74.7 4.98       0.548
4          20  86.4 4.32       0.320
5          25  92.1 3.69       0.188
6          30  94.9 3.16       0.112





require(caret)
lift_chart <- caret::lift(MIS_Status~nnet+rf+gb , data=test_chgoff, MIS_Status = "c0",cuts = 200)
lift_chart_plot<-ggplot(lift_chart) + labs(title="Competing models Lift Charts")+geom_point(x=20,y=60, colour="red")
lift_chart_plot






#confronto curve roc di t e v del modello migliore per vedere se ce overfitting 
rpart2 = predict(rpartTuneCvA, train_chgoff, "prob")[,1]
test_chgoff$rpart = predict(rpartTuneCvA, test_chgoff, "prob")[,1]
roc.rpart2=roc(MIS_Status ~ rpart2, data = train_chgoff)
roc.rpart=roc(MIS_Status ~ rpart, data = test_chgoff)
plot(roc.rpart2)
plot(roc.rpart,add=T,col="violet")
# non ce overfitting 
roc.rpart2
roc.rpart


rf2 = predict(rfTune1, train_chgoff, "prob")[,1]
test_chgoff$rf = predict(rfTune1, test_chgoff, "prob")[,1]
roc.rf2=roc(MIS_Status ~ rf2, data = train_chgoff)
roc.rf=roc(MIS_Status ~ rf, data = test_chgoff)
plot(roc.rf2)
plot(roc.rf,add=T,col="violet")
roc.rf2 #auc 1 
roc.rf#auc 0.946Z


#nnet uguali auc 
nnet2 = predict(nnetFit_glm, train_chgoff, "prob")[,1]
test_chgoff$nnet = predict(nnetFit_glm, test_chgoff, "prob")[,1]
roc.nnet2=roc(MIS_Status ~ nnet2, data = train_chgoff)
roc.nnet=roc(MIS_Status ~ nnet, data = test_chgoff)
plot(roc.nnet2)
plot(roc.nnet,add=T,col="violet")
roc.nnet2
roc.nnet

#gb 
gb2 = predict(gbmFit4, train_chgoff, "prob")[,1]
test_chgoff$gb = predict(gbmFit4, test_chgoff, "prob")[,1]
roc.gb2=roc(MIS_Status ~ gb2, data = train_chgoff)
roc.gb=roc(MIS_Status ~ gb, data = test_chgoff)
plot(roc.gb2)
plot(roc.gb,add=T,col="violet")
roc.gb2
roc.gb



y=test_chgoff$MIS_Status
y=ifelse(y=="c0",0,1)
predprobC0=test_chgoff$nnet
predprobC0=test_chgoff$rf
predprobC0=test_chgoff$gb

library(ROCR)
predRocc0 <- prediction(predprobC0,y)
# this agree with easly specificity (prob R) where the event was M 
#spazio vuoto ci devo mettere probabilita con soglia 
spec.perf = performance(  predRocc0 , measure = "spec")
plot(spec.perf)#DICE LO STESSO DI QUELLO CHE DICEVA LA LIFT 
###################################################################
sens.perf = performance( predRocc0  , measure = "sens")
plot(sens.perf)
#BRUTTO ,DIFFICILE A INTERPRETARE

#precision
prec.perf = performance(predRocc0, measure = "prec")
plot(prec.perf)
#BRUTTO ,DIFFICILE A INTERPRETARE

acc.perf = performance(predRocc0, measure = "acc")
plot(acc.perf)


cut=as.data.frame(acc.perf@x.values)
colnames(cut)="cut"
head(cut)

spec=as.data.frame(spec.perf@y.values)
colnames(spec)="spec"

sens=as.data.frame(sens.perf@y.values)
colnames(sens)="sens"

acc=as.data.frame(acc.perf@y.values)
colnames(acc)="acc"

prec=as.data.frame(prec.perf@y.values)
colnames(prec)="prec"

#'CDICE CHE MI PERMETTE DI PLOTTARE TUTTE LE METRICHE DEL MODELLO VINCENTE 
all=cbind(cut, spec, sens, acc,prec)

head(all)
dim(all)

# now impile vertically the 4 measures in a single variable by cut:
# we create a new var called (Measure_type) that defynes the metric
library(reshape2)
metrics <- melt(all, id.vars = "cut", 
                variable.name = "Measure_type",
                value.name = "Measure")
head(metrics)
dim(metrics)
# plot to a better diagnostic choice of suitable threshold##########
ggplot(metrics, aes(x = cut, y = Measure, color = Measure_type)) + 
  geom_line(size=1) + 
  ylab("") + xlab("Probability Cutoff") +
  theme(legend.position = "top")



#library(ggplot2)
#gathered %>%
#  ggplot(aes(x = threshold, y = y, color = x)) +
#  geom_point() +
#  geom_line() +
#  scale_color_brewer(palette = "Set1") +
#  labs(y = "measures",
#      color = "M: event\nR: nonevent")




#PLOTTA FIGURE MA IN METRICS FA VDERE COLONNE : SOGLIA,METRICA,MISURA 
#COSI POSSIAMO ANDARE A DECIDERE/SCEGLIERE/STABILIRE QUALE SIA SOGLIA 
#MIGLIORE SECONDO UNA METRICA CHE DARA UN VALORE ALTO 
####################################################################

# step 3a: confusion matrix del/dei best model####
roc.nnet
roc.rf
#SOGLIA 


# DO AT HAND THE predicted M....for best model
#0.548,0.320
#0.9

#VOGLIO ALTA SPECIFICITY :CUTOFF(0.75,0.85,0.90)
pred_y=ifelse(test_chgoff$rf>0.75, "c0","c1")#rf
pred_y=ifelse(test_chgoff$rf>0.9, "c0","c1")#rf

pred_y=ifelse(test_chgoff$rf>0.548, "c0","c1")#rf lift 
pred_y=ifelse(test_chgoff$rf>0.320, "c0","c1")#rf lift 

pred_y=ifelse(test_chgoff$gb>0.88, "c0","c1")#gb 
pred_y=ifelse(test_chgoff$gb>0.3116, "c0","c1")#gb lift

0.3116

pred_y=ifelse(test_chgoff$nnet>0.9997477, "c0","c1")#nnet
pred_y=as.factor(pred_y)
head(pred_y)

confusionMatrix(pred_y,test_chgoff$MIS_Status,positive="c0")

# step 3b: studio la soglia......####




# use decision rule for the best model#####
# example 
test_chgoff$pred_y=ifelse(test_chgoff$nnet>0.9997477, "c0","c1")#nnet lift 
test_chgoff$pred_y=ifelse(test_chgoff$rf>0.548, "c0","c1")#rf lift 
test_chgoff$pred_y=ifelse(test_chgoff$rf>0.320, "c0","c1")#rf lift

test_chgoff$pred_y=ifelse(test_chgoff$rf>0.9, "c0","c1")#rf criterio stat
test_chgoff$pred_y=ifelse(test_chgoff$gb>0.88, "c0","c1")#gb criterio stat

# step4 score data with decision rule ##########
#seleziona righe almeno 10 % dt test e poi toglie target vecchia e tutte probabilita quelle numerate(p1,p2,ecc)lascia solo pr  
# newdata=scoredata simulato
#togliere variabili da 14(la 14 e la target ) a 19 e da 21 a 23 e 24(adesso sara in newdata lil target previsto)
#newdata=test_chgoff[c(1:3500),-c(15,16,17,18,19,21,22,23)]#nnet
newdata=test_chgoff[c(1:3500),-c(15,16,17,18,19,20,22,23)]#rf
newdata=test_chgoff[c(1:3500),-c(15,16,17,19,20,21,22,23)]#gb

#devo tenere la target ??per poio fare confusion matrix con probabilita prevista 
#prob=predict(nnetFit_glm,newdata,"prob")#nnet 
prob=predict(rfTune1,newdata,"prob")#rf
prob=predict(gbmFit4,newdata,"prob")#gb

probc0=prob[,1]
#newdata$PRED=ifelse(probc0>0.9,"c0","c1")#nnet
newdata$PRED=ifelse(probc0>0.9,"c0","c1")#rf criterio
newdata$PRED=ifelse(probc0>0.320,"c0","c1")#rf lift


newdata$PRED=ifelse(probc0>0.88,"c0","c1")#gb criterio
newdata$PRED=ifelse(probc0>0.3116,"c0","c1")#gb lift 

#y_Pred_newdata <- predict(rfTune1, newdata, type = "raw")#VIENE

confusionMatrix(as.factor(newdata$MIS_Status),as.factor(newdata$PRED),positive="c0")
confusionMatrix(as.factor(newdata$MIS_Status),as.factor(newdata$PRED),positive="c1")

#confusionMatrix(as.factor(y_Pred_newdata),as.factor(newdata$MIS_Status))



#modello con rf 
newdata$prob = predict(rfTune1, newdata, "prob")#VIENE
head(newdata$prob)#VIENE
probM=newdata$prob[,1]#VIENE
newdata$pred_y=ifelse(probM>0.320, "c0","c1")#VIENE
head(newdata)#VIENE
prop.table(table(newdata$pred_y))#VIENE
y_Pred_newdata <- predict(rfTune1, newdata, type = "raw")#VIENE
confusionMatrix(as.factor(y_Pred_newdata), as.factor(newdata$pred_y), positive="c0")#VIENE


#modello con nnnet
newdata$prob = predict(nnetFit_glm, newdata, "prob")#VIENE
head(newdata$prob)#VIENE
probM=newdata$prob[,1]#VIENE
newdata$pred_y=ifelse(probM>0.9997477, "c0","c1")#VIENE
head(newdata)#VIENE
prop.table(table(newdata$pred_y))#VIENE
y_Pred_newdata <- predict(rfTune1, newdata, type = "raw")#VIENE
confusionMatrix(as.factor(y_Pred_newdata), as.factor(newdata$pred_y), positive="c0")#VIENE

# newdata=scoredata simulato
#newdata=test[c(1:40),-c(61:66)]
#newdata$prob = predict(rfTune, newdata, "prob")
#head(prob)
#probM=prob[,1]
#newdata$pred_y=ifelse(probM>0.548, "M","R")
#head(newdata)


#prob=predict(nnetFit_glm,newdata,"prob")
#CAPIRE SE SI VUOLE ANCHE CONFUSION MATRIX DI SCORE 

#y=ifelse(y=="c0",0,1)
#adult$income=ifelse(adult$incometgt==" >50K","c1","c0")
#pred <- ifelse(test$lasso > 0.2, "c1", "c0")
#table(actual=test$income,pred)
#confusionMatrix(pred, test$income, positive="c1")








