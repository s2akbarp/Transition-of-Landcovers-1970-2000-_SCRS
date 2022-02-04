install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
library(raster)
library(caret)
library(mapview)
library(sf)
library(CAST)
library(e1071)
library(mlbench)
library(caret)
library(xgboost)


###################################################START TRAINING THE MODEL###########################################################

#I will use three methods for traning the model on the new dataset
########1: GLM model/Nnet
#######2: XGboost
#######3:RF

##################################################################GLM_MODEL###########################################################
#################################TRAINING GLM MODEL, scale lst change by /10000, up sampling

####Prepare the data

install.packages("pacman"); pacman::p_load(rgdal,raster,caret,sp,nnet,randomForest,kernlab,e1071)
install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
library(raster)
library(caret)
library(mapview)
library(sf)
library(CAST)
library(e1071)
library(mlbench)

All_class=read.csv('C:/Users/s2akbarpoursafsari/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/generateddata_step2/rf_95/finaldata_dataframe/allclasses_bfp_newprediction.csv')

###################################################START TRAINING THE MODEL###########################################################

#I will use two methods for traning the model on the new dataset
########1: GLM model
#######2: Nnet Model

##################################################################GLM_MODEL###########################################################
#################################TRAINING GLM MODEL

####Prepare the data
All_class=na.omit(All_class)
All_class$Class=factor(All_class$Class)
levels(All_class$Class) <- c("F", "B","P")


######case1: no change
####Result: accuracy 74 

######case2: scale all
All_class$Time=All_class$Time/max(All_class$Time)
All_class$LST_Change=All_class$LST_Change/max(All_class$LST_Change)
All_class$LST=All_class$LST/max(All_class$LST)
All_class$DitanceF=All_class$DitanceF/max(All_class$DitanceF)
All_class$DitancePP =All_class$DitancePP/max(All_class$DitancePP)
All_class$DitanceB=All_class$DitanceB/max(All_class$DitanceB)
All_class$Cost=All_class$Cost/max(All_class$Cost)
####Result: accuracy 74 

########################case3:  remove disB and DisF and scale lst
All_class$LST_Change=All_class$LST_Change/1000
All_class=All_class[,-2]
All_class=All_class[,-3]

####accuracy 74


table(All_class$LST_Change)

#A: partitioning data
trainids_ALLNEW_up <- createDataPartition(All_class$Class,list=FALSE,p=0.7)
trainDat_ALLNEW_up <- All_class[trainids_ALLNEW_up,]
testDat_ALLNEW_up<- All_class[-trainids_ALLNEW_up,]
table(trainDat_ALLNEW_up$Class)

#B: Up sampling
#Modeling 
#logistic regression, tuning, scale just lst_change, no sampling
trainDat_ALLNEW_up <- upSample(x = trainDat_ALLNEW_up[, -1], y = trainDat_ALLNEW_up$Class)

trainDat_ALLNEW_up <-trainDat_ALLNEW_up[order(trainDat_ALLNEW_up$Time),]
counts=table(trainDat_ALLNEW_up$Class,trainDat_ALLNEW_up$Time)
barplot(counts,col=c("red", "blue","green"),legend=row.names(counts))
#Train
fit.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
set.seed(123)  
GLMFit_allclasses_newdata <- train(Class~., data=trainDat_ALLNEW_up, method = "multinom", trControl = fit.control, trace = FALSE)


saveRDS(GLMFit_allclasses_newdata, file = "C:/Users/s2akbarpoursafsari//OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_glm_rep_generateddata.rds")
GLMFit_allclasses_generateddata=readRDS( "C:/Users/s2akbarpoursafsari/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_glm_rep_generateddata.rds")



##########best model
#glm lst divided by 10000 accuracy 81, the best predictions are the time when lst change is divided by 500
saveRDS(GLMFit_allclasses_newdata, file = "C:/Users/s2akbarpoursafsari//OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_glm_rep_generateddata.rds")
GLMFit_allclasses_generateddata=readRDS( "C:/Users/s2akbarp/OneDrive - University of Waterloo/My Data/phd/data/paper1/landcover_modeling/Phase_1/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_glm_rep_generateddata.rds")


#glm lst divided by 10000 accuracy 81
saveRDS(GLMFit_allclasses_newdata, file = "C:/Users/s2akbarpoursafsari//OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_glm_rep_generateddata.rds")
GLMFit_allclasses_generateddata=readRDS( "C:/Users/s2akbarp/OneDrive - University of Waterloo/My Data/phd/data/paper1/landcover_modeling/Phase_1/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_glm_rep_generateddata.rds")




# Predicting the values for train dataset
trainDat_ALLNEW_up$ClassPredicted <- predict(nnetFit, newdata = trainDat_ALLNEW_up, "class")
# Building classification table
tab <- table(trainDat_ALLNEW_up$Class, trainDat_ALLNEW_up$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)


# Predicting the class for test dataset
testDat_ALLNEW_up$ClassPredicted <- predict(multinom_model, newdata = testDat_ALLNEW_up, "class")
# Building classification table
tab <- table(testDat_ALLNEW_up$Class, testDat_ALLNEW_up$ClassPredicted)
tab



#reports glm: 1: no sclae (74%) 2: devide lst change by 10000 (81) 3:scale all (74%), 4: case three 74%

##################################################################Nnet_MODEL###########################################################
#################################TRAINING Nnet MODEL

####Prepare the data
##############scaling,  sampling
All_class=na.omit(All_class)
All_class$Class=factor(All_class$Class)
levels(All_class$Class) <- c("F", "B","P")
table(All_class$Class)

#A: Scale

All_class$Time=All_class$Time/10
All_class$LST_Change=All_class$LST_Change/10
All_class$LST=All_class$LST/max(All_class$LST)
All_class$DitanceF=All_class$DitanceF/max(All_class$DitanceF)
All_class$DitancePP =All_class$DitancePP/max(All_class$DitancePP)
All_class$DitanceB=All_class$DitanceB/max(All_class$DitanceB)
All_class$Cost=All_class$Cost/max(All_class$Cost)

#B: partitioning data
trainids_ALLNEW_up <- createDataPartition(All_class$Class,list=FALSE,p=0.7)
trainDat_ALLNEW_up <- All_class[trainids_ALLNEW_up,]
testDat_ALLNEW_up<- All_class[-trainids_ALLNEW_up,]
table(trainDat_ALLNEW_up$Class)

#C: Up sampling
#Modeling 

trainDat_ALLNEW_up <- upSample(x = trainDat_ALLNEW_up[, -1], y = trainDat_ALLNEW_up$Class)

trainDat_ALLNEW_up <-trainDat_ALLNEW_up[order(trainDat_ALLNEW_up$Time),]
counts=table(trainDat_ALLNEW_up$Class,trainDat_ALLNEW_up$Time)
barplot(counts,col=c("red", "blue","green"),legend=row.names(counts))


###########1: Train All the model by using Nnet, upsampling,scaling, no cross validation
fitControl_nocross <- trainControl(method="none")
nnetGrid<- expand.grid(decay = 1e-2,size =c(10, 25, 40))
nnet_model_all_up<- caret::train(Class~.,trainDat_ALLNEW_up,method = "nnet", metric="Accuracy", trainControl = fitControl_nocross, tuneGrid = nnetGrid)


saveRDS(nnet_model_all_up, file = "C:/Users/s2akbarpoursafsari/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_nnet_nocross_generateddata.rds")
NnetFit_allclasses_generateddata_nocross=readRDS( "C:/Users/s2akbarpoursafsari/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_nnet_nocross_generateddata.rds")



###########2: Train All the model by using Nnet, upsampling,scaling,  cross validation

nnetGrid<- expand.grid(decay = 1e-2,size =seq.int(from=10, to=100, by=10))
fitControl_cross <-  trainControl(method = "cv", number = 3)
nnet_model_all_up_cross<- caret::train(Class~.,trainDat_ALLNEW_up,method = "nnet", metric="Accuracy", trainControl = fitControl_cross, tuneGrid = nnetGrid)


saveRDS(nnet_model_all_up_cross, file = "C:/Users/s2akbarpoursafsari/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_nnet_cross_generateddata.rds")
NnetFit_allclasses_generateddata_cross=readRDS( "C:/Users/s2akbarpoursafsari/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_nnet_cross_generateddata.rds")









##################################################################XGBoost_MODEL###########################################################
########### Train All the model by using XGboost, upsampling,noscaling just LST change,  cross validation
All_class=read.csv("C:/Users/s2akbarp/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/generateddata_step2/rf_95/finaldata_dataframe/Allclasses_datagenerated_rfmodel_laststep.txt")
All_class$LST_Change=All_class$LST_Change/10000

####Prepare the data
##############scaling,  sampling
All_class=na.omit(All_class)
All_class$Class=factor(All_class$Class)
levels(All_class$Class) <- c("F", "B","P")
table(All_class$Class)


#A: partitioning data
trainids_ALLNEW_up <- createDataPartition(All_class$Class,list=FALSE,p=0.2)
trainDat_ALLNEW_up <- All_class[trainids_ALLNEW_up,]
testDat_ALLNEW_up<- All_class[-trainids_ALLNEW_up,]
table(trainDat_ALLNEW_up$Class)

#B: Up sampling
#Modeling 

trainDat_ALLNEW_up <- upSample(x = trainDat_ALLNEW_up[, -3], y = trainDat_ALLNEW_up$Class)

trainDat_ALLNEW_up <-trainDat_ALLNEW_up[order(trainDat_ALLNEW_up$Time),]
counts=table(trainDat_ALLNEW_up$Class,trainDat_ALLNEW_up$Time)
barplot(counts,col=c("red", "blue","green"),legend=row.names(counts))
table(trainDat_ALLNEW_up$LST_Change)



################################################################## method = "xgbTree" no tune
xgbgrid <- expand.grid(nrounds = 500, max_depth = 10,eta = 0.1,gamma=0,colsample_bytree = 0.9,min_child_weight = 1,subsample = 1)

fitControl_cross <- trainControl(method = "cv", number = 3)
gc()
memory.limit()
memory.limit(size=960000000000000000)
xgb_tune = train(Class~.,trainDat_ALLNEW_up,trControl = fitControl_cross,tuneGrid = xgbgrid, method = "xgbTree", metric="Accuracy")


saveRDS(xgb_tune, file = 'C:/Users/s2akbarp/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_xgbtune_cross_generateddata.rds')
xgbFit_allclasses_generateddata_cross_lesstune=readRDS('C:/Users/s2akbarp/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_xgbtune_cross_generateddata.rds')

#####94 percent accuracy nround=500 by increasing this variable the accuracy will increase 

####################tune the model

xgbgrid <- expand.grid(nrounds = c(2500,3000,3500), max_depth =  c(25,30),eta = 0.2,gamma=0,colsample_bytree = 0.9,min_child_weight = 1,subsample = 1)
xgb_tune_all = train(Class~.,trainDat_ALLNEW_up,trControl = fitControl_cross,tuneGrid = xgbgrid, method = "xgbTree", metric="Accuracy")


saveRDS(xgb_tune_all, file = 'C:/Users/s2akbarp/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_xgbtunemore_cross_generateddata.rds')
xgbFit_allclasses_generateddata_cross_tune=readRDS( 'C:/Users/s2akbarp/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_xgbtunemore_cross_generateddata.rds')





##################################################################rf_MODEL###########################################################
########### Train All the model by using rf, upsampling,noscaling just LST change,  cross validation
All_class=read.csv("C:/Users/s2akbarp/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/generateddata_step2/rf_95/finaldata_dataframe/Allclasses_datagenerated_rfmodel_laststep.txt")
All_class$LST_Change=All_class$LST_Change/10000

####Prepare the data
##############scaling,  sampling
All_class=na.omit(All_class)
All_class$Class=factor(All_class$Class)
levels(All_class$Class) <- c("F", "B","P")
table(All_class$Time)


#A: partitioning data
trainids_ALLNEW_up <- createDataPartition(All_class$Class,list=FALSE,p=0.2)
trainDat_ALLNEW_up <- All_class[trainids_ALLNEW_up,]
testDat_ALLNEW_up<- All_class[-trainids_ALLNEW_up,]
table(trainDat_ALLNEW_up$Class)

#B: Up sampling
#Modeling 

trainDat_ALLNEW_up <- upSample(x = trainDat_ALLNEW_up[, -3], y = trainDat_ALLNEW_up$Class)

trainDat_ALLNEW_up <-trainDat_ALLNEW_up[order(trainDat_ALLNEW_up$Time),]
counts=table(trainDat_ALLNEW_up$Class,trainDat_ALLNEW_up$Time)
barplot(counts,col=c("red", "blue","green"),legend=row.names(counts))
table(trainDat_ALLNEW_up$LST_Change)



################################################################## method = "rf"  tune
tunegrid <- expand.grid(.mtry=c(10,15,25,35))

fitControl_cross <- trainControl(method = "cv", number = 3, verboseIter = T, classProbs = T, allowParallel = T) 
set.seed(12345) 
gc()
memory.size() ### Checking your memory size
memory.limit(560000000) ## Checking the set limit

install.packages("devtools", dependencies = TRUE)
devtools::install_github("krlmlr/ulimit")
ulimit::memory_limit(4000)



rf_tune = train(Class~.,trainDat_ALLNEW_up,trControl = fitControl_cross,tuneGrid = tunegrid, method = "rf", metric="Accuracy")


saveRDS(rf_tune, file = 'C:/Users/s2akbarp/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_rfbtune_cross_generateddata.rds')
rfFit_allclasses_generateddata_cross_lesstune=readRDS('C:/Users/s2akbarp/OneDrive - University of Waterloo/Desktop/newdataframe/Try3_NoTransitionclass_BFPclassification/models/step2Models_aftergeneratingData/allclasses_rfbtune_cross_generateddata.rds')



