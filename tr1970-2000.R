install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
library(raster)
library(caret)
library(mapview)
library(sf)
library(CAST)
library(e1071)
library(mlbench)


DirectoryofInputs=list.files("C:/Onedrive/OneDrive - University of Waterloo/My Data/phd/data/paper1/landcover_modeling/Phase_1/input/RESAMPLE",pattern = "*.tif$", all.files = FALSE, recursive = TRUE)


#IMPORT Input data for 30 years time step from 1970 to 2000

tr_30<- raster::raster("tr_30.tif", values=TRUE)

#rasterize independent
Var_cost_1970<- raster::raster("Var_cost_1970.tif", values=TRUE)
Var_dis_1970<- raster::raster("Var_dis_1970.tif", values=TRUE)
Var_disB_1970<- raster::raster("Var_disB_1970.tif", values=TRUE)
Var_disF_1970<- raster::raster("Var_disF_1970.tif", values=TRUE)
Var_lst_1970<- raster::raster("Var_lst_1970.tif", values=TRUE)
Var_lstb_1970<- raster::raster("Var_lstb_1970.tif", values=TRUE)
Var_Ratiob_1970<- raster::raster("Var_Ratiob_1970.tif", values=TRUE)

extentbound= extent(590318, 591223,6797968, 6799086)
rasterextent=raster(extentbound, nrow=500, ncol=400)    
Var_Ratiob_1970=resample(Var_Ratiob_1970,rasterextent,method="ngb")
plot(Var_Ratiob_1970)


#layerize the land cover 

tr_30<- raster::layerize(tr_30)


#stack

stack1970_2000_pp=raster::stack(tr_30$X2,Var_dis_1970,Var_lst_1970)
stack1970_2000_pb=raster::stack(tr_30$X3, Var_disB_1970,Var_lst_1970)
stack1970_2000_pf=raster::stack(tr_30$X5,Var_disF_1970,Var_lst_1970)
stack1970_2000_bf=raster::stack(tr_30$X4, Var_cost_1970,Var_lstb_1970,Var_Ratiob_1970)


#dataframe
dataframe1970_2000_pp=as.data.frame(stack1970_2000_pp, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
dataframe1970_2000_pb=as.data.frame(stack1970_2000_pb, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
dataframe1970_2000_pf=as.data.frame(stack1970_2000_pf, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
dataframe1970_2000_bf=as.data.frame(stack1970_2000_bf, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)

#data partitioning

#pp
dataframe1970_2000_pp$X2=factor(dataframe1970_2000_pp$X2)
levels(dataframe1970_2000_pp$X2) <- c("N", "c")


trainids_pp <- createDataPartition(dataframe1970_2000_pp$X2,list=FALSE,p=0.7)
trainDat_pp <- dataframe1970_2000_pp[trainids_pp,]
testDat_pp <- dataframe1970_2000_pp[-trainids_pp,]



#pb
dataframe1970_2000_pb$X3=factor(dataframe1970_2000_pb$X3)
levels(dataframe1970_2000_pb$X3) <- c("N", "c")

trainids_pb <- createDataPartition(dataframe1970_2000_pb$X3,list=FALSE,p=0.7)
trainDat_pb <- dataframe1970_2000_pb[trainids_pb,]
testDat_pb <- dataframe1970_2000_pb[-trainids_pb,]



#pf
dataframe1970_2000_pf$X5=factor(dataframe1970_2000_pf$X5)
levels(dataframe1970_2000_pf$X5) <- c("N", "c")

trainids_pf <- createDataPartition(dataframe1970_2000_pf$X5,list=FALSE,p=0.7)
trainDat_pf <- dataframe1970_2000_pf[trainids_pf,]
testDat_pf <- dataframe1970_2000_pf[-trainids_pf,]



#bf
dataframe1970_2000_bf$X4=factor(dataframe1970_2000_bf$X4)
levels(dataframe1970_2000_bf$X4) <- c("N", "c")

trainids_bf <- createDataPartition(dataframe1970_2000_bf$X4,list=FALSE,p=0.7)
trainDat_bf <- dataframe1970_2000_bf[trainids_bf,]
testDat_bf <- dataframe1970_2000_bf[-trainids_bf,]



# train models 

    #1: Classifier
    #2: Tunning
    #3:K-fold cross validation


# Cross validation 
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid",classProbs = TRUE,allowParallel = TRUE,summaryFunction = twoClassSummary)


                              #GLM

GLMFit1970_2000_pp <- train(X2~.,trainDat_pp, method='glm', trControl = control,na.action=na.omit,metric = "ROC")
GLMFit1970_2000_pb <- train(X3~.,trainDat_pb, method='glm',trControl = control,na.action=na.omit,metric = "ROC")
GLMFit1970_2000_pf <- train(X5~.,trainDat_pf, method='glm', trControl = control,na.action=na.omit,metric = "ROC")
GLMFit1970_2000_bf<- train(X4~.,trainDat_bf, method='glm',trControl = control,na.action=na.omit,metric = "ROC")


                           #Random forest
#tune RF
tunegrid <- expand.grid(.mtry=c(1:15))
modellist <- list()


#pp

for (ntree in c(10,100,500)) {
  rfFit1970_2000_pp <- train(X2~.,trainDat_pp, method="rf", tuneGrid=tunegrid, trControl=control, ntree=ntree,na.action=na.omit,metric = "ROC")
  key <- toString(ntree)
  modellist[[key]] <- rfFit1970_2000_pp
}

#pb 

for (ntree in c(10,100,500)) {
  rfFit1970_2000_pb <- train(X3~.,trainDat_pb, method="rf", tuneGrid=tunegrid, trControl=control, ntree=ntree,na.action=na.omit,metric = "ROC")
  key <- toString(ntree)
  modellist[[key]] <- rfFit1970_2000_pb
}

#pf 

for (ntree in c(10,100,500)) {
  rfFit1970_2000_pf <- train(X5~.,trainDat_pf, method="rf", tuneGrid=tunegrid, trControl=control, ntree=ntree,na.action=na.omit,metric = "ROC")
  key <- toString(ntree)
  modellist[[key]] <- rfFit1970_2000_pf
}

#bf 

for (ntree in c(10,100,500)) {
  rfFit1970_2000_bf <- train(X4~.,trainDat_bf, method="rf", tuneGrid=tunegrid, trControl=control, ntree=ntree,na.action=na.omit,metric = "ROC")
  key <- toString(ntree)
  modellist[[key]] <- rfFit1970_2000_bf
}




           #Tunning

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), n.trees =c(10,100,500), shrinkage = 0.1, n.minobsinnode = 20)


#Classifiers and tranning models

# XGboost
gbmFit1970_2000_pp <- caret::train(X2~.,trainDat_pp, method = "gbm", trControl = control,tuneGrid = gbmGrid,verbose = FALSE,na.action=na.omit,metric = "ROC")
gbmFit1970_2000_pb <- caret::train(X3~.,trainDat_pb, method = "gbm", trControl = control,tuneGrid = gbmGrid,verbose = FALSE,na.action=na.omit,metric = "ROC")
gbmFit1970_2000_pf <- caret::train(X5~.,trainDat_pf, method = "gbm", trControl = control,tuneGrid = gbmGrid,verbose = FALSE,na.action=na.omit,metric = "ROC")
gbmFit1970_2000_bf <- caret::train(X4~.,trainDat_bf, method = "gbm", trControl = control,tuneGrid = gbmGrid,verbose = FALSE,na.action=na.omit,metric = "ROC")



#predict

#GLM
pr_GLM_pp <- predict(GLMFit1970_2000_pp, testDat_pp, type = "prob",na.actio=na.pass)
pr_GLM_pb <- predict(GLMFit1970_2000_pb, testDat_pb,type = "prob",na.actio=na.pass)
pr_GLM_pf <- predict(GLMFit1970_2000_pf, testDat_pf,type = "prob",na.actio=na.pass)
pr_GLM_bf <- predict(GLMFit1970_2000_bf, testDat_bf,type = "prob",na.actio=na.pass)

#RF
testDat_pp=na.omit(testDat_pp)
pr_RF_pp <- predict(rfFit1970_2000_pp,testDat_pp, type = "prob",na.actio=na.omit)
pr_RF_pb <- predict(rfFit1970_2000_pb, testDat_pb,type = "prob",na.actio=na.omit)
pr_RF_pf <- predict(rfFit1970_2000_pf, testDat_pf,type = "prob",na.actio=na.omit)
pr_RF_bf <- predict(rfFit1970_2000_bf,testDat_bf,type = "prob",na.actio=na.omit)


#GBM
pr_GBM_pp<- predict(gbmFit1970_2000_pp, newdata = testDat_pp,na.actio=na.pass, type = 'prob')
pr_GBM_pb <- predict(gbmFit1970_2000_pb, newdata = testDat_pb, na.actio=na.pass, type = 'prob')
pr_GBM_pf <- predict(gbmFit1970_2000_pf, newdata = testDat_pf, na.actio=na.pass, type = 'prob')
pr_GBM_bf <- predict(gbmFit1970_2000_bf, newdata = testDat_bf,na.actio=na.pass, type = 'prob')







# compare results train results
resamps_pp <- resamples(list(GLM = GLMFit1970_2000_pp,RF = rfFit1970_2000_pp,GB = gbmFit1970_2000_pp))
resamps_pb <- resamples(list(GLM = GLMFit1970_2000_pb,RF = rfFit1970_2000_pb,GB = gbmFit1970_2000_pb))
resamps_pf <- resamples(list(GLM = GLMFit1970_2000_pf,RF = rfFit1970_2000_pf,GB = gbmFit1970_2000_pf))
resamps_bf <- resamples(list(GLM = GLMFit1970_2000_bf,RF = rfFit1970_2000_bf,GB = gbmFit1970_2000_bf))
  

bwplot(resamps_pp, layout = c(3, 1),)
bwplot(resamps_pb, layout = c(3, 1))
bwplot(resamps_pf, layout = c(3, 1))
bwplot(resamps_bf, layout = c(3, 1))





#test results

   #pp

pred_pp_GBM<-ROCR::prediction(predictions=pr_GBM_pp[,1], testDat_pp$X2) 
pred_pp_GLM<-ROCR::prediction(predictions=pr_GLM_pp[,1], testDat_pp$X2) 
testDat_pp=na.omit(testDat_pp)
pred_pp_RF<-ROCR::prediction(predictions=pr_RF_pp[,1],testDat_pp$X2) 

roc0<-performance(pred_pp_GBM, measure="tpr", x.measure="fpr")
roc1<-performance(pred_pp_GLM, measure="tpr", x.measure="fpr")
roc2<-performance(pred_pp_RF, measure="tpr", x.measure="fpr")

plot(roc0, main="ROC curve for Test Data_Permafrost Plateaus", col="red")
plot(roc1,add=TRUE, col="blue")
plot(roc2,add=TRUE, col="green")
legend(0.8, 0.6, legend=c("GBM", "GLM","RF"), col=c("red", "blue","green"), lty=1:2, cex=0.8)




#pb

pred_pb_GBM<-ROCR::prediction(predictions=pr_GBM_pb[,1], testDat_pb$X3) 
pred_pb_GLM<-ROCR::prediction(predictions=pr_GLM_pb[,1], testDat_pb$X3) 
testDat_pb=na.omit(testDat_pb)
pred_pb_RF<-ROCR::prediction(predictions=pr_RF_pb[,1],testDat_pb$X3) 

roc0_pb<-performance(pred_pb_GBM, measure="tpr", x.measure="fpr")
roc1_pb<-performance(pred_pb_GLM, measure="tpr", x.measure="fpr")
roc2_pb<-performance(pred_pb_RF, measure="tpr", x.measure="fpr")

plot(roc0_pb, main="ROC curve for Test Data_Permafrost Plateaus to Bog", col="red")
plot(roc1_pb,add=TRUE, col="blue")
plot(roc2_pb,add=TRUE, col="green")
legend(0.8, 0.6, legend=c("GBM", "GLM","RF"), col=c("red", "blue","green"), lty=1:2, cex=0.8)


#pf

pred_pf_GBM<-ROCR::prediction(predictions=pr_GBM_pf[,1], testDat_pf$X5) 
pred_pf_GLM<-ROCR::prediction(predictions=pr_GLM_pf[,1], testDat_pf$X5) 
testDat_pf=na.omit(testDat_pf)
pred_pf_RF<-ROCR::prediction(predictions=pr_RF_pf[,1],testDat_pf$X5) 

roc0_pf<-performance(pred_pf_GBM, measure="tpr", x.measure="fpr")
roc1_pf<-performance(pred_pf_GLM, measure="tpr", x.measure="fpr")
roc2_pf<-performance(pred_pf_RF, measure="tpr", x.measure="fpr")

plot(roc0_pf, main="ROC curve for Test Data_Permafrost Plateaus to Fen", col="red")
plot(roc1_pf,add=TRUE, col="blue")
plot(roc2_pf,add=TRUE, col="green")
legend(0.8, 0.6, legend=c("GBM", "GLM","RF"), col=c("red", "blue","green"), lty=1:2, cex=0.8)


#bf

pred_bf_GBM<-ROCR::prediction(predictions=pr_GBM_bf[,1], testDat_bf$X4) 
pred_bf_GLM<-ROCR::prediction(predictions=pr_GLM_bf[,1], testDat_bf$X4) 
testDat_bf=na.omit(testDat_bf)
pred_bf_RF<-ROCR::prediction(predictions=pr_RF_bf[,1],testDat_bf$X4) 

roc0_bf<-performance(pred_bf_GBM, measure="tpr", x.measure="fpr")
roc1_bf<-performance(pred_bf_GLM, measure="tpr", x.measure="fpr")
roc2_bf<-performance(pred_bf_RF, measure="tpr", x.measure="fpr")

plot(roc0_bf, main="ROC curve for Test Data_Permafrost Plateaus to Fen", col="red")
plot(roc1_bf,add=TRUE, col="blue")
plot(roc2_bf,add=TRUE, col="green")
legend(0.8, 0.6, legend=c("GBM", "GLM","RF"), col=c("red", "blue","green"), lty=1:2, cex=0.8)





# probability predictions on map

raster::predict(stack1970_2000_bf[-1],rfFit1970_2000_bf)

