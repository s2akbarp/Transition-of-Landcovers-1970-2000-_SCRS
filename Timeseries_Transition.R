
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
tr_7<- raster::raster("tr_7.tif", values=TRUE)
tr_23<- raster::raster("tr_23.tif", values=TRUE)
tr_30<- raster::raster("tr_30.tif", values=TRUE)
tr_38<- raster::raster("tr_38.tif", values=TRUE)
tr_31<- raster::raster("tr_31.tif", values=TRUE)

#rasterize independent

Var_cost_1970<- raster::raster("Var_cost_1970.tif", values=TRUE)
Var_dis_1970<- raster::raster("Var_dis_1970.tif", values=TRUE)
Var_disB_1970<- raster::raster("Var_disB_1970.tif", values=TRUE)
Var_disF_1970<- raster::raster("Var_disF_1970.tif", values=TRUE)
Var_lst_1970<- raster::raster("Var_lst_1970.tif", values=TRUE)
Var_lstb_1970<- raster::raster("Var_lstb_1970.tif", values=TRUE)
Var_Ratiob_1970<- raster::raster("Var_Ratiob_1970.tif", values=TRUE)
Var_cost_2000=raster::raster("Var_cost_2000.tif", values=TRUE)
Var_dis_2000<- raster::raster("Var_dis_2000.tif", values=TRUE)
Var_disB_2000<- raster::raster("Var_disB_2000.tif", values=TRUE)
Var_disF_2000<- raster::raster("Var_disF_2000.tif", values=TRUE)
Var_lst_2000<- raster::raster("Var_lst_2000.tif", values=TRUE)
Var_lstb_2000<- raster::raster("Var_lstb_2000.tif", values=TRUE)
Var_Ratiob_2000<- raster::raster("Var_Ratiob_2000.tif", values=TRUE)
Var_cost_1977<- raster::raster("Var_cost_1977.tif", values=TRUE)
Var_dis_1977<- raster::raster("Var_dis_1977.tif", values=TRUE)
Var_disB_1977<- raster::raster("Var_disB_1977.tif", values=TRUE)
Var_disF_1977<- raster::raster("Var_disF_1977.tif", values=TRUE)
Var_lst_1977<- raster::raster("Var_lst_1977.tif", values=TRUE)
Var_lstb_1977<- raster::raster("Var_lstb_1977.tif", values=TRUE)
Var_Ratiob_1977<- raster::raster("Var_Ratiob_1977.tif", values=TRUE)

Var_lst_c_8<- raster::raster("Var_lst_c_8.tif", values=TRUE)
Var_lst_c_7<- raster::raster("Var_lst_c_7.tif", values=TRUE)
Var_lst_c_23<- raster::raster("Var_lst_c_23.tif", values=TRUE)
Var_lst_c_30<- raster::raster("Var_lst_c_30.tif", values=TRUE)
Var_lst_c_31<- raster::raster("Var_lst_c_31.tif", values=TRUE)
Var_lst_c_38<- raster::raster("Var_lst_c_38.tif", values=TRUE)




plot(stack1970_1977_pf)



extentbound= extent(590318, 591223,6797968, 6799086)
rasterextent=raster(extentbound, nrow=500, ncol=400)    
Var_Ratiob_1970=resample(Var_Ratiob_1970,rasterextent,method="ngb")
Var_Ratiob_1977=resample(Var_Ratiob_1977,rasterextent,method="ngb")
Var_lst_c_8=raster::resample(Var_lst_c_8,rasterextent,method="ngb")
Var_lst_c_7=raster::resample(Var_lst_c_7,rasterextent,method="ngb")
Var_lst_c_23=raster::resample(Var_lst_c_23,rasterextent,method="ngb")
Var_lst_c_30=raster::resample(Var_lst_c_30,rasterextent,method="ngb")
Var_lst_c_31=raster::resample(Var_lst_c_31,rasterextent,method="ngb")
Var_lst_c_38=raster::resample(Var_lst_c_38,rasterextent,method="ngb")

Var_disF_1970=raster::resample(Var_disF_1970,rasterextent,method="ngb")
Var_disF_1977=raster::resample(Var_disF_1970,rasterextent,method="ngb")
Var_disF_2000=raster::resample(Var_disF_2000,rasterextent,method="ngb")




setValues(rasterextent,1)
#layerize the land cover 
tr_7<- raster::layerize(tr_7)
tr_23<- raster::layerize(tr_23)
tr_30<- raster::layerize(tr_30)
tr_31<- raster::layerize(tr_31)
tr_38<- raster::layerize(tr_38)



#stack
stack1970_1977_pp=raster::stack(tr_7$X2,Var_dis_1970,Var_lst_1970,Var_lst_c_7)
stack1970_1977_pb=raster::stack(tr_7$X3, Var_disB_1970,Var_lst_1970,Var_lst_c_7)
stack1970_1977_pf=raster::stack(tr_7$X5,Var_disF_1970,Var_lst_1970,Var_lst_c_7)
stack1970_1977_bf=raster::stack(tr_7$X4, Var_cost_1970,Var_Ratiob_1970,Var_lstb_1970,Var_lst_c_7)

plot(stack1970_1977_pf)
#1970-2000
stack1970_2000_pp=raster::stack(tr_30$X2,Var_dis_1970,Var_lst_1970,Var_lst_c_30)
stack1970_2000_pb=raster::stack(tr_30$X3, Var_disB_1970,Var_lst_1970,Var_lst_c_30)
stack1970_2000_pf=raster::stack(tr_30$X5,Var_disF_1970,Var_lst_1970,Var_lst_c_30)
stack1970_2000_bf=raster::stack(tr_30$X4, Var_cost_1970,Var_Ratiob_1970,Var_lstb_1970,Var_lst_c_30)
#1970-2008
stack1970_2008_pp=raster::stack(tr_38$X2,Var_dis_1970,Var_lst_1970,Var_lst_c_38)
stack1970_2008_pb=raster::stack(tr_38$X3, Var_disB_1970,Var_lst_1970,Var_lst_c_38)
stack1970_2008_pf=raster::stack(tr_38$X5,Var_disF_1970,Var_lst_1970,Var_lst_c_38)
stack1970_2008_bf=raster::stack(tr_38$X4, Var_cost_1970,Var_Ratiob_1970,Var_lstb_1970,Var_lst_c_38)
#1977-2000
stack1977_2000_pp=raster::stack(tr_23$X2,Var_dis_1977,Var_lst_1977,Var_lst_c_23)
stack1977_2000_pb=raster::stack(tr_23$X3, Var_disB_1977,Var_lst_1977,Var_lst_c_23)
stack1977_2000_pf=raster::stack(tr_23$X5,Var_disF_1977,Var_lst_1977,Var_lst_c_23)
stack1977_2000_bf=raster::stack(tr_23$X4, Var_cost_1977,Var_Ratiob_1977,Var_lstb_1977,Var_lst_c_23)

#1977-2008
stack1977_2008_pp=raster::stack(tr_31$X2,Var_dis_1977,Var_lst_1970,Var_lst_c_31)
stack1977_2008_pb=raster::stack(tr_31$X3, Var_disB_1977,Var_lst_1970,Var_lst_c_31)
stack1977_2008_pf=raster::stack(tr_31$X5,Var_disF_1977,Var_lst_1970,Var_lst_c_31)
stack1977_2008_bf=raster::stack(tr_31$X4, Var_cost_1977,Var_Ratiob_1977,Var_lstb_1970,Var_lst_c_31)


#dataframe

#pp
dataframe1970_1977_pp=as.data.frame(stack1970_1977_pp, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_1977_pp)=c('Class','Ditance','LST','LST_Change')
dataframe1970_2000_pp=as.data.frame(stack1970_2000_pp, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_2000_pp)=c('Class','Ditance','LST','LST_Change')
dataframe1977_2000_pp=as.data.frame(stack1977_2000_pp, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1977_2000_pp)=c('Class','Ditance','LST','LST_Change')
dataframe1970_2008_pp=as.data.frame(stack1970_2008_pp, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_2008_pp)=c('Class','Ditance','LST','LST_Change')
dataframe1977_2008_pp=as.data.frame(stack1977_2008_pp, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1977_2008_pp)=c('Class','Ditance','LST','LST_Change')

dataframe1970_1977_pp=cbind(Time=7,dataframe1970_1977_pp)
dataframe1970_2000_pp=cbind(Time=30,dataframe1970_2000_pp)
dataframe1977_2000_pp=cbind(Time=23,dataframe1977_2000_pp)
dataframe1970_2008_pp=cbind(Time=38,dataframe1970_2008_pp)
dataframe1977_2008_pp=cbind(Time=31,dataframe1977_2008_pp)


dataframe_pp=rbind(dataframe1970_1977_pp,dataframe1970_2000_pp,dataframe1977_2000_pp,dataframe1970_2008_pp,dataframe1977_2008_pp)
dataframe_pp=dataframe_pp[sample(nrow(dataframe_pp)),]
dataframe_pp=na.omit(dataframe_pp)
#pb
dataframe1970_1977_pb=as.data.frame(stack1970_1977_pb, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_1977_pb)=c('Class','Ditance','LST','LST_Change')
dataframe1970_2000_pb=as.data.frame(stack1970_2000_pb, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_2000_pb)=c('Class','Ditance','LST','LST_Change')
dataframe1977_2000_pb=as.data.frame(stack1977_2000_pb, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1977_2000_pb)=c('Class','Ditance','LST','LST_Change')
dataframe1970_2008_pb=as.data.frame(stack1970_2008_pb, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_2008_pb)=c('Class','Ditance','LST','LST_Change')
dataframe1977_2008_pb=as.data.frame(stack1977_2008_pb, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1977_2008_pb)=c('Class','Ditance','LST','LST_Change')


dataframe1970_1977_pb=cbind(Time=7,dataframe1970_1977_pb)
dataframe1970_2000_pb=cbind(Time=30,dataframe1970_2000_pb)
dataframe1977_2000_pb=cbind(Time=23,dataframe1977_2000_pb)
dataframe1970_2008_pb=cbind(Time=38,dataframe1970_2008_pb)
dataframe1977_2008_pb=cbind(Time=31,dataframe1977_2008_pb)



dataframe_pb=rbind(dataframe1970_1977_pb,dataframe1970_2000_pb,dataframe1977_2000_pb,dataframe1970_2008_pb,dataframe1977_2008_pb)
dataframe_pb=dataframe_pb[sample(nrow(dataframe_pb)),]
dataframe_pb=na.omit(dataframe_pb)

#pf
dataframe1970_1977_pf=as.data.frame(stack1970_1977_pf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_1977_pf)=c('Class','Ditance','LST','LST_Change')
dataframe1970_2000_pf=as.data.frame(stack1970_2000_pf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_2000_pf)=c('Class','Ditance','LST','LST_Change')
dataframe1977_2000_pf=as.data.frame(stack1977_2000_pf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1977_2000_pf)=c('Class','Ditance','LST','LST_Change')
dataframe1970_2008_pf=as.data.frame(stack1970_2008_pf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_2008_pf)=c('Class','Ditance','LST','LST_Change')
dataframe1977_2008_pf=as.data.frame(stack1977_2008_pf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1977_2008_pf)=c('Class','Ditance','LST','LST_Change')
dataframe1970_1977_pf=cbind(Time=7,dataframe1970_1977_pf)
dataframe1970_2000_pf=cbind(Time=30,dataframe1970_2000_pf)
dataframe1977_2000_pf=cbind(Time=23,dataframe1977_2000_pf)
dataframe1970_2008_pf=cbind(Time=38,dataframe1970_2008_pf)
dataframe1977_2008_pf=cbind(Time=31,dataframe1977_2008_pf)

dataframe_pf=rbind(dataframe1970_1977_pf,dataframe1970_2000_pf,dataframe1977_2000_pf,dataframe1970_2008_pf,dataframe1977_2008_pf)
dataframe_pf=dataframe_pf[sample(nrow(dataframe_pf)),]
dataframe_pf=na.omit(dataframe_pf)





#bf
dataframe1970_1977_bf=as.data.frame(stack1970_1977_bf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_1977_bf)=c('Class','Ditance','Ratio','LST','LST_Change')
dataframe1970_1977_bf=cbind(Time=7,dataframe1970_1977_bf)
dataframe1970_2000_bf=as.data.frame(stack1970_2000_bf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_2000_bf)=c('Class','Ditance','Ratio','LST','LST_Change')
dataframe1970_2000_bf=cbind(Time=30,dataframe1970_2000_bf)
dataframe1977_2000_bf=as.data.frame(stack1977_2000_bf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1977_2000_bf)=c('Class','Ditance','Ratio','LST','LST_Change')
dataframe1977_2000_bf=cbind(Time=23,dataframe1977_2000_bf)
dataframe1970_2008_bf=as.data.frame(stack1970_2008_bf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1970_2008_bf)=c('Class','Ditance','Ratio','LST','LST_Change')
dataframe1970_2008_bf=cbind(Time=38,dataframe1970_2008_bf)
dataframe1977_2008_bf=as.data.frame(stack1977_2008_bf, row.names=NULL, optional=FALSE, xy=FALSE, na.rm=FALSE, long=FALSE)
names(dataframe1977_2008_bf)=c('Class','Ditance','Ratio','LST','LST_Change')
dataframe1977_2008_bf=cbind(Time=31,dataframe1977_2008_bf)

dataframe_bf=rbind(dataframe1970_1977_bf,dataframe1970_2000_bf,dataframe1977_2000_bf,dataframe1970_2008_bf,dataframe1977_2008_bf)
dataframe_bf=dataframe_bf[sample(nrow(dataframe_bf)),]
dataframe_bf=na.omit(dataframe_bf)



#data partitioning

#pp
dataframe_pp$Class=factor(dataframe_pp$Class)
levels(dataframe_pp$Class) <- c("N", "C")


trainids_pp <- createDataPartition(dataframe_pp$Class,list=FALSE,p=0.7)
trainDat_pp <- dataframe_pp[trainids_pp,]
testDat_pp <- dataframe_pp[-trainids_pp,]


#pb
dataframe_pb$Class=factor(dataframe_pb$Class)
levels(dataframe_pb$Class) <- c("N", "c")

trainids_pb <- createDataPartition(dataframe_pb$Class,list=FALSE,p=0.7)
trainDat_pb <- dataframe_pb[trainids_pb,]
testDat_pb <- dataframe_pb[-trainids_pb,]


#pf
dataframe_pf$Class=factor(dataframe_pf$Class)
levels(dataframe_pf$Class) <- c("N", "c")

trainids_pf <- createDataPartition(dataframe_pf$Class,list=FALSE,p=0.7)
trainDat_pf <- dataframe_pf[trainids_pf,]
testDat_pf <- dataframe_pf[-trainids_pf,]



#bf
dataframe_bf$Class=factor(dataframe_bf$Class)
levels(dataframe_bf$Class) <- c("N", "c")

trainids_bf <- createDataPartition(dataframe_bf$Class,list=FALSE,p=0.7)
trainDat_bf <- dataframe_bf[trainids_bf,]
testDat_bf <- dataframe_bf[-trainids_bf,]





# train models 

#1: Classifier
#2: Tunning
#3:K-fold cross validation


# Cross validation 
control <- trainControl(method="repeatedcv", number=10, repeats=2, search="grid",classProbs = TRUE,allowParallel = TRUE,summaryFunction = twoClassSummary)

#GLM

GLMFit1970_2000_pp <- train(Class~.,trainDat_pp, method='glm', trControl = control)
GLMFit1970_2000_pb <- train(Class~.,trainDat_pb, method='glm',trControl = control)
GLMFit1970_2000_pf <- train(Class~.,trainDat_pf, method='glm', trControl = control)
GLMFit1970_2000_bf<- train(Class~.,trainDat_bf, method='glm',trControl = control)
#Random forest
#tune RF
tunegrid <- expand.grid(.mtry=2)
#pp

memory.limit()
memory.limit(size=56000)
rfFit1970_2000_pp <- train(Class~., data=trainDat_pp, method='rf',tuneGrid=tunegrid,trControl=control)

#pb 
rfFit1970_2000_pb <- train(Class~., data=trainDat_pb, method='rf',tuneGrid=tunegrid,trControl=control)



#pf 

memory.limit()
memory.limit(size=56000)
rfFit1970_2000_pf <- train(Class~., data=trainDat_pf, method='rf',tuneGrid=tunegrid,trControl=control)

#bf 

rfFit1970_2000_bf <- train(Class~., data=trainDat_bf, method='rf',tuneGrid=tunegrid,trControl=control)

#Tunning

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), n.trees =c(200,300,400,500), shrinkage = 0.1, n.minobsinnode = 10)


#Classifiers and tranning models

# XGboost
gbmFit1970_2000_pp <- caret::train(Class~.,trainDat_pp, method = "gbm", trControl = control,tuneGrid = gbmGrid,verbose = FALSE,na.action=na.omit,metric = "ROC")
gbmFit1970_2000_pb <- caret::train(Class~.,trainDat_pb, method = "gbm", trControl = control,tuneGrid = gbmGrid,verbose = FALSE,na.action=na.omit,metric = "ROC")
gbmFit1970_2000_pf <- caret::train(Class~.,trainDat_pf, method = "gbm", trControl = control,tuneGrid = gbmGrid,verbose = FALSE,na.action=na.omit,metric = "ROC")
gbmFit1970_2000_bf <- caret::train(Class~.,trainDat_bf, method = "gbm", trControl = control,tuneGrid = gbmGrid,verbose = FALSE,na.action=na.omit,metric = "ROC")



#var importance in random forest
library(gridExtra)
library(grid)
pf_var_r=plot(varImp(GLMFit1970_2000_pp), main="Variable Importance")
pb_var_r=plot(varImp(rfFit1970_2000_pb), ,main=" (PB)")
bf_var_r=plot(varImp(GLMFit1970_2000_bf),main=" (BF)")
grid.arrange(pf_var_r, pb_var_r,bf_var_r, nrow=3,top=textGrob("Variable Importance for Each Transition"))










#predict

#GLM
pr_GLM_pp <- predict(GLMFit1970_2000_pp, testDat_pp, type = "prob")
pr_GLM_pb <- predict(GLMFit1970_2000_pb, testDat_pb,type = "prob")
pr_GLM_pf <- predict(GLMFit1970_2000_pf, testDat_pf,type = "prob")
pr_GLM_bf <- predict(GLMFit1970_2000_bf, testDat_bf,type = "prob")

#RF
testDat_pp=na.omit(testDat_pp)
pr_RF_pp <- predict(rfFit1970_2000_pp,testDat_pp, type = "prob")
pr_RF_pb <- predict(rfFit1970_2000_pb, testDat_pb,type = "prob")
pr_RF_pf <- predict(rfFit1970_2000_pf, testDat_pf,type = "prob")
pr_RF_bf <- predict(rfFit1970_2000_bf,testDat_bf,type = "prob")


#GBM
pr_GBM_pp<- predict(gbmFit1970_2000_pp, newdata = testDat_pp, type = 'prob')
pr_GBM_pb <- predict(gbmFit1970_2000_pb, newdata = testDat_pb, type = 'prob')
pr_GBM_pf <- predict(gbmFit1970_2000_pf, newdata = testDat_pf, type = 'prob')
pr_GBM_bf <- predict(gbmFit1970_2000_bf, newdata = testDat_bf, type = 'prob')






# compare results train results
resamps_pp <- resamples(list(GLM_PP = GLMFit1970_2000_pp,RF_PP = rfFit1970_2000_pp,GB_PP = gbmFit1970_2000_pp))
resamps_pb <- resamples(list(GLM_PB = GLMFit1970_2000_pb,RF_PB = rfFit1970_2000_pb,GB_PB = gbmFit1970_2000_pp))
resamps_pf <- resamples(list(GLM_PF = GLMFit1970_2000_pf,RF_PF = rfFit1970_2000_pf,GB_PF = gbmFit1970_2000_pp))
resamps_bf <- resamples(list(GLM_BF = GLMFit1970_2000_bf,RF_BF = rfFit1970_2000_bf,GB_BF = gbmFit1970_2000_pp))

train_pp=bwplot(resamps_pp, layout = c(3, 1))
train_pb=bwplot(resamps_pb, layout = c(3, 1))
train_pf=bwplot(resamps_pf, layout = c(3, 1))
train_bf=bwplot(resamps_bf, layout = c(3, 1))
library(gridExtra)
grid.arrange(train_pb,train_pf,train_bf,  nrow = 3, top = "Training Results  for Each Class of Transition")




#test results

#pp

pred_pp_GBM<-ROCR::prediction(predictions=pr_GBM_pp[,1], testDat_pp$Class) 
pred_pp_GLM<-ROCR::prediction(predictions=pr_GLM_pp[,1], testDat_pp$Class) 
testDat_pp=na.omit(testDat_pp)
pred_pp_RF<-ROCR::prediction(predictions=pr_RF_pp[,1],testDat_pp$Class) 

roc0<-performance(pred_pp_GBM, measure="tpr", x.measure="fpr")
roc1<-performance(pred_pp_GLM, measure="tpr", x.measure="fpr")
roc2<-performance(pred_pp_RF, measure="tpr", x.measure="fpr")

plot(roc0, main="ROC curve for Test Data (P-P)", col="red")
plot(roc1,add=TRUE, col="blue")
plot(roc2,add=TRUE, col="green")
legend(0.7, 0.6, legend=c("GBM", "GLM","RF"), col=c("red", "blue","green"), lty=1:2, cex=0.8)


par(mfrow=c(2,2))


#pb

pred_pb_GBM<-ROCR::prediction(predictions=pr_GBM_pb[,1], testDat_pb$Class) 
pred_pb_GLM<-ROCR::prediction(predictions=pr_GLM_pb[,1], testDat_pb$Class) 
testDat_pb=na.omit(testDat_pb)
pred_pb_RF<-ROCR::prediction(predictions=pr_RF_pb[,1],testDat_pb$Class) 

roc0_pb<-performance(pred_pb_GBM, measure="tpr", x.measure="fpr")
roc1_pb<-performance(pred_pb_GLM, measure="tpr", x.measure="fpr")
roc2_pb<-performance(pred_pb_RF, measure="tpr", x.measure="fpr")

plot(roc0_pb, main="ROC curve for Test Data (P-B)", col="red")
plot(roc1_pb,add=TRUE, col="blue")
plot(roc2_pb,add=TRUE, col="green")
legend(0.7, 0.6, legend=c("GBM","GLM","RF"), col=c("red","blue","green"), lty=1:2, cex=0.8)


#pf

pred_pf_GBM<-ROCR::prediction(predictions=pr_GBM_pf[,1], testDat_pf$Class) 
pred_pf_GLM<-ROCR::prediction(predictions=pr_GLM_pf[,1], testDat_pf$Class) 
testDat_pf=na.omit(testDat_pf)
pred_pf_RF<-ROCR::prediction(predictions=pr_RF_pf[,1],testDat_pf$Class) 

roc0_pf<-performance(pred_pf_GBM, measure="tpr", x.measure="fpr")
roc1_pf<-performance(pred_pf_GLM, measure="tpr", x.measure="fpr")
roc2_pf<-performance(pred_pf_RF, measure="tpr", x.measure="fpr")

plot(roc0_pf, main="ROC curve for Test Data (P-F)", col="red")
plot(roc1_pf,add=TRUE, col="blue")
plot(roc2_pf,add=TRUE, col="green")
legend(0.7, 0.6, legend=c("GBM","GLM","RF"), col=c("red","blue","green"), lty=1:2, cex=0.8)


#bf

pred_bf_GBM<-ROCR::prediction(predictions=pr_GBM_bf[,1], testDat_bf$Class) 
pred_bf_GLM<-ROCR::prediction(predictions=pr_GLM_bf[,1], testDat_bf$Class) 
testDat_bf=na.omit(testDat_bf)
pred_bf_RF<-ROCR::prediction(predictions=pr_RF_bf[,1],testDat_bf$Class) 

roc0_bf<-performance(pred_bf_GBM, measure="tpr", x.measure="fpr")
roc1_bf<-performance(pred_bf_GLM, measure="tpr", x.measure="fpr")
roc2_bf<-performance(pred_bf_RF, measure="tpr", x.measure="fpr")

plot(roc0_bf, main="ROC curve for Test Data (B-F)", col="red")
plot(roc1_bf,add=TRUE, col="blue")
plot(roc2_bf,add=TRUE, col="green")
legend(0.7, 0.6, legend=c("GBM","GLM","RF"), col=c("red","blue","green"), lty=1:2, cex=0.8)





# probability predictions on map time series
val=30
Time=setValues(rasterextent,val)


#prediction 1970
stack1970_pp=stack(Var_dis_1970,Var_lst_1970,Time,Var_lst_c_30)
names(stack1970_pp)=c('Ditance','LST','Time','LST_Change')
Var_lstbf_1970=mask(Var_lst_1970,pb_1970_30_rf)
stack1970_pB=stack(Var_disB_1970,Var_lst_1970,Time,Var_lst_c_30)
names(stack1970_pB)=c('Ditance','LST','Time','LST_Change')
stack1970_pF=stack(Var_disF_1970,Var_lst_1970,Time,Var_lst_c_30)
names(stack1970_pF)=c('Ditance','LST','Time','LST_Change')
stack1970_bf=stack(Var_cost_1970,Var_lstb_1970,Var_Ratiob_1970,Time,Var_lst_c_30)
names(stack1970_bf)=c('Ditance','LST','Ratio','Time','LST_Change')


plot(stack1970_pF)


#   PREDICTION 30 years probability (1970-2000)

pp_1970_30_rf<- raster::predict (na.omit(stack1970_pp),rfFit1970_2000_pp, type="prob")
pb_1970_30_rf<- raster::predict (na.omit(stack1970_pB),rfFit1970_2000_pb, type="prob")
pf_1970_30_rf<- raster::predict (na.omit(stack1970_pF),rfFit1970_2000_pf, type="prob")
BF_1970_30_rf<- raster::predict (na.omit(stack1970_bf),rfFit1970_2000_bf, type="prob")


pp_1970_30_gbm<- raster::predict (na.omit(stack1970_pp),gbmFit1970_2000_pp, type="prob")
pB_1970_30_gbm<- raster::predict (na.omit(stack1970_pB),gbmFit1970_2000_pb, type="prob")
pF_1970_30_gbm<- raster::predict (na.omit(stack1970_pF),gbmFit1970_2000_pf, type="prob")
BF_1970_30_gbm<- raster::predict (na.omit(stack1970_bf),gbmFit1970_2000_bf, type="prob")


pp_1970_30_glm<- raster::predict (na.omit(stack1970_pp),GLMFit1970_2000_pp, type="prob")
pB_1970_30_glm<- raster::predict (na.omit(stack1970_pB),GLMFit1970_2000_pb, type="prob")
pF_1970_30_glm<- raster::predict (na.omit(stack1970_pF),GLMFit1970_2000_pf, type="prob")
BF_1970_30_glm<- raster::predict (stack1970_bf,GLMFit1970_2000_bf, type="prob")














predic_30years_1970= stack(pf_1970_30_rf,pb_1970_30_rf,BF_1970_30_rf,pF_1970_30_gbm,pB_1970_30_gbm,BF_1970_30_gbm,pF_1970_30_glm,pB_1970_30_glm,BF_1970_30_glm)
names(predic_30years_1970)=c('RF(PF)','RF(PB)','RF(BF)','GBM(PF)','GBM(PB)','GBM(BF)','GLM(PF)','GLM(PB)','GLM(BF)')


#colour
library(viridis)
library(RColorBrewer)
zeroCol <-"#B3B3B3" # (gray color, same as your figure example)
reds <- rev(brewer.pal('YlOrRd', n = 7))
blues <- brewer.pal('Blues', n = 7)
myTheme <- rasterVis::rasterTheme(region = c(reds, zeroCol, blues))
pal <- wesanderson::wes_palette("Zissou1",20, type = "continuous")

library(gridExtra)
library(grid)

rasterVis::levelplot(1-predic_30years_1970, col.regions=rev(pal), margin = FALSE, colorkey = list(space = "bottom"), scales=list(draw=FALSE ),layout=c(3,3), maxpixels= 2e5)

par(mfrow=c(4,3))
plot(1-predic_30years_1970)



#prediction 1970 classification

pp_1970_30_rf_class<- raster::predict (na.omit(stack1970_pp),rfFit1970_2000_pp)
pb_1970_30_rf_class<- raster::predict (na.omit(stack1970_pB),rfFit1970_2000_pb)
pf_1970_30_rf_class<- raster::predict (na.omit(stack1970_pF),rfFit1970_2000_pf)
BF_1970_30_rf_class<- raster::predict (na.omit(stack1970_bf),rfFit1970_2000_bf)


pp_1970_30_gbm_class<- raster::predict (na.omit(stack1970_pp),gbmFit1970_2000_pp)
pB_1970_30_gbm_class<- raster::predict (na.omit(stack1970_pB),gbmFit1970_2000_pb)
pF_1970_30_gbm_class<- raster::predict (na.omit(stack1970_pF),gbmFit1970_2000_pf)
BF_1970_30_gbm_class<- raster::predict (na.omit(stack1970_bf),gbmFit1970_2000_bf)


pp_1970_30_glm_class<- raster::predict (na.omit(stack1970_pp),GLMFit1970_2000_pp)
pB_1970_30_glm_class<- raster::predict (na.omit(stack1970_pB),GLMFit1970_2000_pb)
pF_1970_30_glm_class<- raster::predict (na.omit(stack1970_pF),GLMFit1970_2000_pf)
BF_1970_30_glm_class<- raster::predict (stack1970_bf,GLMFit1970_2000_bf)


#plot
predic_30years_1970_class= stack(pf_1970_30_rf_class,pb_1970_30_rf_class,BF_1970_30_rf_class,pF_1970_30_gbm_class,pB_1970_30_gbm_class,BF_1970_30_gbm_class,pF_1970_30_glm_class,pB_1970_30_glm_class,BF_1970_30_glm_class)
names(predic_30years_1970_class)=c('RF(PF)','RF(PB)','RF(BF)','GBM(PF)','GBM(PB)','GBM(BF)','GLM(PF)','GLM(PB)','GLM(BF)')

real_30years_1970_class=stack(tr_30$X5,tr_30$X3,tr_30$X4)
names(real_30years_1970_class)=c('Real(PF)','Real(PB)','Real(BF)')


p1=rasterVis::levelplot(predic_30years_1970_class, col.regions=c('white','red'), margin = FALSE, colorkey = list(space = "bottom"), scales=list(draw=FALSE ),layout=c(3,3))
p2=rasterVis::levelplot(real_30years_1970_class, col.regions=c('white','red'), margin = FALSE, colorkey = FALSE, scales=list(draw=FALSE ),layout=c(3,1))
grid.arrange(p1, p2, ncol=2)















#animation time series

# Time list
timelist <- list()
Stacklistpredict <- list()

for(i in 1:30){
  Time=setValues(rasterextent,i)
  name <- paste('Time',i,sep='')
  timelist[[name]] <- Time
  for (j in timelist){ 
    stack1970_pF=stack(Var_disF_1970,Var_lst_1970,Time,Var_lst_c_30)
    names(stack1970_pF)=c('Ditance','LST','Time','LST_Change')
    namestack <- paste('stack1970_pF_',i,sep='')
    Stacklistpredict[[namestack]] <- stack1970_pF}}

listpredict <- list()
library(animation)
ani.options(interval=.5)
saveGIF({
  for (i in 1:length(Stacklistpredict)){
    pf_1970_30_rf<- raster::predict (Stacklistpredict[[i]],gbmFit1970_2000_pf, type="prob")
    l <- rasterVis::levelplot(pF_1970_30_rf, col.regions=rev(pal), margin = FALSE, colorkey = list(space = "bottom"), scales=list(draw=FALSE ))
    plot(l, legend=FALSE, main = paste("Year", i))  }
}) 
