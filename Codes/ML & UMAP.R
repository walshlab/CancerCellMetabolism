library(ggplot2)
library(extrafont)
library(umap)
# All metabolic groups
plotData <- rbind(controlData,tenM2DGData,twentyM2DGData,fiftyM2DGData,noGlucoseData,cyanideData)
plotData$label <- as.factor(plotData$label)
plotData$label <- factor(plotData$label , levels=c("Control", "10 mM 2-DG", "20 mM 2-DG", "50 mM 2-DG","No glucose","Cyanide"))
setwd("C:/Linghao Hu/Project/Metabolic_Study/Sketch/Figures/Glycolysis_OXPHOS")

plotDataUmap <- plotData[c(1:12)]
plotData.labels <- factor(plotData$label , levels=c("Control", "10 mM 2-DG", "20 mM 2-DG", "50 mM 2-DG","No glucose","Cyanide"))
plotData.umap = umap(plotDataUmap,n_neighbors = 20, min_dist = 0.1, n_epochs = 200, n_trees = 10, pca = 3)
head(plotData.umap$layout, 3)

df<- data.frame(x = plotData.umap$layout[,1],
                y = plotData.umap$layout[,2],
                label = plotData.labels)

ggplot(df, aes(x, y, group = label)) +
  geom_point(aes(color = label,shape = label), size = 1.2, alpha = 0.8)+
  xlab("UMAP Dimension 1") +
  ylab("UMAP Dimension 2") +
  scale_colour_manual(name="label", values = c("red","grey50","grey50","grey50","grey50","blue"))+
  scale_shape_manual(values=c(16, 16, 15, 17, 18 ,16)) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14),
        axis.title=element_text(size=18,face="bold"),
        legend.position= 'bottom',
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.text=element_text(size=14,face="bold"))+
  guides(color = guide_legend(override.aes = list(size = 5)))


# Glycolysis and OXPHOS inhibition groups
plotData <- rbind(fiftyM2DGData,noGlucoseData,cyanideData)
plotData$label <- as.factor(plotData$label)
plotData$label <- factor(plotData$label , levels=c("50 mM 2-DG","No glucose","Cyanide"))
setwd("C:/Linghao Hu/Project/Metabolic_Study/Sketch/Figures/Glycolysis_OXPHOS")

plotDataUmap <- plotData[c(1:12)]
plotData.labels <- factor(plotData$label , levels=c("50 mM 2-DG","No glucose","Cyanide"))
plotData.umap = umap(plotDataUmap,n_neighbors = 10, min_dist = 0.1, n_epochs = 200, n_trees = 10, pca = 5)
head(plotData.umap$layout, 3)

df<- data.frame(x = plotData.umap$layout[,1],
                y = plotData.umap$layout[,2],
                label = plotData.labels)

ggplot(df, aes(x, y, group = label)) +
  geom_point(aes(color = label,shape = label), size = 1.5, alpha = 0.8)+
  xlab("UMAP Dimension 1") +
  ylab("UMAP Dimension 2") +
  scale_colour_manual(name="label", values = c("grey50","grey50","blue"))+
  scale_shape_manual(values=c(16, 15, 16)) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14),
        axis.title=element_text(size=18,face="bold"),
        legend.position= 'bottom',
        legend.title = element_blank(),
        legend.text=element_text(size=16,face="bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = NA, fill = NA))+
  guides(color = guide_legend(override.aes = list(size = 5)))

# Classification based on machine learning methods--------

controlData$Metabolic <- "Control"
tenM2DGData$Metabolic <- "Inhibit Glycolysis"
twentyM2DGData$Metabolic <- "Inhibit Glycolysis"
fiftyM2DGData$Metabolic <- "Inhibit Glycolysis"
noGlucoseData$Metabolic <- "Inhibit Glycolysis"
cyanideData$Metabolic <- "Inhibit OXPHOS"

# Random forest tree
# 2 classes
library(caTools)
library(randomForest)
mlData <-rbind(cyanideData,fiftyM2DGData,noGlucoseData)
mlData <- mlData[,c(1:12,14)]
mlData <- as.data.frame(mlData)
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Inhibit Glycolysis","Inhibit OXPHOS"))
sample = sample.split(mlData$Metabolic, SplitRatio  = 3/4, group = NULL)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)

# 3 classes
# Random forest tree
library(caTools)
library(randomForest)
mlData <-rbind(cyanideData,fiftyM2DGData,noGlucoseData,controlData)
mlData <- mlData[,c(1:12,14)]
mlData <- as.data.frame(mlData)
mlData$Metabolic <- factor(mlData$Metabolic, levels = c("Control","Inhibit Glycolysis","Inhibit OXPHOS"))
sample = sample.split(mlData$Metabolic, SplitRatio  = 3/4, group = NULL)
train = subset(mlData, sample == TRUE)
test = subset(mlData, sample == FALSE)

# classification using random forest tree------
set.seed(100)
model <- randomForest(Metabolic~., data=train, ntree = 2500, mtry = 5)
pred.rft = predict(model, newdata=test[-13])
predPro.rft = predict(model, newdata=test[-13],type="prob")
confusionMatrix = table(test[,13], pred.rft)
getTree(model, k=1, labelVar=TRUE)
importanceList = importance(model)

### Repeat five times(cross-validation) to get the average performance 
library(modelr)
k = 5

for (x in 1:k){    
  cvData  <- crossv_kfold(mlData, k = k)
  model  <- randomForest(Metabolic~., data = cvData$train[[x]], ntree = 2500, mtry = 5)
  importanceList = importance(model)
  if (x == 1) {importanceLists <- importanceList }
  else{importanceLists <- cbind(importanceLists,importanceList)}
}


## Barplot with mean
colnames(importanceLists) <- c('T1','T2','T3','T4','T5')
featureSD <- apply(importanceLists, 1, sd) 
featureMean <- apply(importanceLists, 1, mean) 
data <- data.frame(
  name = letters[1:12],
  value = featureMean,
  sd = featureSD
)
ggplot(data) +
  geom_bar( aes(x=name, y=value,fill=name), stat="identity", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="red", alpha=0.9, size=1.3)+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(size = 12,angle = 45,hjust = 1,color = "black"),
        axis.text.y = element_text(size = 12, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold",size = 20),
        legend.position = 'none')

# ROC plot
energy = as.numeric(test$Metabolic)
rft.roc <- get.tpr.fpr(predPro.rft,energy)
features <- rep('RFT', times=length(rft.roc[[1]]))
df <- data.frame(tpr=rft.roc[[1]],fpr=rft.roc[[2]],
                 label = features ,stringsAsFactors = FALSE)
a <- ggplot() + geom_line(aes(x=fpr, y=tpr,color = label),data = df,size =3)+
  xlab("1 - Specificity") +
  ylab("Sensitivity") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14),
        axis.title=element_text(size=18,face="bold"),
        legend.position= "None",
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.key=element_blank(),
        legend.background = element_rect(fill = "transparent"))
a


# Valide with Other 2-DG data
valid <-  twentyM2DGData;
valid <- valid[,c(1:12,14)]

pred.rft = predict(model, newdata=valid)
predPro.rft = predict(model, newdata=valid,type="prob")
predictResult <- summary(pred.rft)
glycolysisInhRatio<- predictResult[1]/nrow(valid)
oxphoxInhRatio <- predictResult[2]/nrow(valid)

# Save model
saveRDS(model, "./RFT.Model.rds")


# classification using SVM------
library(e1071)
model <- svm(Metabolic ~ ., data=train, type = 'C-classification',kernel = 'linear', probability = TRUE)
pred.svm = predict(model, newdata=test[-13])

predPro.svm = predict(model, newdata=test[-13],probability = TRUE)
predPro.svm = attr(predPro.svm, "probabilities")

confusionMatrix = table(test[,13], pred.svm)
energy = as.numeric(test$Metabolic)
svm.roc <- get.tpr.fpr(predPro.svm,energy)
print(model)

# classification using qda------
library(MASS)
model <- qda(Metabolic ~ ., data=train)
pred.qda = predict(model, newdata=test[-13])
predPro.qda = predict(model, newdata=test[-13],probability = TRUE)
confusionMatrix = table(test[,13], predPro.qda$class)


energy = as.numeric(test$Metabolic)
qda.roc <- get.tpr.fpr(pred.qda$posterior[,1],energy)

# Calculate ROC-------
# Draw ROC curve
library(ROCR)
library(plotROC)
library(pROC)

get.tpr.fpr <- function(featuresPro, readVallue ){
  if (is.null(dim(featuresPro)))
  {
    pred <- prediction(featuresPro, readVallue)
    perf <- performance(pred, "fpr", "tpr")
    tpr <- perf@x.values[[1]]
    fpr <- perf@y.values[[1]]
  }
  else
  {
    pred <- prediction(featuresPro[,2], readVallue)
    perf <- performance(pred, "fpr", "tpr")
    tpr <- perf@x.values[[1]]
    fpr <- perf@y.values[[1]]
  }
  auc = performance(pred, "auc")@y.values[[1]]
  result <- list(tpr,fpr,auc)
  return(result)
}

# Draw Roc fo all
energy = as.numeric(test$Metabolic)
rft.roc <- get.tpr.fpr(predPro.rft,energy)

nadht1.roc <- get.tpr.fpr(test$NADH.t1,energy)
nadht2.roc <- get.tpr.fpr(test$NADH.t2,energy)
nadha1.roc <- get.tpr.fpr(test$NADH.a1,energy)
nadhFLIM.roc <- get.tpr.fpr(test$NADH.FLIM,energy)
nadhInt.roc <- get.tpr.fpr(test$NADH.Intensity,energy)

fadt1.roc <- get.tpr.fpr(test$FAD.t1,energy)
fadt2.roc <- get.tpr.fpr(test$FAD.t2,energy)
fada1.roc <- get.tpr.fpr(test$FAD.a1,energy)
fadFLIM.roc <- get.tpr.fpr(test$FAD.FLIM,energy)
fadInt.roc <- get.tpr.fpr(test$FAD.Intensity,energy)

RR.roc <- get.tpr.fpr(test$Redox.Ratio,energy)
FLIRR.roc <- get.tpr.fpr(test$FLIRR,energy)
