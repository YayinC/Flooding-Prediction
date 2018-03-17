setwd("D:/Study/2018 Spring/Land Use and Modeling Environment/midTermProject_Data")

library(tidyverse)
library(sf)
library(caret)
library(stats)
library(pscl)
library(plotROC)
library(pROC)
library(car)
library(ggmap)
library(knitr)
library(corrplot)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(ROCR)
library(MKmisc)
library(Boruta)
library(viridis)

####Data Wrangling####
Indianapolis<-st_read("Indianapolis_data.shp") %>% st_sf()
Indianapolis_predictors<-Indianapolis
st_geometry(Indianapolis_predictors)<-NULL

Indianapolis_predictors<-Indianapolis_predictors %>% 
  mutate(st_elev=(elev-min(Indianapolis_predictors$elev))/(max(Indianapolis_predictors$elev)-min(Indianapolis_predictors$elev)))

calgary<-st_read("Calgary/fishnet.shp")

calgary<-calgary %>% mutate(forest=ifelse(l_cover==1,1,0),
                            barren=ifelse(l_cover==5,1,0),
                            grass=ifelse(l_cover==2,1,0),
                            develop=ifelse(l_cover==4,1,0),
                            agri=ifelse(l_cover==3,1,0),
                            flood=ifelse(inun==2|inun==1,1,0))
calgary<-calgary %>% dplyr::rename(dis_dev=dis_develo)
calgary<-calgary %>% mutate(st_elev=(elev-min(calgary$elev))/(max(calgary$elev)-min(calgary$elev)))

caldata<-calgary
caldata<-caldata %>% filter(inun!=3)

st_geometry(caldata)<-NULL
regdata<-caldata %>% dplyr::select(cellID,flood,elev,st_elev,slope,dis_river,dis_steep,dis_agri,
                                   dis_barren,dis_dev,dis_forest,dis_grass,dis_str,
                                   forest,barren,grass,develop,agri,river)

####Comparison across results####
variables<-regdata %>% dplyr::select(flood,st_elev,slope,dis_forest,dis_dev,dis_agri,dis_barren,dis_grass,dis_river,
                                     dis_str) %>%
  gather(key, value,st_elev,slope,dis_forest,dis_dev,dis_agri,dis_barren,dis_grass,dis_river,
         dis_str) %>%
  mutate(value = ifelse(key == "slope"|key == "st_elev", value*100, value))

ggplot(variables, aes(flood,value,fill=as.factor(flood))) + 
  geom_bar(stat="identity",alpha=0.7) + 
  facet_wrap(~key,ncol=3) +
  scale_fill_manual(values = c("#7bccc4","#0868ac"),
                    labels = c("Not Flooded","Flooded"),
                    name = "") +
  labs(title="Comparison between Flooded and Not Flooded",x="Flooded", y="Value")+
  theme(text = element_text(size = 12,face = "italic"),
        plot.title = element_text(size = 17,face = "bold",colour = "black", hjust = 0),
        plot.subtitle = element_text(size = 12, face = "italic", colour = "dark grey", hjust = 0),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
        legend.text = element_text(size = 10),
        legend.position="left",
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0,0,0,0), "lines"))


ggplot(variables, aes(flood,value,fill=as.factor(flood))) +
  geom_boxplot(aes(fill=as.factor(flood)),width=20,alpha=0.6,outlier.color=NA,color="grey",size=0.2) +  
  facet_wrap(~key,scales="free",ncol=3) +
  scale_fill_manual(values = c("#7bccc4","#0868ac"),
                    labels = c("Not Flooded","Flooded")) +
  labs(title="Variable Distribution across Results",
       x="Results",
       y="Value") +
  theme(text = element_text(size = 12,face = "italic"),
        plot.title = element_text(size = 17,face = "bold",colour = "black", hjust = 0),
        plot.subtitle = element_text(size = 12, face = "italic", colour = "dark grey", hjust = 0),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
        legend.text = element_text(size = 10),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0,0,0,0), "lines"))

####Correlation####
floodcolor<-colorRampPalette(c("#b30000","#e34a33","#fc8d59","#fdcc8a","#fef0d9","#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac"))

predictors<-regdata %>% dplyr::select(-flood,-cellID,-elev,-city,-river)
cor<-cor(predictors)
corrplot(cor, method="square",tl.col="black",col=floodcolor(200),tl.srt=45)

####Feature Selection####
#use boruta for feature selection
boruta_Flooding<- Boruta(flood~st_elev+slope+dis_river+dis_steep+dis_agri+
                           dis_barren+dis_dev+dis_forest+dis_grass+dis_str+
                           forest+barren+grass+develop+agri,data = regdata,doTrace = 2)

var_flooding <- getSelectedAttributes(boruta_Flooding)
flooding_formula <- formula(paste("flood ~", paste(var_flooding, collapse = " + ")))

####Define the function to calculate best cutoff value####
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x-0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

####Out-sample prediction####
#create randomly selected training and test set
inTrain <- createDataPartition(
  y = regdata$flood, 
  p = .75, list = FALSE)

training_fld<- regdata[inTrain,]
test_fld <- regdata[-inTrain,]

mylogit <- glm(flooding_formula,data = training_fld,family = "binomial")
summary(mylogit)

fit_test<-predict(mylogit,test_fld,type="response") %>% as.data.frame()

OS_TestProbs <- cbind(test_fld$flood, fit_test)

colnames(OS_TestProbs) <- c("Class", "Probs")
head(OS_TestProbs)
roc_test <- as.data.frame(OS_TestProbs)
head(roc_test)

pred_test<- prediction(roc_test$Probs, roc_test$Class)
roc.perf_test = performance(pred_test, measure = "tpr", x.measure="fpr")
cutoff<-opt.cut(roc.perf_test,pred_test)

#Results
logitoutput <- summary(mylogit)
logitoutput
logitcoeffs <- logitoutput$coefficients
logitcoeffs
or_ci <- exp(cbind(OR=coef(mylogit), confint(mylogit)))
finallogitoutput <- cbind(logitcoeffs, or_ci) %>% as.data.frame()
finallogitoutput$Variable<-rownames(finallogitoutput)

predictors<-finallogitoutput %>% filter(Variable!="(Intercept)")

ggplot(predictors, aes(x=reorder(Variable,OR),y=OR), fill=Variable) + 
  geom_bar(stat="identity",fill="#43a2ca")+  
  geom_hline(yintercept=1, colour = "#7bccc4", linetype= "longdash", size=1)+
  theme(
    text = element_text(size = 12,face = "italic"),
    plot.title = element_text(size = 20,face = "bold",colour = "black", hjust = 0),
    plot.subtitle = element_text(size = 15, face = "italic", colour = "dark grey", hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8, face = "italic", colour = "grey"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  labs(title="Odds Ratio of Predictors",
       subtitle="Flooding Prediction for Calgary",
       x="Variable",
       y="Odds Ratio")

#prediction accuracy
OS_TestProbs<-OS_TestProbs %>% mutate(predClass=ifelse(Probs >=cutoff,1,0))

ggplot(OS_TestProbs, aes(d = Class, m = Probs)) + 
  geom_roc(n.cuts = 50, labels = FALSE,color="#43a2ca") + 
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC",
       subtitle = "Out-sample Prediction") + 
  theme(
    text = element_text(size = 12,face = "italic"),
    plot.title = element_text(size = 20,face = "bold",colour = "black", hjust = 0),
    plot.subtitle = element_text(size = 15, face = "italic", colour = "dark grey", hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8, face = "italic", colour = "grey")
  )

ggplot(data=OS_TestProbs,aes(x=Probs)) + 
  geom_histogram(fill = "#0868ac", alpha = 0.7,binwidth=0.01)+
  labs(title = "Probability Distribution",
       subtitle = "Out-sample Prediction") + 
  theme(
    text = element_text(size = 12,face = "italic"),
    plot.title = element_text(size = 20,face = "bold",colour = "black", hjust = 0),
    plot.subtitle = element_text(size = 15, face = "italic", colour = "dark grey", hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
    legend.position="right",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8, face = "italic", colour = "grey")
  )

ggplot(OS_TestProbs, aes(x = Probs, fill=as.factor(Class))) + 
  geom_density(alpha=0.7,size=0.1,color=NA) +
  facet_grid(Class ~ .) + xlab("Probability") +
  scale_fill_manual(values = c("#7bccc4", "#0868ac"),
                    labels = c("Not Flooded", "Flooded"),
                    name="") +
  labs(title = "Density Plot of Flood Inundataion",
       subtitle = "Out-sample Prediction") + 
  theme(
    text = element_text(size = 12,face = "italic"),
    plot.title = element_text(size = 20,face = "bold",colour = "black", hjust = 0),
    plot.subtitle = element_text(size = 15, face = "italic", colour = "dark grey", hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
    legend.position="right",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8, face = "italic", colour = "grey")
  )

AUC<-auc(OS_TestProbs$Class,OS_TestProbs$Probs)
AUC

#McFadden
pR2(mylogit)

#Confusion Matrix
confusion<-OS_TestProbs %>% group_by(Class,predClass) %>% dplyr::summarise(count=n())

confusionMatrix(OS_TestProbs$predClass ,OS_TestProbs$Class)

ggplot(confusion, aes(as.factor(Class),as.factor(predClass))) +
  geom_point(aes(size = count), colour = "#43a2ca",alpha=0.7,show.legend = FALSE)+ 
  geom_text(aes(label = count))+
  scale_size_continuous(range=c(10,30))+
  labs(title = "Confusion Matrix", 
       subtitle="Flooding Prediction for Calgary",
       x="Actual Results",
       y="Predicted Results")+
  theme(
    text = element_text(size = 12,face = "italic"),
    plot.title = element_text(size = 20,face = "bold",colour = "black", hjust = 0),
    plot.subtitle = element_text(size = 15, face = "italic", colour = "dark grey", hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
    legend.position="right",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8, face = "italic", colour = "grey")
  )

####Cross-validation####
ctrl <- trainControl(method = "cv", 
                     number = 100, 
                     savePredictions = TRUE)

cvFit <- train(as.factor(flood) ~ st_elev+slope+dis_river+dis_steep+dis_agri+
                 dis_barren+dis_dev+dis_forest+dis_grass+dis_str+
                 forest+barren+grass+develop+agri+river,  data = regdata, 
               method="glm", family="binomial",
               trControl = ctrl)

cvFit

ggplot(as.data.frame(cvFit$resample), aes(Accuracy)) + 
  geom_histogram(bins=30,alpha=0.7,fill="#0868ac") +
  scale_x_continuous(limits = c(0, 1)) +
  labs(title = "K-fold Cross-validation Results",
       subtitle = "K=100",
       x="Accuracy",
       y="Count") + 
  theme(
    text = element_text(size = 12,face = "italic"),
    plot.title = element_text(size = 20,face = "bold",colour = "black", hjust = 0),
    plot.subtitle = element_text(size = 15, face = "italic", colour = "dark grey", hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8, face = "italic", colour = "grey")
  )

####Spatial Cross-validation####
#Elevation
elev_qtl<-quantile(regdata$elev,seq(0,1,0.01))
elev_qtl
elev_high<-regdata %>% filter(elev>elev_qtl[67])
elev_med<-regdata %>% filter(elev>elev_qtl[34] & elev<=elev_qtl[67])
elev_low<-regdata %>% filter(elev<=elev_qtl[34])
regdata<-regdata %>% mutate(elev_level=ifelse(elev>elev_qtl[67],"High",
                                              ifelse(elev>elev_qtl[34] & elev<=elev_qtl[67],"Medium","Low")))

#hold out low elevation
mylogit2 <- glm(flooding_formula,data=rbind(elev_high,elev_med),family = "binomial")

low_test<-predict(mylogit2,elev_low,type="response") %>% as.data.frame()

low_TestProbs <- data.frame(Class =elev_low$flood,
                            Probs =low_test)

colnames(low_TestProbs)[2]<-"Probs"

low_pred <- prediction(low_TestProbs$Probs, low_TestProbs$Class)

low_perf = performance(low_pred, measure = "tpr", x.measure="fpr")

cutoff_low<-opt.cut(low_perf, low_pred)

low_TestProbs<-low_TestProbs %>% mutate(predClass=ifelse(Probs >=cutoff_low,1,0))

confusionMatrix(low_TestProbs$predClass,low_TestProbs$Class)

AUC2<-auc(low_TestProbs$Class,low_TestProbs$Probs)
AUC2

#hold out high elevation
mylogit3 <- glm(flooding_formula,data=rbind(elev_low,elev_med),family = "binomial")

high_test<-predict(mylogit3,elev_high,type="response") %>% as.data.frame()

high_TestProbs <- data.frame(Class =elev_high$flood,
                             Probs =high_test)

colnames(high_TestProbs)[2]<-"Probs"

high_pred <- prediction(high_TestProbs$Probs, high_TestProbs$Class)

high_perf = performance(high_pred, measure = "tpr", x.measure="fpr")

cutoff_high<-opt.cut(high_perf, high_pred)

high_TestProbs<-high_TestProbs %>% mutate(predClass=ifelse(Probs >=cutoff_high,1,0))

confusionMatrix(high_TestProbs$predClass,high_TestProbs$Class)

AUC3<-auc(high_TestProbs$Class,high_TestProbs$Probs)
AUC3

#hold out med elevation
mylogit4 <- glm(flooding_formula,data=rbind(elev_low,elev_high),family = "binomial")

med_test<-predict(mylogit4,elev_med,type="response") %>% as.data.frame()

med_TestProbs <- data.frame(Class =elev_med$flood,
                            Probs =med_test)

colnames(med_TestProbs)[2]<-"Probs"

med_pred <- prediction(med_TestProbs$Probs,med_TestProbs$Class)

med_perf = performance(med_pred, measure = "tpr", x.measure="fpr")

cutoff_med<-opt.cut(med_perf, med_pred)

med_TestProbs<-med_TestProbs %>% mutate(predClass=ifelse(Probs >=cutoff_med,1,0))

confusionMatrix(med_TestProbs$predClass,med_TestProbs$Class)

AUC4<-auc(med_TestProbs$Class,med_TestProbs$Probs)
AUC4

elev_cv<-c(AUC2,AUC3,AUC4) %>% as.data.frame()
colnames(elev_cv)<-c("AUC")
rownames(elev_cv)<-c("Low_elev","High_elev","Med_elev")

#ROC
ggplot(low_TestProbs, aes(d = Class, m = Probs)) + 
  geom_roc(n.cuts = 50, labels = FALSE,color="#43a2ca") + 
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC",
       subtitle = "Spatial Cross-validation: Low Elevation") + 
  theme(
    text = element_text(size = 12,face = "italic"),
    plot.title = element_text(size = 20,face = "bold",colour = "black", hjust = 0),
    plot.subtitle = element_text(size = 15, face = "italic", colour = "dark grey", hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8, face = "italic", colour = "grey")
  )

ggplot(med_TestProbs, aes(d = Class, m = Probs)) + 
  geom_roc(n.cuts = 50, labels = FALSE,color="#43a2ca") + 
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC",
       subtitle = "Spatial Cross-validation: Medium Elevation") + 
  theme(
    text = element_text(size = 12,face = "italic"),
    plot.title = element_text(size = 20,face = "bold",colour = "black", hjust = 0),
    plot.subtitle = element_text(size = 15, face = "italic", colour = "dark grey", hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8, face = "italic", colour = "grey")
  )


ggplot(high_TestProbs, aes(d = Class, m = Probs)) + 
  geom_roc(n.cuts = 50, labels = FALSE,color="#43a2ca") + 
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC",
       subtitle = "Spatial Cross-validation: High Elevation") + 
  theme(
    text = element_text(size = 12,face = "italic"),
    plot.title = element_text(size = 20,face = "bold",colour = "black", hjust = 0),
    plot.subtitle = element_text(size = 15, face = "italic", colour = "dark grey", hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8, face = "italic", colour = "grey")
  )

####Predict for Indianapolis####
Indi_results<-predict(mylogit,Indianapolis_predictors,type="response")
Indi_prediction<-cbind(Indianapolis,Indi_results)
Indi_prediction<-Indi_prediction %>% mutate(pred=ifelse(Indi_results>cutoff,1,0))
Indi_pred_save<-Indi_prediction

st_geometry(Indi_pred_save)<-NULL
write.csv(Indi_pred_save,file="Indi_prediction.csv",row.names=FALSE)

count_fld_indi<-Indi_prediction %>% group_by(pred) %>% dplyr::summarise(count=n())
