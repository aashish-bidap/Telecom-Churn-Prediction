#libraries
library(ggplot2)
library(dbplyr)
library(dplyr)
library(gridExtra)
library(corrplot)
library(dummies)
library(party)
library(MASS)
library(pROC)
library(randomForest)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)

#importing data file
my_data <- read.csv("/Users/abhishekbidap/Desktop/my_stuff/Intermediate Analytics/Final Project/Project Stuff/WA_Fn-UseC_-Telco-Customer-Churn.csv")

str(my_data)

########################################################################
#-------------------------**DATA OVERVIEW**------------------------
########################################################################

my_missing_NA_value_function <- function(dataset){
  
  Total_NA <- sum(is.na(dataset))
  Column_sums <- colSums(is.na(dataset))
  cat("\n\n Total NA in the dataset - \n\n",Total_NA)
  cat('\n\n Total NA by column in the dataset-\n\n',Column_sums)
  Column_names <- colnames(dataset)[apply(dataset,2,anyNA)]
  cat('\n\n Names of NA columns in the dataset-\n\n',Column_names)
}


my_data_overview <- function(dataset){
  data <- dim(dataset)
  cat("\nTotal Number of [rows vs columns] in the dataset- \n",data)
  Column_datatypes <- sapply(dataset,class)
  cat('\n\n Datatypes of all the columns in the dataset-\n',Column_datatypes)
  Column_Names <- colnames(dataset)
  cat('\n\n Names of all the columns in the dataset-\n',Column_Names)
  apply(dataset,2,function(x) length(unique(x)))
}


#function calls to understand the overview of the data
my_missing_NA_value_function(my_data)
my_data_overview(my_data)

#Unique data in each column
apply(my_data,2,function(x) length(unique(x)))


########################################################################
#-------------------------**DATA MANIPULATION**------------------------
########################################################################

#count of missing values
sum(is.na(my_data$TotalCharges))

#missing value deletion
my_data <- na.omit(my_data)

#senior citizen is in integer form
my_data$SeniorCitizen <- as.factor(my_data$SeniorCitizen)

#outlier check
par(mfrow=c(1,2))
boxplot(my_data$MonthlyCharges,outline = T,col="Steelblue",notch = T,xlab="MonthlyCharges")
boxplot(my_data$TotalCharges,outline = T,col="Steelblue",notch = T,xlab="TotalCharges")

########################################################################
#------------------**EXPLORATORY DATA ANALYSIS**------------------------
########################################################################

#---Data transformation for the tenure column from months to years.

my_data <- mutate(my_data,tenure_bin=tenure)
my_data$tenure_bin[my_data$tenure_bin >= 0 & my_data$tenure_bin <= 12]   <- "0 - 1 years"
my_data$tenure_bin[my_data$tenure_bin >= 13 & my_data$tenure_bin <= 24]  <- "1 - 2 years"
my_data$tenure_bin[my_data$tenure_bin >= 25 & my_data$tenure_bin <= 36]  <- "2 - 3 years"
my_data$tenure_bin[my_data$tenure_bin >= 37 & my_data$tenure_bin <= 48]  <- "3 - 4 years"
my_data$tenure_bin[my_data$tenure_bin >= 49 & my_data$tenure_bin <= 60]  <- "4 - 5 years"
my_data$tenure_bin[my_data$tenure_bin >= 61 & my_data$tenure_bin <= 72]  <- "5 - 6 years"


#converting the newly created column into factor tenure_bin.
my_data$tenure_bin = as.factor(my_data$tenure_bin)

#Graphical Representation of the Numerical Variables histograms
plot_A <- ggplot(my_data,aes(x = tenure_bin,fill=tenure_bin))+ geom_bar(width=0.4)
plot_B <- ggplot(my_data,aes(x = MonthlyCharges))+ geom_bar(fill="Steelblue",width=0.4)

#numerical
grid.arrange(plot_A,plot_B)

summary(my_data$MonthlyCharges)
summary(my_data$TotalCharges)
#Graphical Representation of the Categorical Variables histograms for each of the columns
plot4 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = PaymentMethod,y=..prop..,group=2),fill="Steelblue",stat='count',width=0.4)+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
plot5 <- ggplot(data=my_data) +
  geom_bar(mapping = aes(x = gender,y=..prop..,group=2),fill="Steelblue",stat='count',width=0.4)
plot6 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = SeniorCitizen,y=..prop..,group=2),fill="Steelblue",stat='count',width=0.4)
plot7 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = Partner,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot8 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = Dependents,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot9 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = PhoneService,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot10 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = MultipleLines,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')

plot11 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = InternetService,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot12 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = OnlineSecurity,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot13 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = OnlineBackup,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot14 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = DeviceProtection,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot15 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = TechSupport,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot16 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = StreamingTV,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot17 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = StreamingMovies,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot18 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = Contract,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot19 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = PaperlessBilling,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')
plot20 <- ggplot(data=my_data) + 
  geom_bar(mapping = aes(x = Churn,y=..prop..,group=2),fill="Steelblue",width=0.4,stat='count')

#categorical
grid.arrange(plot5,plot19,plot20,nrow=1,ncol=3)

grid.arrange(plot11,plot12,plot13,plot14,plot15,plot16,nrow=3,ncol=2)

#Numerical Summaries of  Variables 
#Gender
type_counts1 <- table(my_data$gender)
type_counts1 / sum(type_counts1) * 100
#SeniorCitizen
type_counts2 <- table(my_data$SeniorCitizen)
type_counts2 / sum(type_counts2) * 100
#Partner
type_counts3 <- table(my_data$Partner)
type_counts3 / sum(type_counts3) * 100
#Dependents
type_counts4 <- table(my_data$Dependents)
type_counts4 / sum(type_counts4) * 100
             
#PhoneService
type_counts5 <- table(my_data$PhoneService)
type_counts5 / sum(type_counts5) * 100
#MultipleLines
type_counts6 <- table(my_data$MultipleLines)
type_counts6 / sum(type_counts6) * 100
#MultipleLines
type_counts7 <- table(my_data$InternetService)
type_counts7 / sum(type_counts7) * 100

#OnlineSecurity
type_counts8 <- table(my_data$OnlineSecurity)
type_counts8 / sum(type_counts8) * 100
              
#OnlineBackup
type_counts9 <- table(my_data$OnlineBackup)
type_counts9 / sum(type_counts9) * 100

#DeviceProtection
type_counts10 <- table(my_data$DeviceProtection)
type_counts10 / sum(type_counts10) * 100

#TechSupport
type_counts11 <- table(my_data$TechSupport)
type_counts11 / sum(type_counts11) * 100

#StreamingTV
type_counts12 <- table(my_data$StreamingTV)
type_counts12 / sum(type_counts12) * 100

#STreaming Movies
type_counts13 <- table(my_data$StreamingMovies)
type_counts13 / sum(type_counts13) * 100

#Contract
type_counts14 <- table(my_data$Contract)
type_counts14 / sum(type_counts14) * 100

#PaperlessBilling
type_counts15 <- table(my_data$PaperlessBilling)
type_counts15 / sum(type_counts15) * 100

#PaymentMethod
type_counts16 <- table(my_data$PaymentMethod)
type_counts16 / sum(type_counts16) * 100

#Churn
type_counts17 <- table(my_data$Churn)
type_counts17 / sum(type_counts17) * 100

#All 9% of No phone Service subscribers were active on Internet Service.
my_data[my_data$PhoneService == 'No',"InternetService"]

#Relationship between the variables (Numerical variables)
ggplot(my_data, aes(x = tenure_bin , y = TotalCharges)) + geom_point(aes(colour=factor(Churn)))
#ggplot(my_data, aes(x = tenure , y = TotalCharges)) + geom_point(color='Steelblue')

ggplot(my_data, aes(x = MonthlyCharges , y = TotalCharges)) + geom_point(aes(colour=factor(Churn)))
#Relationship between the variables (Categorical variables)

#Relationship between the variables (dependent variable vs Categorical Variables) 
#gender vs churn
plot_relation_1 =ggplot(my_data, aes(x = gender,fill=Churn)) + 
  geom_bar(width = 0.4)
#SeniorCitizen vs Churn
plot_relation_2 =ggplot(my_data, aes(x = SeniorCitizen,fill=Churn)) + 
  geom_bar(width = 0.4)
#partner vs Churn
plot_relation_3 =ggplot(my_data, aes(x = Partner,fill=Churn)) + 
  geom_bar(width = 0.4)
#Dependents vs Churn
plot_relation_4 =ggplot(my_data, aes(x = Dependents,fill=Churn)) + 
  geom_bar(width = 0.4)
#phoneservices vs Churn
plot_relation_5 =ggplot(my_data, aes(x = PhoneService,fill=Churn)) + 
  geom_bar(width = 0.4)
#MultipleLines vs Churn
plot_relation_6 =ggplot(my_data, aes(x = MultipleLines,fill=Churn)) + 
  geom_bar(width = 0.4)
#InternetServices vs Churn
plot_relation_7 =ggplot(my_data, aes(x = InternetService,fill=Churn)) + 
  geom_bar(width = 0.4)
#OnlineSecurity vs Churn
plot_relation_8 =ggplot(my_data, aes(x = OnlineSecurity,fill=Churn)) + 
  geom_bar(width = 0.4)
#Onlinebackup vs Churn
plot_relation_9 =ggplot(my_data, aes(x = OnlineBackup,fill=Churn)) + 
  geom_bar(width = 0.4)
#DeviceProtection vs Churn
plot_relation_10 =ggplot(my_data, aes(x = DeviceProtection,fill=Churn)) + 
  geom_bar(width = 0.4)
#TechSupport vs Churn
plot_relation_11 =ggplot(my_data, aes(x = TechSupport,fill=Churn)) + 
  geom_bar(width = 0.4)
#StreamingTV vs Churn
plot_relation_12 =ggplot(my_data, aes(x = StreamingTV,fill=Churn)) + 
  geom_bar(width = 0.4)
#StreamingMovies vs Churn
plot_relation_13 =ggplot(my_data, aes(x = StreamingMovies,fill=Churn)) + 
  geom_bar(width = 0.4)
#Contract vs Churn
plot_relation_14 =ggplot(my_data, aes(x = Contract,fill=Churn)) + 
  geom_bar(width = 0.4)
#PaperlessBilling vs Churn
plot_relation_15 =ggplot(my_data, aes(x = PaperlessBilling,fill=Churn)) + 
  geom_bar(width = 0.4)
#PaymentMethod vs Churn
plot_relation_16 =ggplot(my_data, aes(x = PaymentMethod,fill=Churn)) + 
  geom_bar(width = 0.4)


grid.arrange(plot_relation_1,plot_relation_2,plot_relation_3,plot_relation_4,plot_relation_5,plot_relation_6,plot_relation_15,plot_relation_16)
grid.arrange(plot_relation_9,plot_relation_10,plot_relation_11,plot_relation_12,plot_relation_7,plot_relation_8,plot_relation_13,plot_relation_14)


#############################################################################
#----------------------**FEATURE ENGINEERING**--------------------------------
#############################################################################


#---converting the No phone service column to No.

#MultipleLines No phone service -> No
my_data$MultipleLines[which(my_data$MultipleLines=="No phone service")]<-"No"

#creating dummy variables 
my_data_cat <- my_data[,-c(1,3,6,19,20)]
dummy<- data.frame(sapply(my_data_cat,function(x) data.frame(model.matrix(~x-1,data = my_data))[,-1]))
my_new_data_set <- cbind(my_data[,c(1,3,6,19,20)],dummy)

#now that we dont need the customerID i ll be removing it off from the dataset.
my_new_data_set$customerID <- NULL

#now that we dont need the TENURE  i ll be removing it off from the dataset.
my_new_data_set$tenure <- NULL


#data splliting in 70-30 ratio.
num = sample(2,nrow(my_new_data_set),replace = T,prob = c(0.7,0.3))

train_data <-  my_new_data_set[num==1,]
test_data  <-  my_new_data_set[num==2,]


#############################################################################
#----------------------**MODEL PROCESSING**--------------------------------
#############################################################################

#-------------------------------------
##--LOGISTIC REGRESSION
#-------------------------------------

log_model <- glm(Churn ~ .,family ="binomial", data=train_data)

summary(log_model)

step_2 <- stepAIC(log_model,direction = "both")

#Using the final Fit model.
log_model <- glm(Churn ~ SeniorCitizen + TotalCharges + PhoneService + MultipleLines.xYes + 
                   InternetService.xFiber.optic + InternetService.xNo + OnlineSecurity.xYes + 
                   OnlineBackup.xYes + TechSupport.xYes + StreamingTV.xYes + 
                   StreamingMovies.xYes + Contract.xOne.year + Contract.xTwo.year + 
                   PaperlessBilling + PaymentMethod.xElectronic.check + tenure_bin.x1...2.years + 
                   tenure_bin.x2...3.years + tenure_bin.x3...4.years + tenure_bin.x4...5.years + 
                   tenure_bin.x5...6.years,family ="binomial", data=train_data)
summary(log_model)

predict_log_model <- predict(log_model,newdata=test_data) 

length(predict_log_model)

length(test_data$Churn)

y_pred_num <- ifelse(predict_log_model > 0.5, 1, 0)

y_predicted <- factor(y_pred_num, levels=c(0,1))

y_observed <- test_data$Churn

predict_log_model

LM_confusionMatrix <- confusionMatrix(y_predicted,as.factor(y_observed))

Accuracy_LM <- mean(y_observed == y_predicted)

roc_lm <- roc(test_data$Churn,predict_log_model,plot=TRUE,auc = TRUE,precent=T,auc.polygon=TRUE,grid=T)

fourfoldplot(LM_confusionMatrix$table,main = "Confusion Matrix Logistic Regression")

#-------------------------------------
##--DECISION TREE MODELLING
#-------------------------------------

train_data$Churn <- as.factor(train_data$Churn)

test_data$Churn <- as.factor(test_data$Churn)

decision_model <- rpart(Churn ~ ., data=train_data,method="class")

printcp(decision_model)

plotcp(decision_model)

rpart.plot(decision_model,type = 4,extra=101)

predict_decision_model <- predict(decision_model,newdata=test_data,type='class')

table(predict_decision_model,test_data$Churn)

DT_confusionMatrix <- confusionMatrix(predict_decision_model,as.factor(test_data$Churn))

pred <- predict(decision_model,test_data,type = 'prob')

roc_dm <- roc(test_data$Churn,pred[,2],auc.polygon=T,auc=T,grid=T,precent=T)

Accuracy_DT <- mean(y_observed == predict_decision_model)

fourfoldplot(DT_confusionMatrix$table,main="Confusion Matrix Decision Tree Model")

#--------------------------------
## - Random Forest Modelling
#--------------------------------

train_data$Churn <- as.factor(train_data$Churn)

test_data$Churn <- as.factor(test_data$Churn)

forest_model <- randomForest(Churn ~ .,data = train_data,proximity=TRUE)

plot(forest_model)

predict_forest_model <- predict(forest_model,newdata=test_data,type ="prob" )

predict_forest_model_cm<- predict(forest_model,newdata=test_data)

table(predict_forest_model_cm,test_data$Churn)

RF_confusionMatrix <- confusionMatrix(predict_forest_model_cm,as.factor(test_data$Churn))

fourfoldplot(RF_confusionMatrix$table,main = "Confusion Matrix for Random Forest Model")

roc_rfm <- roc(test_data$Churn,predict_forest_model[,2],auc.polygon=T,auc=T,grid=T,precent=T)

Accuracy_RF <-  mean(y_observed == predict_forest_model_cm)

#---------------------------------------------------
#   -------Accuracy for the three Models
#---------------------------------------------------

cat("Accuracy for Logistic Model" ,Accuracy_LM)
cat("\nAccuracy for Decision Tree Model",Accuracy_DT)
cat("\nAccuracy for Random Forest Model",Accuracy_RF)

#---------------------------------------------------
#   -------ROC analysis for the three Models
#---------------------------------------------------

#First curve:
plot(roc_rfm, col = "red", lty = 2, main = "ROC analysis for Random Forest Model-Logistic Model-Decision Tree Model")
# to add to the same graph: add=TRUE
plot(roc_lm, col = "green", lty = 3, add = TRUE)
plot(roc_dm, col = "black", lty = 10, add = TRUE)
legend(0.6,0.45, c('Random Forest','Logistic','Decision'),lty=c(1,1),lwd=c(2,2),col=c('red','green','black'))


#---------------------------------------------------
#   -------Business Cost Assumptions----------------
#---------------------------------------------------


# 1 1 TRUE POSITIVE  
# 0 0 TRUE NEGATIVE  
# 0 1 FALSE NEGATIVE 
# 1 0 FALSE POSITIVE  

#If no predictive model is implemented by the company
Total_Cost_No_model <- length(test_data$Churn[test_data$Churn == 1]) * 500
Total_Cost_No_model

#Logistic regression 
LM_acuracy <- data.frame(LM_confusionMatrix$table)

TP_LM <- LM_acuracy$Freq[LM_acuracy$Prediction == 0 & LM_acuracy$Reference == 0]
TN_LM <- LM_acuracy$Freq[LM_acuracy$Prediction == 1 & LM_acuracy$Reference == 1]
FN_LM <- LM_acuracy$Freq[LM_acuracy$Prediction == 0 & LM_acuracy$Reference == 1]
FP_LM <- LM_acuracy$Freq[LM_acuracy$Prediction == 1 & LM_acuracy$Reference == 0]

Total_Cost_LM <- FN_LM * 500 + TN_LM * 100 + FP_LM * 100
Total_Cost_LM

#Decision tree
DT_acuracy <- data.frame(DT_confusionMatrix$table)

TP_DT <- DT_acuracy$Freq[DT_acuracy$Prediction == 0 & DT_acuracy$Reference == 0]
TN_DT <- DT_acuracy$Freq[DT_acuracy$Prediction == 1 & DT_acuracy$Reference == 1]
FN_DT <- DT_acuracy$Freq[DT_acuracy$Prediction == 0 & DT_acuracy$Reference == 1]
FP_DT <- DT_acuracy$Freq[DT_acuracy$Prediction == 1 & DT_acuracy$Reference == 0]

Total_Cost_DT <- FN_DT * 500 + TN_DT * 100 + FP_DT * 100
Total_Cost_DT

#Random Forest
RF_acuracy <- data.frame(RF_confusionMatrix$table)

TP_RF <- RF_acuracy$Freq[RF_acuracy$Prediction == 0 & RF_acuracy$Reference == 0]
TN_RF <- RF_acuracy$Freq[RF_acuracy$Prediction == 1 & RF_acuracy$Reference == 1]
FN_RF <- RF_acuracy$Freq[RF_acuracy$Prediction == 0 & RF_acuracy$Reference == 1]
FP_RF <- RF_acuracy$Freq[RF_acuracy$Prediction == 1 & RF_acuracy$Reference == 0]

Total_Cost_RF <- FN_RF * 500 + TN_RF * 100 + FP_RF * 100
Total_Cost_RF

x <- c("Cost_logitModel","Cost_DecisionTree","Cost_RandomForest","Cost_No_model")
y <- c(Total_Cost_LM,Total_Cost_DT,Total_Cost_RF,Total_Cost_No_model)

model_cost <- data.frame("Model" = x,"Cost" = y)


ggplot(model_cost) + 
geom_bar(aes(x=model_cost$Model,y=model_cost$Cost),stat ="identity",fill="steelblue") +
#coord_flip()+
xlab("Model Name")+
ylab("Model Cost")+
labs(title = "Business Cost Assumptions for all Models")



