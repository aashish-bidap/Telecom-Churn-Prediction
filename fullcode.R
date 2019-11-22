library(ggplot2)
library(dbplyr)
library(gridExtra)

my_data <- read.csv("/Users/abhishekbidap/Desktop/my_stuff/Intermediate Analytics/Final Project/WA_Fn-UseC_-Telco-Customer-Churn.csv")

my_missing_NA_value_function <- function(dataset){
  
  Total_NA <- sum(is.na(dataset))
  Column_sums <- colSums(is.na(dataset))
  cat("\n\n Total NA in the dataset - \n",Total_NA)
  cat('\n\n Total NA by column in the dataset-\n',Column_sums)
  Column_names <- colnames(dataset)[apply(dataset,2,anyNA)]
  cat('\n\n Names of NA columns in the dataset-\n',Column_names)
}


my_data_overview <- function(dataset){
  data <- dim(dataset)
  cat("\nTotal Number of [rows vs columns] in the dataset- \n",data)
  Column_datatypes <- sapply(dataset,class)
  cat('\n\n Datatypes of all the columns in the dataset-\n',Column_datatypes)
  Column_Names <- colnames(dataset)
  cat('\n\n Names of all the columns in the dataset-\n',Column_Names)
}

#function calls
my_missing_NA_value_function(my_data)
my_data_overview(my_data)

#count of missing values
sum(is.na(my_data$TotalCharges))

#missing value deletion
my_data <- na.omit(my_data)

#Graphical Representation of the Numerical Variables histograms
ggplot(my_data,aes(x = tenure))+ geom_bar(fill="Blue",width=0.4)
ggplot(my_data,aes(x = MonthlyCharges))+ geom_bar(fill="Blue",width=0.4)
ggplot(my_data,aes(x = TotalCharges))+ geom_bar(fill="Blue",width=0.4)

summary(my_data$tenure)
summary(my_data$MonthlyCharges)
summary(my_data$TotalCharges)

#numerical
grid.arrange(plot1,plot2,plot3)

#Graphical Representation of the Categorical Variables histograms for each of the columns
plot4 <- ggplot(data=my_data) + geom_point(mapping = aes(x = PaymentMethod),fill="Blue",width=0.4)
plot5 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = gender),fill="Blue",width=0.4)
plot6 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = SeniorCitizen),fill="Blue",width=0.4)
plot7 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = Partner),fill="Blue",width=0.4)
plot8 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = Dependents),fill="Blue",width=0.4)
plot9 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = PhoneService),fill="Blue",width=0.4)
plot10 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = MultipleLines),fill="Blue",width=0.4)
plot11 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = InternetService),fill="Blue",width=0.4)
plot12 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = OnlineSecurity),fill="Blue",width=0.4)
plot13 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = OnlineBackup),fill="Blue",width=0.4)
plot14 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = DeviceProtection),fill="Blue",width=0.4)
plot15 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = TechSupport),fill="Blue",width=0.4)
plot16 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = StreamingTV),fill="Blue",width=0.4)
plot17 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = StreamingMovies),fill="Blue",width=0.4)
plot18 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = Contract),fill="Blue",width=0.4)
plot19 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = PaperlessBilling),fill="Blue",width=0.4)
plot20 <- ggplot(data=my_data) + geom_bar(mapping = aes(x = Churn),fill="Blue",width=0.4)

#categorical
grid.arrange(plot5,plot6,plot7,plot8)
grid.arrange(plot9,plot10,plot11,plot12)
grid.arrange(plot13,plot14,plot15,plot16)
grid.arrange(plot17,plot18,plot19,plot20)

#Numerical Summaries of Categorical Variables Exploration
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

#Relationship between the variables (Numerical variables)
ggplot(my_data, aes(x = tenure , y = TotalCharges)) + geom_point(aes(colour=factor(Churn)))
#ggplot(my_data, aes(x = tenure , y = TotalCharges)) + geom_point(color='Blue')

ggplot(my_data, aes(x = MonthlyCharges , y = TotalCharges)) + geom_point(aes(colour=factor(Churn)))
#Relationship between the variables (Categorical variables)

#Relationship between the variables (dependent variable vs Categorical Variables) 
#gender vs churn
plot_relation_1 =ggplot(my_data, aes(x = gender,fill=Churn)) + geom_bar(width = 0.4)
#SeniorCitizen vs Churn
plot_relation_2 =ggplot(my_data, aes(x = SeniorCitizen,fill=Churn)) + geom_bar(width = 0.4)
#partner vs Churn
plot_relation_3 =ggplot(my_data, aes(x = Partner,fill=Churn)) + geom_bar(width = 0.4)
#Dependents vs Churn
plot_relation_4 =ggplot(my_data, aes(x = Dependents,fill=Churn)) + geom_bar(width = 0.4)
#phoneservices vs Churn
plot_relation_5 =ggplot(my_data, aes(x = PhoneService,fill=Churn)) + geom_bar(width = 0.4)
#MultipleLines vs Churn
plot_relation_6 =ggplot(my_data, aes(x = MultipleLines,fill=Churn)) + geom_bar(width = 0.4)
#InternetServices vs Churn
plot_relation_7 =ggplot(my_data, aes(x = InternetService,fill=Churn)) + geom_bar(width = 0.4)
#OnlineSecurity vs Churn
plot_relation_8 =ggplot(my_data, aes(x = OnlineSecurity,fill=Churn)) + geom_bar(width = 0.4)
#Onlinebackup vs Churn
plot_relation_9 =ggplot(my_data, aes(x = OnlineBackup,fill=Churn)) + geom_bar(width = 0.4)
#DeviceProtection vs Churn
plot_relation_10 =ggplot(my_data, aes(x = DeviceProtection,fill=Churn)) + geom_bar(width = 0.4)
#TechSupport vs Churn
plot_relation_11 =ggplot(my_data, aes(x = TechSupport,fill=Churn)) + geom_bar(width = 0.4)
#StreamingTV vs Churn
plot_relation_12 =ggplot(my_data, aes(x = StreamingTV,fill=Churn)) + geom_bar(width = 0.4)
#StreamingMovies vs Churn
plot_relation_13 =ggplot(my_data, aes(x = StreamingMovies,fill=Churn)) + geom_bar(width = 0.4)
#Contract vs Churn
plot_relation_14 =ggplot(my_data, aes(x = Contract,fill=Churn)) + geom_bar(width = 0.4)
#PaperlessBilling vs Churn
plot_relation_15 =ggplot(my_data, aes(x = PaperlessBilling,fill=Churn)) + geom_bar(width = 0.4)
#PaymentMethod vs Churn
plot_relation_16 =ggplot(my_data, aes(x = PaymentMethod,fill=Churn)) + geom_bar(width = 0.4)


grid.arrange(plot_relation_1,plot_relation_2,plot_relation_3,plot_relation_4)
grid.arrange(plot_relation_5,plot_relation_6,plot_relation_7,plot_relation_8)
grid.arrange(plot_relation_9,plot_relation_10,plot_relation_11,plot_relation_12)
grid.arrange(plot_relation_13,plot_relation_14,plot_relation_15,plot_relation_16)
head(my_data)


#---check check


