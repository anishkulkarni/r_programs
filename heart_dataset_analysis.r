#Only 14 attributes used:
#1. (age)
#2. (sex)
#3. (cp)
#cp: chest pain type
#-- Value 1: typical angina
#-- Value 2: atypical angina
#-- Value 3: non-anginal pain
#-- Value 4: asymptomatic 
#4. (trestbps)
# trestbps: resting blood pressure (in mm Hg on admission to the hospital)
#. (chol)
#chol: serum cholestoral in mg/dl
#6. (fbs)
#fbs: (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false) 
#7. (restecg)
#restecg: resting electrocardiographic results 
#8. (thalach)
#halach: maximum heart rate achieved 
#9. (exang)
#exang: exercise induced angina (1 = yes; 0 = no) 
#0. (oldpeak)
#oldpeak = ST depression induced by exercise relative to rest
#11. (slope)
#slope: the slope of the peak exercise ST segment
#-- Value 1: upsloping
#- Value 2: flat
#-- Value 3: downsloping 
#12. (ca)
#ca: number of major vessels (0-3) colored by flourosopy 
#13.  (thal)
#thal: 3 = normal; 6 = fixed defect; 7 = reversable defect 
#14. (num) (the predicted attribute)
#num: diagnosis of heart disease (angiographic disease status)
#-- Value 0: < 50% diameter narrowing
#-- Value 1: > 50% diameter narrowing
#in any major vessel: attributes 59 through 68 are vessels) 

#load data
hdata=read.csv(path,header=FALSE,sep=",")

#set column names
colnames(hdata)<- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")

#set non-zero values to 1
hdata$num[hdata$num>0]<-1

#replace ? with NA
levels(hdata$thal)[levels(hdata$thal)=="?"]<-NA

#convert factors to numeric
hdata$ca <- as.numeric(as.character(hdata$ca))
hdata$thal<-as.numeric(as.character(hdata$thal))

#replace ? and outliers with mean
for(i in 1:ncol(hdata))
{
  without_question_mark=hdata[-which(hdata[,i] %in% "?"),]
  hdata[,i][hdata[,i] %in% "?"]=mean(without_question_mark[,i])
  outliers=boxplot(hdata[,i])$out
  without_outliers=hdata[-which(hdata[,i] %in% outliers),]
  hdata[,i][hdata[,i] %in% outliers]=mean(without_outliers[,i])
}

#divide dataset into training and testing subsets
library(caTools) 
hdata[, c(1)] <- sapply(hdata[, c(1)],as.numeric) 
set.seed(123)
split = sample.split(hdata$num,SplitRatio = 2/3) 
train_hdata = subset(hdata, split ==TRUE) 
test_hdata = subset(hdata, split == FALSE)

#train linear regression model
library(caTools)
regressor=lm(formula = num~age, data=train_hdata)

#predics values using model
hd_age_predict=predict(regressor, newdata=test_hdata)

#round the fractional values
round_age=hd_age_predict
rage=round(round_age)

#import libraries
library(e1071)
library(caret)
Loading required package: lattice

#convert rounded predictions to data frame
r1=as.data.frame(rage)

#generate confusion matrix
confusionMatrix(as.factor(r1$rage),as.factor(test_hdata$num))
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 35 20
#          1 20 26
                                          
#                Accuracy : 0.604           
#                  95% CI : (0.5017, 0.6999)
#     No Information Rate : 0.5446          
#     P-Value [Acc > NIR] : 0.1357          
                                          
#                   Kappa : 0.2016          
                                          
#  Mcnemar's Test P-Value : 1.0000          
                                          
#             Sensitivity : 0.6364          
#             Specificity : 0.5652          
#          Pos Pred Value : 0.6364          
#          Neg Pred Value : 0.5652          
#              Prevalence : 0.5446          
#          Detection Rate : 0.3465          
#    Detection Prevalence : 0.5446          
#       Balanced Accuracy : 0.6008          
                                          
#        'Positive' Class : 0

#multiple linear regression
regressor = lm(formula = num~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope, data=train_hdata)

#predict values from multiple linear regression
hd_age_predict=predict(regressor, newdata=test_hdata)

#round values
round_age=hd_age_predict
rage=round(round_age)

#convert to data frame
r1=as.data.frame(rage)

#confusion matrix
confusionMatrix(as.factor(r1$rage),as.factor(test_hdata$num))
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 43 12
#          1 12 34
                                          
#                Accuracy : 0.7624          
#                  95% CI : (0.6674, 0.8414)
#     No Information Rate : 0.5446          
#     P-Value [Acc > NIR] : 4.892e-06       
                                          
#                   Kappa : 0.5209          
                                          
#  Mcnemar's Test P-Value : 1               
                                          
#             Sensitivity : 0.7818          
#             Specificity : 0.7391          
#          Pos Pred Value : 0.7818          
#          Neg Pred Value : 0.7391          
#              Prevalence : 0.5446          
#          Detection Rate : 0.4257          
#    Detection Prevalence : 0.5446          
#       Balanced Accuracy : 0.7605          
                                          
#        'Positive' Class : 0

#define function normallize
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }

#normalize data
h1data<-hdata
h1data_n <- as.data.frame(lapply(h1data[1:11], normalize))

#split data into training and testing sets
traink_hdata=h1data_n[1:212,]
testk_hdata=h1data_n[213:303,]

#remove NaN, replacing with 0
traink_hdata$fbs = 0
testk_hdata$fbs = 0

#set training and testing labels
h1data_train_labels <- hdata[1:212, 14]
h1data_test_labels <- hdata[213:303, 14]

#include library
library(class)

#train knn model and predict values of test set
h1data_test_pred <- knn(train=traink_hdata, test=testk_hdata, cl=h1data_train_labels, k=7)

#include llibrary
library(gmodels)

#generate cross table of actual labels and predicted labels
CrossTable(x=h1data_test_labels,y=h1data_test_pred,prop.chisq = FALSE)

 
#    Cell Contents
# |-------------------------|
# |                       N |
# |           N / Row Total |
# |           N / Col Total |
# |         N / Table Total |
# |-------------------------|

 
# Total Observations in Table:  91 

 
#                    | h1data_test_pred 
# h1data_test_labels |         0 |         1 | Row Total | 
# -------------------|-----------|-----------|-----------|
#                  0 |        41 |         7 |        48 | 
#                    |     0.854 |     0.146 |     0.527 | 
#                    |     0.732 |     0.200 |           | 
#                    |     0.451 |     0.077 |           | 
# -------------------|-----------|-----------|-----------|
#                  1 |        15 |        28 |        43 | 
#                    |     0.349 |     0.651 |     0.473 | 
#                    |     0.268 |     0.800 |           | 
#                    |     0.165 |     0.308 |           | 
# -------------------|-----------|-----------|-----------|
#       Column Total |        56 |        35 |        91 | 
#                    |     0.615 |     0.385 |           | 
# -------------------|-----------|-----------|-----------|

#simpler form of cross table
table(h1data_test_labels,h1data_test_pred)
#                   h1data_test_pred
# h1data_test_labels  0  1
#                  0 41  7
#                  1 15 28

#generate confusion matrix for knn predictions
confusionMatrix(as.factor(h1data_test_labels),as.factor(h1data_test_pred))
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 41  7
#          1 15 28
                                          
#                Accuracy : 0.7582          
#                  95% CI : (0.6572, 0.8419)
#     No Information Rate : 0.6154          
#     P-Value [Acc > NIR] : 0.002826        
                                          
#                   Kappa : 0.5103          
                                          
#  Mcnemar's Test P-Value : 0.135593        
                                          
#             Sensitivity : 0.7321          
#             Specificity : 0.8000          
#          Pos Pred Value : 0.8542          
#          Neg Pred Value : 0.6512          
#              Prevalence : 0.6154          
#          Detection Rate : 0.4505          
#    Detection Prevalence : 0.5275          
#       Balanced Accuracy : 0.7661          
                                          
#        'Positive' Class : 0

#kmeans clustering
#set random seed
set.seed(123)

#form 2 clusters of 4th and 5th column
clusters=kmeans(hdata[,4:5],2)

#describe clusters
str(clusters)
# List of 9
#  $ cluster     : int [1:303] 2 1 2 2 2 2 1 1 1 2 ...
#  $ centers     : num [1:2, 1:2] 131 129 288 214
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : chr [1:2] "1" "2"
#   .. ..$ : chr [1:2] "trestbps" "chol"
#  $ totss       : num 672961
#  $ withinss    : num [1:2] 109762 167087
#  $ tot.withinss: num 276849
#  $ betweenss   : num 396111
#  $ size        : int [1:2] 122 181
#  $ iter        : int 1
#  $ ifault      : int 0
#  - attr(*, "class")= chr "kmeans"

#import library for visualization
 >library(ggplot2)

 #create new data frame, adding column cluster to hdata
 clust_d <- data.frame(hdata, cluster=factor(clusters$cluster))

 #visualize the clusters
 ggplot() + geom_point(data = clust_d, mapping = aes(x = trestbps,y = chol,colour = cluster)) + scale_color_manual(values=c("black", "blue")) + geom_point(mapping = aes_string(x = clusters$centers[, "trestbps"], y = clusters$centers[, "chol"]), color = "red", size = 4)
