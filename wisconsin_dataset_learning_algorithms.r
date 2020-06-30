#dataset description
#   Attribute                     Domain
#   -- -----------------------------------------
#   1. Sample code number            id number
#   2. Clump Thickness               1 - 10
#  3. Uniformity of Cell Size       1 - 10
#   4. Uniformity of Cell Shape      1 - 10
#   5. Marginal Adhesion             1 - 10
#   6. Single Epithelial Cell Size   1 - 10
#  7. Bare Nuclei                   1 - 10
#  8. Bland Chromatin               1 - 10
#  9. Normal Nucleoli               1 - 10
# 10. Mitoses                       1 - 10
#  11. Class:                        (2 for benign, 4 for malignant)
  
#  Missing attribute values: 16

#  There are 16 instances in Groups 1 to 6 that contain a single missing 
#   (i.e., unavailable) attribute value, now denoted by "?".  
#
# Class distribution:
# 
#   Benign: 458 (65.5%)
#   Malignant: 241 (34.5%)

#load data
hdata=read.csv("wisconsin.data",header=TRUE,sep=",")

#rename columns
names(hdata)=c("Sample code number","Clump Thickness","Uniformity of Cell Size","Uniformity of Cell Shape","Marginal Adhesion","Single Epithelial Cell Size","Bare Nuclei","Bland Chromatin","Normal Nucleoli","Mitose","Class")

#replace classes 2 with 0 and 4 with 1
hdata[hdata$Class == 2, "Class"] <- 0
hdata[hdata$Class == 4, "Class"] <- 1

#convert class to factor
hdata$Class <- as.numeric(hdata$Class)

#divide dataset into training and testing subsets
library(caTools)
set.seed(123)
split = sample.split(hdata$Class,SplitRatio = 2/3) 
train_hdata = subset(hdata, split ==TRUE) 
test_hdata = subset(hdata, split == FALSE)

#train linear regression model
library(caTools)
regressor=lm(formula = `Class`~`Clump Thickness`, data=train_hdata)

#predics values using model
hd_predict=predict(regressor, newdata=test_hdata)

#round the fractional values
rounded=hd_predict
rounded=round(rounded)

#import libraries
library(e1071)
library(caret)
Loading required package: lattice
Loading required package: ggplot2

#convert rounded predictions to data frame
r1=as.data.frame(rounded)

#generate confusion matrix
confusionMatrix(as.factor(r1$rounded),as.factor(test_hdata$Class))
# Confusion Matrix and Statistics

# Reference
# Prediction   0   1
# 0 146  23
# 1   6  57

# Accuracy : 0.875           
# 95% CI : (0.8254, 0.9147)
# No Information Rate : 0.6552          
# P-Value [Acc > NIR] : 2.016e-14       

# Kappa : 0.7087          

# Mcnemar's Test P-Value : 0.002967        
                                          
#             Sensitivity : 0.9605          
#             Specificity : 0.7125          
#          Pos Pred Value : 0.8639          
#          Neg Pred Value : 0.9048          
#              Prevalence : 0.6552          
#          Detection Rate : 0.6293          
#    Detection Prevalence : 0.7284          
#       Balanced Accuracy : 0.8365          
                                          
#        'Positive' Class : 0 

#multiple linear regression
regressor = lm(formula = Class~`Clump Thickness`+`Uniformity of Cell Size`+`Uniformity of Cell Shape`, data=train_hdata)

#predict values from multiple linear regression
hd_age_predict=predict(regressor, newdata=test_hdata)

#round values
round_age=hd_age_predict
rage=round(round_age)

#convert to data frame
r1=as.data.frame(rage)

#confusion matrix
confusionMatrix(as.factor(r1$rage),as.factor(test_hdata$Class))
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 149  12
#          1   3  68
                                          
#                Accuracy : 0.9353          
#                  95% CI : (0.8956, 0.9634)
#     No Information Rate : 0.6552          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.853           
                                          
#  Mcnemar's Test P-Value : 0.03887         

# Sensitivity : 0.9803          
# Specificity : 0.8500          
# Pos Pred Value : 0.9255          
# Neg Pred Value : 0.9577          
# Prevalence : 0.6552          
# Detection Rate : 0.6422          
# Detection Prevalence : 0.6940          
# Balanced Accuracy : 0.9151          

# 'Positive' Class : 0

#define function normallize
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }

#factor to numeric
hdata$`Bare Nuclei` <- as.numeric(hdata$`Bare Nuclei`)

#normalize data
h1data<-hdata
h1data_n <- as.data.frame(lapply(h1data[1:10], normalize))

#split data into training and testing sets
traink_hdata=h1data_n[1:212,]
testk_hdata=h1data_n[213:303,]

#remove NaN, replacing with 0
traink_hdata$fbs = 0
testk_hdata$fbs = 0

#set training and testing labels
h1data_train_labels <- hdata[1:212, 11]
h1data_test_labels <- hdata[213:303, 11]

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
#                  0 |        38 |         4 |        42 | 
#                    |     0.905 |     0.095 |     0.462 | 
#                    |     0.905 |     0.082 |           | 
#                    |     0.418 |     0.044 |           | 
# -------------------|-----------|-----------|-----------|
#                  1 |         4 |        45 |        49 | 
#                    |     0.082 |     0.918 |     0.538 | 
#                    |     0.095 |     0.918 |           | 
#                    |     0.044 |     0.495 |           | 
# -------------------|-----------|-----------|-----------|
#       Column Total |        42 |        49 |        91 | 
#                    |     0.462 |     0.538 |           | 
# -------------------|-----------|-----------|-----------|

 
#simpler form of cross table
table(h1data_test_labels,h1data_test_pred)
#                   h1data_test_pred
# h1data_test_labels  0  1
#                  0 38  4
#                  1  4 45

#generate confusion matrix for knn predictions
confusionMatrix(as.factor(h1data_test_labels),as.factor(h1data_test_pred))
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 38  4
#          1  4 45
                                          
#                Accuracy : 0.9121          
#                  95% CI : (0.8341, 0.9613)
#     No Information Rate : 0.5385          
#     P-Value [Acc > NIR] : 9.531e-15       
                                          
#                   Kappa : 0.8231          
                                          
#  Mcnemar's Test P-Value : 1               
                                          
#             Sensitivity : 0.9048          
#             Specificity : 0.9184          
#          Pos Pred Value : 0.9048          
#          Neg Pred Value : 0.9184          
#              Prevalence : 0.4615          
#          Detection Rate : 0.4176          
#    Detection Prevalence : 0.4615          
#       Balanced Accuracy : 0.9116          
                                          
#        'Positive' Class : 0               
                                          

#kmeans clustering
#set random seed
set.seed(123)

#form 2 clusters of 4th and 5th column
clusters=kmeans(hdata[,4:5],2)

#describe clusters
str(clusters)
# List of 9
#  $ cluster     : int [1:698] 1 2 1 2 1 2 2 2 2 2 ...
#  $ centers     : num [1:2, 1:2] 7.27 1.66 6.54 1.38
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : chr [1:2] "1" "2"
#   .. ..$ : chr [1:2] "Uniformity of Cell Shape" "Marginal Adhesion"
#  $ totss       : num 11848
#  $ withinss    : num [1:2] 2636 1107
#  $ tot.withinss: num 3743
#  $ betweenss   : num 8105
#  $ size        : int [1:2] 193 505
#  $ iter        : int 1
#  $ ifault      : int 0
#  - attr(*, "class")= chr "kmeans"

#import library for visualization
library(ggplot2)

#create new data frame, adding column cluster to hdata
clust_d <- data.frame(hdata, cluster=factor(clusters$cluster))

#visualize the clusters
ggplot() + geom_point(data = clust_d, mapping = aes(x = df$`Uniformity of Cell Shape`,y = df$`Marginal Adhesion`,colour = cluster)) + scale_color_manual(values=c("black", "blue")) + geom_point(mapping = aes_string(x = clusters$centers[, "Uniformity of Cell Shape"], y = clusters$centers[, "Marginal Adhesion"]), color = "red", size = 4)
