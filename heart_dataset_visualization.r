#load dataset
> hdata=read.csv(file="path", header=FALSE,sep=",")

#rename columns
> names(hdata)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")

#replace non zero values of num to 1
> hdata$num[hdata$num>0]<-1

#replace ? by NA
> levels(hdata$thal)[levels(hdata$thal)=="?"]<-NA
> levels(hdata$ca)[levels(hdata$ca)=="?"]<-NA

#replacing NA with max factor
> hdata$thal[is.na(hdata$thal)]<-3.0
> hdata$ca[is.na(hdata$ca)]<-0.0

# barplot fate(disease 1 or not 0)
#using barplot
barplot(table(hdata$num), main="Fate", col="blue")

#import library
> library(ggplot2)

#using ggplot2
> ggplot(hdata, aes(x=hdata$num, fill=hdata$num)) +
+  geom_bar() +
+  xlab("Heart Disease") +
+  ylab("Count") +
+  ggtitle("Analysis of Presence and Absence of Heart Disease") +
+  scale_fill_discrete(name = "Heart Disease", labels = c("Absence", "Presence"))

#mosaic plot of fate vs gender
> mosaicplot(hdata$sex ~ hdata$num,main="Fate by Gender",
+  shade=FALSE,color=TRUE,xlab="Gender", ylab="Heart disease")

#box plt of fate by age
> boxplot(hdata$age ~ hdata$num,main="Fate by
+ Age",shade=FALSE,color=TRUE,ylab="Age",xlab="Heart
+ disease")

#age data distribution
#using boxplt
> boxplot(hdata$age)

#using histogram
> hist(hdata$age)

#Group the different ages in three groups (young, middle, old)
> young <- hdata[which((hdata$age<45)), ]
> middle <- hdata[which((hdata$age>=45)&(hdata$age<55)), ]
> elderly <- hdata[which(hdata$age>55), ]
> groups <- data.frame(age_group = c("young","middle","elderly"), group_count = c(NROW(young),
+ NROW(middle), NROW(elderly)))

#ploting different age groups
> ggplot(groups, aes(x=groups$age_group, y=groups$group_count, fill=groups$age_group)) +
+  ggtitle("Age Analysis") +
+  xlab("Age Group") +
+  ylab("group Count") +
+  geom_bar(stat="identity") +
+  scale_fill_discrete(name = "Age Group", labels = c("Elderly", "Middle", "Young"))

# Adding the age groups to the dataset
> hdata <- cbind(hdata, groups = ifelse((hdata$age<45), 0, ifelse((hdata$age>=45)&(hdata$age<55), 1, 2)))
> hdata$groups <- as.factor(hdata$groups)

# Discrete vs Discrete vs Discrete variable: age_group ~ target ~ sex
> ggplot(hdata, aes(x= factor(hdata$groups), y=hdata$sex, colour=num)) +
+  geom_boxplot(stat = "boxplot",
+  position = "dodge2") +
+  geom_boxplot(outlier.shape = NA) +
+  geom_jitter(width = 0.2) +
+  xlab("Age Groups") +
+  ylab("Gender") +
+  ggtitle("Analysis of gender with different age group with presence or absense of heart disease")

# Bar plot for sex
> ggplot(hdata, aes(x= hdata$sex, fill=hdata$num)) +
+  geom_bar() +
+  xlab("Gender") +
+  ylab("Gender Count") +
+  ggtitle("Analysis of Gender") +
+  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# Bar plot for The chest pain experienced
> ggplot(hdata, aes(x=cp, fill=cp)) +
+  geom_bar() +
+  xlab("Chest Pain Type") +
+  ylab("Count") +
+  ggtitle("Analysis of Chest Pain Experienced") +
+  scale_fill_discrete(name = "Chest Pain Type", labels = c("Typical angina pain", "Atypical angina
+ pain", "Non-Anginal pain", "Asymptomatic pain"))

# Histogram for trestbps (resting blood pressure)
> ggplot(hdata, aes(x=trestbps)) +
+  geom_histogram() +
+  xlab("Resting blood pressure") +
+  ylab("Count") +
+  ggtitle("Analysis of blood pressure")

# removing the outliers
> hdata$trestbps = ifelse(hdata$trestbps > 180, NA, hdata$trestbps)
> hdata$trestbps = ifelse(is.na(hdata$trestbps), median(hdata$trestbps[which(!is.na(hdata$trestbps))]),
+ hdata$trestbps)

# After the removal of outliers
> ggplot(hdata, aes(x=trestbps)) +
+  geom_histogram() +
+  xlab("Resting blood pressure") +
+  ylab("Count") +
+  ggtitle("Analysis of blood pressure")

# Density graph for trestbps (resting blood pressure)
> ggplot(hdata, aes(x = trestbps, fill = num)) +
+  geom_density(alpha=0.5) +
+  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# Density plot for oldpeak ~ num
> ggplot(hdata, aes(x = oldpeak, fill = num)) +
+  geom_density(alpha=0.5) +
+  xlab("ST depression induced") +
+  ylab("Count") +
+  ggtitle("Analysis of ST depression induced and presence of heart disease") +
+  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# Bar plot for slope (slope of the peak exercise ST segment)
> hdata$slope <- as.factor(hdata$slope)
> ggplot(hdata, aes(x=hdata$slope, fill=hdata$slope)) +
+  geom_bar() +
+  xlab("Slope of ST segment") +
+  ylab("Count") +
+  ggtitle("Analysis of slope of the peak exercise ST segment") +
+  scale_fill_discrete(name = "Slope of ST segment", labels = c("Upsloping", "Flat", "Downsloping"))

# Plot for slope ~ target
> ggplot(hdata, aes(x= slope, fill=num)) +
+  geom_bar(position = 'dodge') +
+  xlab("slope of the peak exercise ST segment") +
+  ylab("count") +
+  ggtitle("Analysis of slope of the peak exercise ST segment with presence or absense of heart disease")+
+  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# Histogram for thalach (maximum heart rate achieved)
> ggplot(hdata, aes(x=thalach)) +
+  geom_histogram() +
+  xlab("Maximum heart rate achieved") +
+  ylab("Count") +
+  ggtitle("Analysis of maximum heart rate achieved")

# Density plot for thalach ~ target
> ggplot(hdata, aes(x = thalach, fill = num)) +
+  geom_density(alpha=0.5) +
+  xlab("Maximum Heart Rate Achieved") +
+  ylab("Count") +
+  ggtitle("Analysis of relation of heart rate with presence of heart disease") +
+  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

#convert data types to numeric for correlation
> hdata[,11] <- as.numeric(hdata[,11])

# Finding correlation among variables
> correlation=cor(hdata[,1:11],hdata[,1:11])

#import libraries
> library(reshape2)
> library(ggplot2)

#melt correlation
> melted_corr=melt(correlation)

#plot heatmap of corelated variables
> ggplot(melted_corr, aes(x = Var1, y=Var2, fill=as.numeric(value))) + geom_tile() +
+ geom_text(aes(Var1, Var2, label=as.numeric(value)),color='white',size=2)+
+ scale_fill_gradient(low='gray',high='black')+
+ theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))