#load data
> hdata=read.csv("wisconsin.data",header=TRUE,sep=",")

#rename columns
> names(hdata)=c("Sample code number","Clump Thickness","Uniformity of Cell Size","Uniformity of Cell Shape","Marginal Adhesion","Single Epithelial Cell Size","Bare Nuclei","Bland Chromatin","Normal Nucleoli","Mitose","Class")

#replace classes 2 with 0 and 4 with 1
> hdata[hdata$Class == 2, "Class"] <- 0
> hdata[hdata$Class == 4, "Class"] <- 1

# barplot fate(disease 1 or not 0)
#import library
> library(ggplot2)

#using ggplot2
> ggplot(hdata, aes(x=hdata$Class, fill=hdata$Class)) +
+  geom_bar() +
+  xlab("Disease") +
+  ylab("Count") +
+  ggtitle("Analysis of Presence and Absence of Disease") +
+  scale_fill_discrete(name = "Heart Disease", labels = c("Absence", "Presence"))

#mosaic plot of fate vs clump thickness
> mosaicplot(hdata$`Clump Thickness` ~ hdata$Class,main="Fate by Clump Thickness",
+  shade=FALSE,color=TRUE,xlab="Clump Thickness", ylab="Disease")

#box plt of fate by Uniformity of Cell Size
> boxplot(hdata$`Uniformity of Cell Size` ~ hdata$Class,main="Fate by
+ Cell Size",shade=FALSE,color=TRUE,ylab="Age",xlab="Disease")

#clump thickness data distribution
#using boxplt
> boxplot(hdata$`Clump Thickness`)

#using histogram
> hist(hdata$`Clump Thickness`)

# Bar plot for bland chromatin
> ggplot(hdata, aes(x= hdata$`Bland Chromatin`, fill=hdata$`Bland Chromatin`)) +
+  geom_bar() +
+  xlab("Bland Chromatin") +
+  ylab("Count") +
+  ggtitle("Analysis") +
+  scale_fill_discrete(name = "Disease", labels = c("No", "Yes"))

# removing the outliers
> hdata$`Marginal Adhesion` = ifelse(hdata$`Marginal Adhesion` > 5, NA, hdata$`Marginal Adhesion`)
> hdata$`Marginal Adhesion` = ifelse(is.na(hdata$`Marginal Adhesion`), mean(hdata$`Marginal Adhesion`[which(!is.na(hdata$`Marginal Adhesion`))]),
+ hdata$`Marginal Adhesion`)

# After the removal of outliers
> ggplot(hdata, aes(x=`Marginal Adhesion`)) +
+  geom_histogram() +
+  xlab("Resting adhesion") +
+  ylab("Count") +
+  ggtitle("Analysis of adhesion")

# Density graph for trestbps (resting blood pressure)
> ggplot(hdata, aes(x = `Marginal Adhesion`, fill = Class)) +
+  geom_density(alpha=0.5) +
+  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

#convert data types to numeric for correlation
> for (i in 1:11)
  + hdata[,i] <- as.numeric(hdata[,i])

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