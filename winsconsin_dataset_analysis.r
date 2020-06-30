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
df = read.csv(path, header=FALSE)

#display header of dataframe, first 10 rows
head(df, n=10)
#         V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11
# 1  1000025  5  1  1  1  2  1  3  1   1   2
# 2  1002945  5  4  4  5  7 10  3  2   1   2
# 3  1015425  3  1  1  1  2  2  3  1   1   2
# 4  1016277  6  8  8  1  3  4  3  7   1   2
# 5  1017023  4  1  1  3  2  1  3  1   1   2
# 6  1017122  8 10 10  8  7 10  9  7   1   4
# 7  1018099  1  1  1  1  2 10  3  1   1   2
# 8  1018561  2  1  2  1  2  1  3  1   1   2
# 9  1033078  2  1  1  1  2  1  1  1   5   2
# 10 1033078  4  2  1  1  2  1  2  1   1   2

#find various datatypes in the dataframe
str(df)
# 'data.frame':	699 obs. of  11 variables:
#  $ V1 : int  1000025 1002945 1015425 1016277 1017023 1017122 1018099 1018561 1033078 1033078 ...
#  $ V2 : int  5 5 3 6 4 8 1 2 2 4 ...
#  $ V3 : int  1 4 1 8 1 10 1 1 1 2 ...
#  $ V4 : int  1 4 1 8 1 10 1 2 1 1 ...
#  $ V5 : int  1 5 1 1 3 8 1 1 1 1 ...
#  $ V6 : int  2 7 2 3 2 7 2 2 2 2 ...
#  $ V7 : Factor w/ 11 levels "?","1","10","2",..: 2 3 4 6 2 3 3 2 2 2 ...
#  $ V8 : int  3 3 3 3 3 9 3 3 1 2 ...
#  $ V9 : int  1 2 1 7 1 7 1 1 1 1 ...
#  $ V10: int  1 1 1 1 1 1 1 1 5 1 ...
#  $ V11: int  2 2 2 2 2 4 2 2 2 2 ...

#count rows
nrow(df)
# [1] 699

#column count
ncol(df)
# [1] 11

#rename columns
names(df)=c("Sample code number","Clump Thickness","Uniformity of Cell Size","Uniformity of Cell Shape","Marginal Adhesion","Single Epithelial Cell Size","Bare Nuclei","Bland Chromatin","Normal Nucleoli","Mitose","Class")

#replace classes 2 with 0 and 4 with 1
df[df$Class == 2, "Class"] <- 0
df[df$Class == 4, "Class"] <- 1

#count of rows grouped by clump thickness
nrow(df[df$`Clump Thickness`==1,])
# [1] 145

#display frequency of elements of variable
table(df$`Clump Thickness`)

#   1   2   3   4   5   6   7   8   9  10 
# 145  50 108  80 130  34  23  46  14  69 

#column wisee split
#Subset 1 -  1 2 3 4 5 11
#Subset 2 -  1 6 7 8 9 10 11
df_col_subset_1 = df[,c(1, 2, 3, 4, 5, 11)]
df_col_subset_2 = df[,c(1, 6, 7, 8, 9, 10, 11)]

#classwise split (row wise)
df_row_subset_1 = df[df$Class == 0,]
df_row_subset_2 = df[df$Class == 1,]

#rbind - combine by rows
df_row_combined  = rbind(df_row_subset_1, df_row_subset_2)
#cbind - combine by columns
df_col_combined = cbind(df_col_subset_1, df_col_subset_2)

#merge - merge the dataframe
df_merged = merge(df_col_subset_1, df_col_subset_2, by=c("Sample code number", "Class"))
df_merged = merge(df_row_subset_1, df_row_subset_2)

#sort the dataframe
#ascending
sorted_df <- df[order(df$Class),]
#descending
sorted_df_inverse <- df[order(-df$Class),]

#transpose
transpose_df <- t(df)

#melting
library(reshape)
df_before_melt <- df
df_before_melt$`Bare Nuclei` <- as.integer(df_before_melt$`Bare Nuclei`)
molten_df = melt(df_before_melt, id<-c("Class"))

#casting
cast(molten_df, Class~variable, mean)
#   Class SampleCodeNumber Clump Thickness Uniformity of Cell Size Uniformity of Cell Shape
# 1     0          1107591        2.956332                1.325328                 1.443231
# 2     1          1003505        7.195021                6.572614                 6.560166
#   Marginal Adhesion Single Epithelial Cell Size Bare Nuclei Bland Chromatin Normal Nucleoli   Mitose
# 1          1.364629                    2.120087    2.371179        2.100437        1.290393 1.063319
# 2          5.547718                    5.298755    4.676349        5.979253        5.863071 2.589212

#summary
summary(df$`Clump Thickness`)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   4.000   4.418   6.000  10.000 

#outlier removal
outliers = boxplot(df$`Marginal Adhesion`, plot=FALSE)$out
df_no_outlier = df[-which(df$`Marginal Adhesion` %in% outliers),]

#outlier replacement
df_outlier_replaced = df
df_outlier_replaced[df_outlier_replaced$`Marginal Adhesion`==10,"Marginal Adhesion"] = mean(df[,"Marginal Adhesion"])
df_outlier_replaced[df_outlier_replaced$`Marginal Adhesion`==9,"Marginal Adhesion"] = mean(df[, "Marginal Adhesion"])