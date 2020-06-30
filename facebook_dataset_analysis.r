#dataset description
#Feature		Type ofinformation	SourceData		type
#Posted		Identification		Facebook		Date/time
#Permanent link	Identification		Facebook		Text
#Post ID
#Post message	Content			Facebook		Text
#ype		Categorization		Facebook		Factor: {Link, Photo, Status, Video }
#Category	Categorization		Facebook page managers	Factor: {action, product, inspiration }
#Paid		Categorization		Facebook		Factor: {yes, no }
#Page total likesPerformance		Facebook		Numeric
#Lifetime post total reach
#Lifetime post total impressions
#Lifetime engaged users
#Lifetime post consumers
#Lifetime post consumptions
#Lifetime post impressions by people who have liked your page
#Lifetime post reach by people who like your pageLifetime people who have liked your page and engaged with your post
#Comments	Performance		Facebook		Numeric
#Likes
#Shares
#Total interactions	Performance	Computed		Numeric

#load data
df = read.csv(path, header=TRUE, sep=';')

#display header of dataframe, first 10 rows
head(df, n=10)
# Page.total.likes   Type Category Post.Month Post.Weekday Post.Hour Paid Lifetime.Post.Total.Reach
# 1            139441  Photo        2         12            4         3    0                      2752
# 2            139441 Status        2         12            3        10    0                     10460
# 3            139441  Photo        3         12            3         3    0                      2413
# 4            139441  Photo        2         12            2        10    1                     50128
# 5            139441  Photo        2         12            2         3    0                      7244
# 6            139441 Status        2         12            1         9    0                     10472
# 7            139441  Photo        3         12            1         3    1                     11692
# 8            139441  Photo        3         12            7         9    1                     13720
# 9            139441 Status        2         12            7         3    0                     11844
# 10           139441  Photo        3         12            6        10    0                      4694
# Lifetime.Post.Total.Impressions Lifetime.Engaged.Users Lifetime.Post.Consumers
# 1                             5091                    178                     109
# 2                            19057                   1457                    1361
# 3                             4373                    177                     113
# 4                            87991                   2211                     790
# 5                            13594                    671                     410
# 6                            20849                   1191                    1073
# 7                            19479                    481                     265
# 8                            24137                    537                     232
# 9                            22538                   1530                    1407
# 10                            8668                    280                     183
# Lifetime.Post.Consumptions Lifetime.Post.Impressions.by.people.who.have.liked.your.Page
# 1                         159                                                         3078
# 2                        1674                                                        11710
# 3                         154                                                         2812
# 4                        1119                                                        61027
# 5                         580                                                         6228
# 6                        1389                                                        16034
# 7                         364                                                        15432
# 8                         305                                                        19728
# 9                        1692                                                        15220
# 10                        250                                                         4309
# Lifetime.Post.reach.by.people.who.like.your.Page
# 1                                              1640
# 2                                              6112
# 3                                              1503
# 4                                             32048
# 5                                              3200
# 6                                              7852
# 7                                              9328
# 8                                             11056
# 9                                              7912
# 10                                             2324
# Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post comment like share
# 1                                                                  119       4   79    17
# 2                                                                 1108       5  130    29
# 3                                                                  132       0   66    14
# 4                                                                 1386      58 1572   147
# 5                                                                  396      19  325    49
# 6                                                                 1016       1  152    33
# 7                                                                  379       3  249    27
# 8                                                                  422       0  325    14
# 9                                                                 1250       0  161    31
# 10                                                                 199       3  113    26
# Total.Interactions
# 1                 100
# 2                 164
# 3                  80
# 4                1777
# 5                 393
# 6                 186
# 7                 279
# 8                 339
# 9                 192
# 10                142

#find various datatypes in the dataframe
str(df)
# 'data.frame':	500 obs. of  19 variables:
# $ Page.total.likes                                                   : int  139441 139441 139441 139441 139441 139441 139441 139441 139441 139441 ...
# $ Type                                                               : Factor w/ 4 levels "Link","Photo",..: 2 3 2 2 2 3 2 2 3 2 ...
# $ Category                                                           : int  2 2 3 2 2 2 3 3 2 3 ...
# $ Post.Month                                                         : int  12 12 12 12 12 12 12 12 12 12 ...
# $ Post.Weekday                                                       : int  4 3 3 2 2 1 1 7 7 6 ...
# $ Post.Hour                                                          : int  3 10 3 10 3 9 3 9 3 10 ...
# $ Paid                                                               : int  0 0 0 1 0 0 1 1 0 0 ...
# $ Lifetime.Post.Total.Reach                                          : int  2752 10460 2413 50128 7244 10472 11692 13720 11844 4694 ...
# $ Lifetime.Post.Total.Impressions                                    : int  5091 19057 4373 87991 13594 20849 19479 24137 22538 8668 ...
# $ Lifetime.Engaged.Users                                             : int  178 1457 177 2211 671 1191 481 537 1530 280 ...
# $ Lifetime.Post.Consumers                                            : int  109 1361 113 790 410 1073 265 232 1407 183 ...
# $ Lifetime.Post.Consumptions                                         : int  159 1674 154 1119 580 1389 364 305 1692 250 ...
# $ Lifetime.Post.Impressions.by.people.who.have.liked.your.Page       : int  3078 11710 2812 61027 6228 16034 15432 19728 15220 4309 ...
# $ Lifetime.Post.reach.by.people.who.like.your.Page                   : int  1640 6112 1503 32048 3200 7852 9328 11056 7912 2324 ...
# $ Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post: int  119 1108 132 1386 396 1016 379 422 1250 199 ...
# $ comment                                                            : int  4 5 0 58 19 1 3 0 0 3 ...
# $ like                                                               : int  79 130 66 1572 325 152 249 325 161 113 ...
# $ share                                                              : int  17 29 14 147 49 33 27 14 31 26 ...
# $ Total.Interactions                                                 : int  100 164 80 1777 393 186 279 339 192 142 ...

#count rows
nrow(df)
# [1] 500

#column count
ncol(df)
# [1] 19

#count of rows grouped by category of post
nrow(df[df$Category==1,])
# [1] 215

#display frequency of elements of variable
table(df$Category)

# 1   2   3 
# 215 130 155 

#column wisee split
#Subset 1 -  1 2 3 4 5 11
#Subset 2 -  1 6 7 8 9 10 11
df_col_subset_1 = df[,c(1, 2, 3, 4, 5, 12, 13, 14)]
df_col_subset_2 = df[,c(1, 6, 7, 8, 9, 10, 11)]

#classwise split (row wise)
df_row_subset_1 = df[df$Category == 0,]
df_row_subset_2 = df[df$Category == 1,]
df_row_subset_3 = df[df$Category == 2,]

#rbind - combine by rows
df_row_combined  = rbind(df_row_subset_1, df_row_subset_2)
#cbind - combine by columns
df_col_combined = cbind(df_col_subset_1, df_col_subset_2)

#merge - merge the dataframe
df_merged = merge(df_col_subset_1, df_col_subset_2, by=c("Page.total.likes"))
df_merged = merge(df_row_subset_1, df_row_subset_2)

#sort the dataframe
#ascending
sorted_df <- df[order(df$Category),]
#descending
sorted_df_inverse <- df[order(-df$Category),]

#transpose
transpose_df <- t(df)

#drop missing values
df <- na.omit(df)

dim(df)
# [1] 495  19

#converting data_type
df$Type <- as.numeric(df$Type)
df$Page.total.likes <- as.numeric(df$Page.total.likes)
df$Category <- as.numeric(df$Category)
df$Post.Month <- as.numeric(df$Post.Month)
df$Post.Weekday <- as.numeric(df$Post.Weekday)
df$Post.Hour <- as.numeric(df$Post.Hour)
df$Paid <- as.numeric(df$Paid)
df$Lifetime.Post.Total.Reach <- as.numeric(df$Lifetime.Post.Total.Reach)
df$Lifetime.Post.Total.Impressions <- as.numeric(df$Lifetime.Post.Total.Impressions)
df$Lifetime.Engaged.Users <- as.numeric(df$Lifetime.Engaged.Users)
df$Lifetime.Post.Consumers <- as.numeric(df$Lifetime.Post.Consumers)
df$Lifetime.Post.Consumptions <- as.numeric(df$Lifetime.Post.Consumptions)
df$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page <- as.numeric(df$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page)
df$Lifetime.Post.reach.by.people.who.like.your.Page <- as.numeric(df$Lifetime.Post.reach.by.people.who.like.your.Page)
df$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post <- as.numeric(df$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post)
df$comment <- as.numeric(df$comment)
df$like <- as.numeric(df$like)
df$share <- as.numeric(df$share)
df$Total.Interactions <- as.numeric(df$Total.Interactions)

#melting
library(reshape)
molten_df = melt(df, id<-c("Category"))

#casting
cast(molten_df, Category~variable, mean)
# Category Page.total.likes     Type Post.Month Post.Weekday Post.Hour      Paid
# 1        1         122760.3 1.985782   7.113744     4.232227  8.597156 0.3033175
# 2        2         129020.8 2.286822   8.062016     4.108527  6.914729 0.2480620
# 3        3         118868.8 2.019355   6.051613     4.019355  7.593548 0.2774194
# Lifetime.Post.Total.Reach Lifetime.Post.Total.Impressions Lifetime.Engaged.Users
# 1                 18651.322                        41130.45               863.3412
# 2                  9909.667                        17632.98              1142.8140
# 3                 11162.155                        24684.05               833.5032
# Lifetime.Post.Consumers Lifetime.Post.Consumptions
# 1                778.2038                  1726.7204
# 2                995.9767                  1459.5426
# 3                679.8387                   988.4645
# Lifetime.Post.Impressions.by.people.who.have.liked.your.Page
# 1                                                     21589.96
# 2                                                     11464.64
# 3                                                     15091.22
# Lifetime.Post.reach.by.people.who.like.your.Page
# 1                                         7665.929
# 2                                         6092.465
# 3                                         5703.432
# Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post   comment     like    share
# 1                                                            533.8246  5.914692 126.5355 18.72512
# 2                                                            845.1085 11.186047 219.6977 34.40310
# 3                                                            531.2323  6.774194 217.0129 32.94839
# Total.Interactions
# 1           151.1754
# 2           265.2868
# 3           256.7355

#summary
summary(df$Page.total.likes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 81370  112324  129600  123173  136393  139441 

#outlier removal
outliers = boxplot(df$Page.total.likes, plot=FALSE)$out
df_no_outlier = df[-which(df$Page.total.likes %in% outliers),]

#outlier replacement
df_outlier_replaced = df
df_outlier_replaced[df_outlier_replaced$`Marginal Adhesion`==10,"Page.total.likes"] = mean(df[,"Page.total.likes"])
df_outlier_replaced[df_outlier_replaced$`Marginal Adhesion`==9,"Page.total.likes"] = mean(df[, "Page.total.likes"])
