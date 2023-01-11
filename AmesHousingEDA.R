################################################################################
#Final Assignment EDA
#Ambika Huluse Kapanaiah
#Student ID: u3227622
#Ames housing dataset sale Price prediction and linear regression modelling.
################################################################################
#preparing the environment for loading data
#clean all global variables
rm(list = ls())
#clean Console  as command (CTRL + L)
cat("\014")

#loading the required libraries
library(tidyverse)
library(stringr)
library(ggplot2)
library(GGally)
library(reshape2)
library(naniar)
library(car)
library(DMwR2)
library(modelr)
####################------Step1:Load and Read the data----######################

#setting the working directory for the current script to execute
for(i in 1:2){
  Wrkng_Dir <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))}
Wrkng_Dir
#reading train data
Ames_train <- read.csv("data/train.csv",na.strings=c(""," ","NA"))
#Ames_train <- as.tibble(Ames_train)
summary(Ames_train)
str(Ames_train)
#reading test data
Ames_test <- read.csv("data/test.csv",na.strings=c(""," ","NA"))
#Ames_train <- as.tibble(Ames_train)
summary(Ames_test)

######################------Step2:Prelimenary Analysis----######################

#First of all the target variable SalePrice descriptive statistics to be checked.

ggplot(Ames_train, aes(x = SalePrice, fill = ..count..)) + 
  geom_histogram(bins=15) +
  xlab("Sale Price in $") +
  ylab("Number of observations") +
  ggtitle("Histogram of sale prices")

outliers_SP <- boxplot.stats(Ames_train$SalePrice)$out
outliers_SP
#right skew,heavy-tailed distribution, which was confirmed by both 
#histogram and boxplot of the Sale Price, and there are 61 outliers
#Need investigation on whether transformation by Log  could help?
#without removing more outiliers.
#taking log of SalePrice to see the distribution

Ames_train$log_SalePrice <- log(Ames_train$SalePrice)
Ames_test$log_SalePrice <- log(Ames_test$SalePrice)

ggplot(Ames_train, aes(x = log_SalePrice, fill = ..count..)) + 
  geom_histogram(bins=15) +
  xlab("Sale Price in $") +
  ylab("Number of observations") +
  ggtitle("Histogram of sale prices")
#Now sale Price looks normally distributed.

#converting all the character columns to factor 
Ames_train_tidied <- data.frame(Ames_train) #taking a copy of original
Ames_train_tidied[sapply(
  Ames_train_tidied, is.character)] <- lapply(
    Ames_train_tidied[sapply(Ames_train_tidied, is.character)], 
    as.factor)
Ames_test[sapply(
  Ames_test, is.character)] <- lapply(
    Ames_test[sapply(Ames_test, is.character)], 
    as.factor)
#After careful study of Data set Total Ara of the property
#created by adding 2 columns TotalBsmtSF and  GrLivArea
Ames_train_tidied$Totl_Area <- Ames_train_tidied$TotalBsmtSF+
  Ames_train$GrLivArea

Ames_train_tidied %>%
  ggplot(aes(x=log_SalePrice,y=Totl_Area))+
  geom_point()
#There is strong linear positive relation between Totl_Area and log_SalePrice.
Ames_test$Totl_Area <- Ames_test$TotalBsmtSF+
  Ames_test$GrLivArea

####################Missing values and treatment###############################
#Checking for cols with missing values and verifying whether we would 
#need those columns or not in the future to consider for modelling.
#numeric columns with NA
ColswithNA <- Ames_train[,colSums(is.na(
  Ames_train[ , 1:ncol(Ames_train)])) > 0]
ColswithNA$SalePrice <- Ames_train$SalePrice
ColswithNA$log_SalePrice <- Ames_train$log_SalePrice


#Missing value ration in numeric features.
missing <- naniar::miss_var_summary(ColswithNA)

missing <- missing %>%
  filter(n_miss >0) %>%
  mutate(pct_miss = round(pct_miss,1))

#barplot of missing percentage foe each categoric feature

ggplot(missing,aes(reorder(variable, -pct_miss),y=pct_miss,fill=variable))+
  geom_bar(position="dodge", stat = "identity")+
  ggtitle("Seasonwise SalePrice from 2006-2010")
#there are 19 columns with NA.
#before deciding whether to drop, we need to check on correlation heatmap
#for numeric columns and visualise categorical variables relation with SalePrice.

numeric_cols <- unlist(lapply(ColswithNA, is.numeric)) 
colsNA_Numeric <-  ColswithNA[ , numeric_cols]
colnames(colsNA_Numeric)

#There are 3 numeric columns with NA. We should check the correlation between 
#them and log_SalePrice
ggcorr(colsNA_Numeric,label=T)

ggpairs(colsNA_Numeric, 
        lower=list(continuous=wrap("smooth", colour="blue")),
        diag=list(continuous=wrap("barDiag", fill="darkblue")))

#LotFrontage has less correlation , moderate to weak positive relationship 
#to SalePrice #and has got many outliers. 
#so removing the features with less than +/-0.5 correlation with SalePrice.
drop <- c("LotFrontage")
Ames_train_tidied = Ames_train_tidied[,!(names(Ames_train_tidied) %in% drop)]
Ames_test = Ames_test[,!(names(Ames_test) %in% drop)]
ncol(Ames_train_tidied)
#MasVnrArea and GarageYrBlt having good correlation with Saleprice

#imputing MasVnrArea NA's with median values
Ames_train_tidied <- Ames_train_tidied %>%
  mutate(MasVnrArea=ifelse(is.na(MasVnrArea),
                           median(MasVnrArea, na.rm=TRUE),
                           MasVnrArea))
Ames_test <- Ames_test %>%
  mutate(MasVnrArea=ifelse(is.na(MasVnrArea),
                           median(MasVnrArea, na.rm=TRUE),
                           MasVnrArea))
########Numeric Cols with NA missing values handled-completed

#categorical columns with NA
categoric_cols <- ColswithNA %>% select(!colnames(colsNA_Numeric))
ncol(categoric_cols)
categoric_cols$SalePrice <- Ames_train$SalePrice

#There are 16 categorical columns

#Further checking how much percentage of missing values we have here.
#which would help us to drop columns if the missing values are 50%
#Missing value ratio in categoric features.

missing <- naniar::miss_var_summary(categoric_cols)

missing <- missing %>%
  filter(n_miss >0) %>%
  mutate(pct_miss = round(pct_miss,1))

#barplot of missing percentage foe each categoric feature

ggplot(missing,aes(reorder(variable, -pct_miss),y=pct_miss,fill=variable))+
  geom_bar(position="dodge", stat = "identity")+
  ggtitle("Seasonwise SalePrice from 2006-2010")
##3 columns have more than 80% missing value

catcols_withNA_80P <- categoric_cols[, colSums(
  is.na(categoric_cols)) >= nrow(categoric_cols) * 0.8]
colnames(catcols_withNA_80P)

catcols_withNA_80P$SalePrice <- Ames_train$SalePrice

catcols_withNA_80P %>% 
  gather(variable, value,-SalePrice) %>%
  ggplot(aes(factor(value), SalePrice, fill = factor(value))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside")

#Alley,PoolQC,Fence , MiscFeature and  FireplaceQu having equal or more 
#than 50% NA values and are having outliers. And also no much variability when related with SalePrice.
#We can directly drop these variables..

#Dropping Alley,PoolQC,Fence and MiscFeature columns from the 
#original dataset copy(both train and test)
drop <- c("Alley","PoolQC","Fence","MiscFeature")

Ames_train_tidied = Ames_train_tidied[,!(names(Ames_train_tidied) %in% drop)]
Ames_test = Ames_test[,!(names(Ames_test) %in% drop)]

ncol(Ames_train_tidied)
ncol(Ames_test)

#collecting all Temporal(Time/Year) related features

Cols_Year <- Ames_train %>% 
  dplyr:: select(grep("Yr", names(Ames_train)),
                 grep("Year", names(Ames_train)),
                 grep("*Yr", names(Ames_train)))




#Kepping Yearbuilt in terms of Year. The rest of the columns can be converted
#to how much aged they are. For example taking the difference bewtwwn Yearsold
#and Yearbuilt gives the age of the property and similarly for GarageAge and 
#Age of property after renovation.

for(cols in colnames(Cols_Year)){
  if(cols!="YrSold"){
    Ames_train_tidied[,cols] <- Ames_train_tidied$YrSold-Ames_train_tidied[,cols]
    Ames_test[,cols] <- Ames_test$YrSold-Ames_test[,cols]}}

Cols_Year <- Ames_train_tidied %>% 
  dplyr:: select(grep("Yr", names(Ames_train_tidied)),
                 grep("Year", names(Ames_train_tidied)),
                 grep("*Yr", names(Ames_train_tidied)))

Cols_Year$SalePrice <- Ames_train_tidied$SalePrice
Cols_Year$log_SalePrice <- Ames_train_tidied$log_SalePrice


ggpairs(Cols_Year[-2], 
        lower=list(continuous=wrap("smooth", colour="green")),
        diag=list(continuous=wrap("barDiag", fill="darkgreen")))

#As the property ages the SalePrice is decreasing. Looks like
#all the 3 Yearwise columns are important in predicting SalePrice.
# All their correlation value is very high.

#checking the Sale Price versus YearSold
Yrsold_SP <- Cols_Year %>% group_by(YrSold) %>%
  summarise(Median_SP=median(SalePrice))

ggplot(Yrsold_SP, aes(x=YrSold, y=Median_SP)) +
  geom_line(color="red")+
  geom_point()+
  ggtitle("Yearwise sold properties Median Sale Price")+
  xlab("YearSold")+
  ylab("Median of Sale Price")
#looks like Sale Price decreasing from 2006 to 2010, which is also an 
#important column to consider.

#Using moSold, SeasonSold column is been created .
Winter <- c(1,2,12)
autumn <- c(9:11)
summmer <- c(6:8)
spring <- c(3:5)

Ames_train_tidied <- Ames_train_tidied %>% mutate(
  SeasonSold = ifelse(MoSold %in% autumn, "autumn",
                                ifelse(MoSold %in% Winter, "Winter",
                                       ifelse(MoSold %in% summmer, "summmer",
                                              "Spring"))))
Ames_test <- Ames_test %>% mutate(
  SeasonSold = ifelse(MoSold %in% autumn, "autumn",
                      ifelse(MoSold %in% Winter, "Winter",
                             ifelse(MoSold %in% summmer, "summmer",
                                    "Spring"))))

#dropping MoSold as I created SeasonSold
drop <- c("MoSold")
Ames_train_tidied = Ames_train_tidied[,!(names(Ames_train_tidied) %in% drop)]
Ames_test = Ames_test[,!(names(Ames_test) %in% drop)]
ncol(Ames_train_tidied)

Cols_Year$SeasonSold <- Ames_train_tidied$SeasonSold

Cols_Year <- Cols_Year %>% group_by(YrSold,SeasonSold) %>%
  mutate(median_SP=median(SalePrice))

ggplot(Cols_Year,aes(x=YrSold,y=SalePrice,fill=SeasonSold))+
  geom_bar(position="dodge", stat = "identity")+
  ggtitle("Seasonwise SalePrice from 2006-2010")
#SeasonSold has very big impact on the SalePrice especially in 
#the year 2007 wherein during Winter and Sumar had peak Sale Prices 
#for the properties sold followed by Spring and Autumn.
#Overall all the seasons with properties sold from 2006 to 2010 
#are having alost common trend except 2007.

colSums(is.na(Cols_Year))

#Imputing GarageYrBlt NA's with median values as it is skewed and have outliers.

Ames_train_tidied <- Ames_train_tidied %>%
  mutate(GarageYrBlt=ifelse(is.na(GarageYrBlt),
                           median(GarageYrBlt, na.rm=TRUE),
                           GarageYrBlt))

Ames_test <- Ames_test %>%
  mutate(GarageYrBlt=ifelse(is.na(GarageYrBlt),
                            median(GarageYrBlt, na.rm=TRUE),
                            GarageYrBlt))
##Missing Values handling for Numric columns completed


#Numeric columns Analysis of columns not having NA's
numeric_cols <- unlist(lapply(Ames_train_tidied, is.numeric)) 
Cols_allNumrc <-  Ames_train_tidied[ , numeric_cols]
ncol(Cols_allNumrc)
colnames(Cols_allNumrc)

#38 columns including Sale Price and ID are numeric and
#removing  Yearwise columns as we have already analysed them
#while removing the numeric cols with NA which we have already analysed.
drop <- c("GarageYrBlt","YrSold","YearBuilt" ,
          "YearRemodAdd","LotFrontage","MasVnrArea")
Cols_allNumrc = Cols_allNumrc[,!(names(Cols_allNumrc) %in% drop)]

#30 variables to Analyse. 

#Finding correlation with SalePrice
ggcorr(Cols_allNumrc,label = T)

#Features which are having low correlation that is less than
#+/-0.5 with SalePrice 
#These columns could be dropped as there is no much relation with
#the target variable.Removing Id column also

drop <- c("MiscVal","PoolArea","ScreenPorch",
          "EnclosedPorch","OpenPorchSF","WoodDeckSF",
          "KitchenAbvGr","BedroomAbvGr","HalfBath",
          "BsmtHalfBath","BsmtFullBath","LowQualFinSF",
          "X2ndFlrSF","BsmtUnfSF","BsmtFinSF2","BsmtUnfSF",
          "X3SsnPorch","OverallCond","LotArea","MSSubClass")

#Analysing them once before dropping######################
plot_drop <- Cols_allNumrc %>% 
  select(drop)
plot_drop$SalePrice <- Ames_train_tidied$SalePrice

ggpairs(plot_drop, 
        lower=list(continuous=wrap("smooth", colour="green")),
        diag=list(continuous=wrap("barDiag", fill="darkgreen")))

######

#Dropping the numeric cols which are having less tha +/-0.5 correlation
#with SalePrice after Analysis.

Ames_train_tidied = Ames_train_tidied[,!(names(Ames_train_tidied) %in% drop)]
ncol(Ames_train_tidied)
Ames_test = Ames_test[,!(names(Ames_test) %in% drop)]
ncol(Ames_train_tidied)
Cols_allNumrc = Cols_allNumrc[,!(names(Cols_allNumrc) %in% drop)]
colnames(Cols_allNumrc)

#segregating rest of the numeric columns 
#to continuous and discrete elements
colnames(Cols_allNumrc)
disc_cols <- c("OverallQual",
               "FullBath",
               "TotRmsAbvGrd","Fireplaces",
               "GarageCars")

cols_discrete <- data.frame(Cols_allNumrc) %>% select(disc_cols)
cols_discrete$SalePrice <- Ames_train$SalePrice

ncol(cols_discrete)
#There are 5 columns which are discrete.
ggpairs(cols_discrete, 
        lower=list(continuous=wrap("smooth", colour="blue")),
        diag=list(continuous=wrap("barDiag", fill="darkblue")))

cols_discrete %>% 
  gather(variable, value,-SalePrice) %>%
  ggplot(aes(factor(value), SalePrice, fill = factor(value))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside")

#As the overall quality,Full bath and Garage Cars
#having positive strong relation with SalePrice.
# the SalePrice is increasing positively with feature
# Fireplaces and TotRmsAbvGrd also have positive 
#strong relation and much variability with SalePrice


#continuous columns
cols_continuous <- Cols_allNumrc %>% select(!disc_cols)
ncol(cols_continuous)

ggpairs(cols_continuous, 
        lower=list(continuous=wrap("smooth", colour="yellow")),
        diag=list(continuous=wrap("barDiag", fill="orange")))

#All these continuous features has very
#strong positive linear relation with SalePrice.
####Numerical columns analysis complete##########################

#features in categorical has to be analysed.

cols_categoric <- Ames_train_tidied %>% select_if(negate(is.numeric))
colnames(cols_categoric)
cols_categoric$SalePrice  <- Ames_train_tidied$SalePrice


#Fireplace can be removed as it has highest percentage of missing value

#Have grouped into 4 groups just for visualisation purpose.
#1st category will have categories what 
#I feel are important from the buyers perspective
cols_cat1_imp1 <- c("Street","LotShape","LandContour",  
              "Utilities","Neighborhood", "BldgType","HouseStyle")

cat1_imp1  <- Ames_train %>% select(cols_cat1_imp1)
cat1_imp1$SalePrice <- Ames_train$SalePrice
cat1_imp1$YrSold <- Ames_train_tidied$YrSold

cat1_imp1 %>% 
  gather(variable, value,-SalePrice) %>%
  ggplot(aes(factor(value), SalePrice, fill = factor(value))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside")
#Utilities could be removed. because of
#no variability with SalePrice and many outliers

cols_cat1_imp2 <- c("RoofStyle","RoofMatl","MasVnrType","ExterQual",
                    "ExterCond","BsmtCond","KitchenQual","GarageQual",
                    "SaleType","SaleCondition")

cat1_imp2  <- Ames_train %>% select(cols_cat1_imp2)
cat1_imp2$SalePrice <- Ames_train$SalePrice
cat1_imp2$YrSold <- Ames_train_tidied$YrSold

cat1_imp2 %>% 
  gather(variable, value,-SalePrice) %>%
  ggplot(aes(factor(value), SalePrice, fill = factor(value))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside")

#4 columns can be removed as there is no much variability when 
#compared with SalePrice
#BsmtCond,ExterCond, RoofStyle,RoofMatl


drop <- c("BsmtCond","ExterCond", "RoofStyle","RoofMatl",
          "HouseStyle", "Street","Utilities" )
Ames_test = Ames_test[,!(names(Ames_test) %in% drop)]
Ames_train_tidied = Ames_train_tidied[,!(names(Ames_train_tidied) %in% drop)]


#miscellaneous category
cols_cat2_misc <- c("MSZoning","LotConfig","LandSlope","Condition1","Condition2",
                    "Heating","HeatingQC","CentralAir","Electrical","Functional",
                    "Foundation","TotRmsAbvGrd", "FireplaceQu")

cat2_misc <-  Ames_train_tidied %>% select(cols_cat2_misc)
cat2_misc$SalePrice <- Ames_train_tidied$SalePrice
cat2_misc$YrSold <- Ames_train_tidied$YrSold


cat2_misc %>% 
  gather(variable, value,-SalePrice) %>%
  ggplot(aes(factor(value), SalePrice, fill = factor(value))) +
  geom_violin() +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside")
#TotRmsAbvGrd has very good variability compared to other features here.
#dropping everything except TotRmsAbvGrd

drop <- c("MSZoning","LotConfig","LandSlope","Condition1","Condition2",
          "Heating","HeatingQC","CentralAir","Electrical","Functional",
          "Foundation", "FireplaceQu")
Ames_test = Ames_test[,!(names(Ames_test) %in% drop)]
Ames_train_tidied = Ames_train_tidied[,!(names(Ames_train_tidied) %in% drop)]


#External category features
cols_cat3_Ext <- c("PavedDrive", "GarageType","GarageFinish","GarageCond",
                   "BsmtQual","BsmtExposure","BsmtFinType1", 
                   "BsmtFinType2","Exterior1st","Exterior2nd")

cat3_Ext <-  Ames_train_tidied %>% select(cols_cat3_Ext)
cat3_Ext$SalePrice <- Ames_train_tidied$SalePrice
cat3_Ext$YrSold <- Ames_train_tidied$YrSold


cat3_Ext %>% 
  gather(variable, value,-SalePrice) %>%
  ggplot(aes(factor(value), SalePrice, fill = factor(value))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside")
#BsmtQual looks better related compared to all other features here.
#not removing BsmtQual 
drop <- c("PavedDrive", "GarageType","GarageFinish","GarageCond",
                   "BsmtExposure","BsmtFinType1", 
                   "BsmtFinType2","Exterior1st","Exterior2nd")
Ames_test = Ames_test[,!(names(Ames_test) %in% drop)]
Ames_train_tidied = Ames_train_tidied[,!(names(Ames_train_tidied) %in% drop)]


#imputing missing values of categorical variables with replacing NA to Missing.


replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "Missing", x)
  x <- as.factor(x)
}

Ames_train_tidied <- Ames_train_tidied %>%
  mutate_if(is.factor, replace_factor_na)

Ames_test <- Ames_test %>%
  mutate_if(is.factor, replace_factor_na)

colSums(is.na(Ames_train_tidied))
colSums(is.na(Ames_test))


for (cols in which(sapply(Ames_test, is.numeric))) {
  for (row in which(is.na(Ames_test[, cols]))) {
    Ames_test[row, cols] <- median(Ames_test[[cols]],
                                           na.rm = TRUE)}}

#Missing values imputation completed
colnames(Ames_train_tidied)
colnames(Ames_test)

##################-----Further Data Analysis------#############################

#########----Data Analysis problems and solution----###########################


#Problem 1: Identify which suburb/location had the biggest growth in 
#SalePrice by plotting and examining the sale prices cross different suburbs
#Has there been a trend on the type of house bought and had big
#hike in Sale price from 2006 to 2010

Ames_train_tidied %>%
  group_by(Neighborhood,YrSold) %>%
  summarise(med_SP=median(log_SalePrice)) %>% 
  ggplot(aes(YrSold,med_SP,color=factor(Neighborhood))) +
  geom_point() +
  geom_line() +
  facet_wrap(~Neighborhood)+
  ggtitle("SalePrice based on GarageQual and Garage Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Sale Price $")


#1. The suburb Somerst had increaing trend till 2009,
# in 2010 the price dropped
#2. Noridge also has increasing Saleprice as trend 
#but in 2009 it is decreased.
#3. NAmes has slight increasing trend every year.

##############################

#Problem 2:	Analyze a possible pattern of SalePrice vs 
#YrSold/MoSold, LotArea and/or some other variables which can 
#reasonably be included
#considering Totl_Area instead of LotArea, SeasonSold instead of MonthSold

Ames_train_tidied %>%
  ggplot(aes(x = SeasonSold , y = Totl_Area,col = log_SalePrice)) +
  geom_line()+
  geom_point() +
  facet_wrap(~ YrSold , scales = "free") +
  scale_color_continuous(name="Sale Price in $",
                         low="yellow",
                         high="darkblue")+
  theme_light()

# In 2006 during Summer the SalePrice have seen extreme peak
# looks like outliers, However, all the season has touched max Saleprice
# around 4000$ for the properties sold which are having bigger
# Totl_Area(Ground Living and Total Basement Area)

#In 2008 All the Seasons had very less Saleprices even for the properties
# which are big in Totl_Area, The average log_Saleprice was around 
# 4000$
# In 2009 all the seasons had peaks of almost 5000$ for the 
#properties with lasrger Totl_Area

#############

#Problem 3: Whether SaleCondition has any impact on the SalePrice,
#Explain with Data Analysis and give insights on whether this feature
#needs to be considered.

ggplot(Ames_train_tidied,aes(x=SalePrice))+
  geom_histogram(aes(fill=SaleCondition),
                     position=position_stack(), binwidth = 10000,
                 colour="yellow")+
  ggtitle("Median Saleprice based on SaleCondition")

ggplot(Ames_train_tidied,aes(SaleCondition,SalePrice,fill=SaleCondition))+
  geom_boxplot()+
  ggtitle("Median Saleprice across SaleCondition")

# Definitely there is a relation between SalePrice and SaleCondition
# Normal Sale Varied with lot of outliers. Family	Sale between family members
# had extreme decrease in SalePrices. Similary, with Abnorml	Abnormal Sale -
# my be due to trade,short sale, foreclosure

###########

#Problem 4 : Any change in SalePrice over the period 
#from 2006 to 2010 based on GarageQual

  ggplot(Ames_train_tidied, aes(x = YrSold,
                                y = log_SalePrice, fill = factor(GarageQual))) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Property sale Price in $ by GarageQuality") +
  ylab("Sale Price $") +
  xlab("Year Sold") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position ="right",
        axis.text.x = element_text(angle = 30,  
                                   vjust = 1, 
                                   size = 6, 
                                   hjust = 1)) +
  scale_fill_discrete(name="Garage Quality")

#Definitely there was a change of SalePrice over years. During 2006 to 2009
# The log_SalePrice for Average qualaty Garage properties sold around for 3500$
# in 2010 suddenly there is drop in SalePrice for even the Garage with 
# excellent quality, may be due to the age of the Garage.
  
#############

#Problem 5:Over the years.
  #How the SalePrice changed based on Neighborhood and BldgType .Explain

  Ames_train_tidied %>%
    group_by(Neighborhood, BldgType,YrSold) %>%
    summarise(med_SP= median(SalePrice)) %>%
    ggplot(aes(YrSold, med_SP, fill = BldgType)) +
    geom_bar(position="dodge", stat = "identity") +
    facet_wrap( ~ Neighborhood ) +
    ggtitle("SalePrice from 2006-2010 based on Neighborhood and BldgType") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Sale Price $")
  
#Buiding Type 1Fam has been sold in Neighborhoods
# BrkSide, ClearCr, Gilbert, IDOTRR, NoRidge,
#NWAmes,Timber, NRidgHt, StoneBr and so on
# In ClearCr Gilbert,Timber NoRidge location only buildingtype of 1Fam looks like trendy
# throughout from 2006 to 2010 The Sale price is almost Same in every year.
# In Suburb Blmngtn CollgCr StoneBr, Somerst the building type TwnhsE, 1Fam 
#looks like trendy. The saleprice is almost same for both building type from 2006-2010
  

################

#Problem 6:Was there any difference in SalePrice for the properties
  #sold between 2006 to 2010 based on LotShape/LandContour?
  #which is basically considered in Indian tradition, Just for curiosity 
  #have included this problem analysis.
  
  Ames_train_tidied %>% 
    group_by(LandContour, LotShape,YrSold) %>%
    summarise(med_SP= median(log_SalePrice))  %>%
    gather(variable, value,-med_SP,-YrSold) %>%
    ggplot(aes(variable, med_SP, fill = factor(value))) +
    geom_boxplot() +
    facet_wrap( ~ YrSold ) +
    ggtitle("SalePrice from 2006-2010 based on LandContour and LotShape") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("log_Sale Price $")

# Looks like LandContour and Landshape also has prominent
# dependency on SalePrice in 2006, 2009 and 2010

####################
  
#Problem 7:Check the seasonality of SalePrice based
#  on MasVnrType used.
  
  Ames_train_tidied %>% 
    group_by(YrSold,SeasonSold) %>% 
    gather(variable,value ,-MasVnrType,-SalePrice,-SeasonSold,-YrSold) %>%
    ggplot(aes(SeasonSold,SalePrice , fill=factor(MasVnrType))) +
    geom_boxplot() +
    facet_wrap( ~ YrSold ) +
    ggtitle("SalePrice from 2006-2010 based on seasonality and MasVnrType") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Sale Price $")

# Very Slight Variation of SalePrice when it comes to  MasVnrType
# Saleprice almost stays similar in all seasons with no much variation.

##############


#Problem 8:Did the Age of Garage,  property
#  made any difference in the Saleprice from 2006 2010.  

  Ames_train_tidied %>% 
    ggplot(aes(GarageYrBlt,SalePrice,color=factor(GarageQual))) +
    geom_point() +
    geom_smooth()+
    ggtitle("SalePrice based on GarageQual and Garage Age") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Sale Price $")
  
#Definitely there is strong relation which negative. As the Garage Ages
# The SalePrice is decreasing. Most of the Garages also are typical/Average quality
# from the beginning of the 2006 data.

###############################

#Problem 9:Do you think we could get good linear model with 
#just considering Overall quality as a stand alone parameter
#for Sale Price prediction? Give thoughts

ggplot(Ames_train_tidied,aes(x=OverallQual,y=log_SalePrice))+
  geom_point()

model <- lm(Ames_train_tidied$log_SalePrice ~ Ames_train_tidied$OverallQual,
            Ames_train_tidied)
summary(model)
#Even though RSE is 0.2303 the R-squared: 0.6678, which
#cannot be considered as a good linear model just by 
#considering the OverallQuality.

#The problem 10 will be solved once fitting the model is completed

####################----------Linear Modeling------------######################

#Removing Id column from both train and test data set
drop <- c("Id")
Ames_train_tidied = Ames_train_tidied[,!(names(Ames_train_tidied) %in% drop)]
Ames_test = Ames_test[,!(names(Ames_test) %in% drop)]
ncol(Ames_train_tidied)
ncol(Ames_test)

colnames(Ames_train_tidied)
colnames(Ames_test)


model_Ames_train_tidied<- lm(Ames_train_tidied$log_SalePrice ~ Ames_train_tidied$LotShape + 
                                 Ames_train_tidied$Neighborhood +
                                 Ames_train_tidied$BldgType +
                                 Ames_train_tidied$OverallQual +
                                 Ames_train_tidied$FullBath +
                                 Ames_train_tidied$KitchenQual +
                                 Ames_train_tidied$TotRmsAbvGrd + 
                                 Ames_train_tidied$Fireplaces+
                                 Ames_train_tidied$GarageCars+
                                 Ames_train_tidied$GarageArea +
                                 Ames_train_tidied$YrSold +
                                 Ames_train_tidied$Totl_Area +
                                 Ames_train_tidied$SeasonSold,Ames_train_tidied)
  summary(model_Ames_train_tidied) 
  

mode2_Ames_train_tidied<- lm(Ames_train_tidied$log_SalePrice ~ Ames_train_tidied$LotShape + 
                                 Ames_train_tidied$LandContour +
                                 Ames_train_tidied$Neighborhood +
                                 Ames_train_tidied$BldgType +
                                 Ames_train_tidied$OverallQual +
                                 Ames_train_tidied$YearBuilt +
                                 Ames_train_tidied$YearRemodAdd +
                                 Ames_train_tidied$MasVnrType +
                                 Ames_train_tidied$MasVnrArea +
                                 Ames_train_tidied$ExterQual +
                                 Ames_train_tidied$BsmtQual +
                                 Ames_train_tidied$BsmtFinSF1 +
                                 Ames_train_tidied$TotalBsmtSF +
                                 Ames_train_tidied$X1stFlrSF +
                                 Ames_train_tidied$GrLivArea +
                                 Ames_train_tidied$FullBath +
                                 Ames_train_tidied$KitchenQual +
                                 Ames_train_tidied$TotRmsAbvGrd + 
                                 Ames_train_tidied$Fireplaces+
                                 Ames_train_tidied$GarageYrBlt+
                                 Ames_train_tidied$GarageCars+
                                 Ames_train_tidied$GarageArea +
                                 Ames_train_tidied$YrSold +
                                 Ames_train_tidied$Totl_Area ,Ames_train_tidied)
  summary(mode2_Ames_train_tidied) 
  
  
mode3_Ames_train_tidied<- lm(Ames_train_tidied$log_SalePrice ~ Ames_train_tidied$OverallQual + 
                                 Ames_train_tidied$YearBuilt +
                                 Ames_train_tidied$Totl_Area +
                                 Ames_train_tidied$YearRemodAdd +
                                 Ames_train_tidied$MasVnrArea +
                                 Ames_train_tidied$BsmtFinSF1 +
                                 Ames_train_tidied$TotalBsmtSF +
                                 Ames_train_tidied$GrLivArea +
                                 Ames_train_tidied$X1stFlrSF +
                                 Ames_train_tidied$FullBath +
                                 Ames_train_tidied$Fireplaces +
                                 Ames_train_tidied$GarageYrBlt +
                                 Ames_train_tidied$YrSold +
                                 Ames_train_tidied$BldgType +
                                 Ames_train_tidied$OverallQual +
                                 Ames_train_tidied$MasVnrType +
                                 Ames_train_tidied$ExterQual + 
                                 Ames_train_tidied$KitchenQual+
                                 Ames_train_tidied$GarageQual+
                                 Ames_train_tidied$SaleType+
                                 Ames_train_tidied$SaleCondition,Ames_train_tidied)
  summary(mode3_Ames_train_tidied)
  
mode4_Ames_train_tidied<- lm(Ames_train_tidied$log_SalePrice ~ Ames_train_tidied$LotShape + 
                                 Ames_train_tidied$LandContour +
                                 Ames_train_tidied$Neighborhood +
                                 Ames_train_tidied$BldgType +
                                 Ames_train_tidied$OverallQual +
                                 Ames_train_tidied$YearBuilt +
                                 Ames_train_tidied$YearRemodAdd +
                                 Ames_train_tidied$MasVnrType +
                                 Ames_train_tidied$MasVnrArea +
                                 Ames_train_tidied$ExterQual +
                                 Ames_train_tidied$BsmtQual +
                                 Ames_train_tidied$BsmtFinSF1 +
                                 Ames_train_tidied$TotalBsmtSF +
                                 Ames_train_tidied$X1stFlrSF +
                                 Ames_train_tidied$GrLivArea +
                                 Ames_train_tidied$FullBath +
                                 Ames_train_tidied$KitchenQual +
                                 Ames_train_tidied$TotRmsAbvGrd + 
                                 Ames_train_tidied$Fireplaces+
                                 Ames_train_tidied$GarageYrBlt+
                                 Ames_train_tidied$GarageCars+
                                 Ames_train_tidied$GarageArea +
                                 Ames_train_tidied$GarageQual +
                                 Ames_train_tidied$YrSold +
                                 Ames_train_tidied$SaleType +
                                 Ames_train_tidied$SaleCondition +
                                 Ames_train_tidied$Totl_Area +
                                 Ames_train_tidied$SeasonSold,Ames_train_tidied)
  
  outlierTest(mode4_Ames_train_tidied)
  #There are few outliers.
  summary(mode4_Ames_train_tidied) 
  
  
  plot(mode4_Ames_train_tidied,1)
  plot(mode4_Ames_train_tidied,2)
  plot(mode4_Ames_train_tidied,3)
  plot(mode4_Ames_train_tidied,4)
  # mode4_Ames_train_tidied is the best fit with RMSE=0.1401
  # and R^2=0.884 compared to other fitted models

# Checking R-squared and RMSE for SalePrice and not log transformation here.
  model_Ames_trainSalePrice <- lm(Ames_train_tidied$SalePrice ~ Ames_train_tidied$LotShape + 
                                 Ames_train_tidied$LandContour +
                                 Ames_train_tidied$Neighborhood +
                                 Ames_train_tidied$BldgType +
                                 Ames_train_tidied$OverallQual +
                                 Ames_train_tidied$YearBuilt +
                                 Ames_train_tidied$YearRemodAdd +
                                 Ames_train_tidied$MasVnrType +
                                 Ames_train_tidied$MasVnrArea +
                                 Ames_train_tidied$ExterQual +
                                 Ames_train_tidied$BsmtQual +
                                 Ames_train_tidied$BsmtFinSF1 +
                                 Ames_train_tidied$TotalBsmtSF +
                                 Ames_train_tidied$X1stFlrSF +
                                 Ames_train_tidied$GrLivArea +
                                 Ames_train_tidied$FullBath +
                                 Ames_train_tidied$KitchenQual +
                                 Ames_train_tidied$TotRmsAbvGrd + 
                                 Ames_train_tidied$Fireplaces+
                                 Ames_train_tidied$GarageYrBlt+
                                 Ames_train_tidied$GarageCars+
                                 Ames_train_tidied$GarageArea +
                                 Ames_train_tidied$GarageQual +
                                 Ames_train_tidied$YrSold +
                                 Ames_train_tidied$SaleType +
                                 Ames_train_tidied$SaleCondition +
                                 Ames_train_tidied$Totl_Area +
                                 Ames_train_tidied$SeasonSold,Ames_train_tidied)
  
summary(model_Ames_trainSalePrice)
#Consideing SalePrice as Target R-squared value is 0.8654 and RMSE=30020
#As the RMSE is very high it is better to use log_SalePrice rather SalePrice.
  
#Finalised the 4th model with log_SalePrice as the response variable.

# for test data set-> lm
model_Ames_test<- lm(Ames_test$log_SalePrice ~ Ames_test$LotShape + 
                         Ames_test$LandContour +
                         Ames_test$Neighborhood +
                         Ames_test$BldgType +
                         Ames_test$OverallQual +
                         Ames_test$YearBuilt +
                         Ames_test$YearRemodAdd +
                         Ames_test$MasVnrType +
                         Ames_test$MasVnrArea +
                         Ames_test$ExterQual +
                         Ames_test$BsmtQual +
                         Ames_test$BsmtFinSF1 +
                         Ames_test$TotalBsmtSF +
                         Ames_test$X1stFlrSF +
                         Ames_test$GrLivArea +
                         Ames_test$FullBath +
                         Ames_test$KitchenQual +
                         Ames_test$TotRmsAbvGrd + 
                         Ames_test$Fireplaces+
                         Ames_test$GarageYrBlt+
                         Ames_test$GarageCars+
                         Ames_test$GarageArea +
                         Ames_test$GarageQual +
                         Ames_test$YrSold +
                         Ames_test$SaleType +
                         Ames_test$SaleCondition +
                         Ames_test$Totl_Area +
                         Ames_test$SeasonSold,Ames_test)
  summary(model_Ames_test) 
  
  plot(model_Ames_test,1)
  plot(model_Ames_test,2)
  plot(model_Ames_test,3)
  plot(model_Ames_test,4)
  
  rmse(mode4_Ames_train_tidied,Ames_train_tidied)
  rmse(model_Ames_test,Ames_test)
  
#predicting test dataset and plotting residuals
  
  lmpred <- predict(model_Ames_test, newdata = Ames_test)
  lmdata <- Ames_test %>% mutate(y = log_SalePrice) %>% 
    mutate(ybar = lmpred) %>% mutate(diff = abs(y - ybar))
  
  badlmdata <- lmdata %>% filter(diff > 1.5) %>% arrange(desc(diff))
  
  lmresiduals <- ggplot(lmdata, aes(x = y, y = y-ybar), col = diff) +
    geom_point(aes(x = y, y = y-ybar, color = diff)) +
    geom_point(data = badlmdata, colour="red") +
    scale_color_gradient(name = "|y - ybar|", limits = c(0, 1.5)) +
    geom_abline(slope = 1, intercept = 0) +
    xlab("y") +
    ylab("y-ybar") +
    ggtitle("Linear model residuals") +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title.x = element_text(size = 12, hjust = 0.5),
          axis.title.y = element_text(size = 12, hjust = 0.5),
          legend.position = "right",
          legend.spacing.y = unit(0.5, 'cm'))
  
  lmresiduals 
  sqrt(mean((Ames_test$log_SalePrice - lmpred) ^ 2))
  

###########
  
#Problem 10: Use predictions from your final model to compare suburbs
#which have shown varying growth. Or, to identify which suburbs
#have been growing the most over the last few years.
  
#According the fitted final model  the formula for prediction of SalePrice
#is as follows.
  
#SalePrice = (-3.479e-02)*NeighborhoodBlueste+1.318e+01
#SalePrice = ( -9.795e-02)*NeighborhoodBrDale+1.318e+01
#SalePrice = (1.237e-01)*NeighborhoodVeenker+1.318e+01
#and so on
  
#so Based on the Neighbourhood value in test dataset
#SalePrice will be predicted.

##############################-------End------#################################