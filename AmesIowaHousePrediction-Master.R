#########################    HOUSING PRICE PREDICTION MODEL   ################################
#	TEAM: Gayathri Movva and other team members from OR 568 course
##############################################################################################

# Packages / LIBRARIES INCLUDED
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyr")
install.packages("viridis")
install.packages("ggplot2")
install.packages("car")
install.packages("caret")
install.packages("mice")
install.packages("foreign") # R version 4 required
install.packages("VIM", dependencies = TRUE)
install.packages("naniar")
install.packages('fastDummies')
install.packages("ggrepel")
install.packages("tseries")
install.packages("e1071")
install.packages("elasticnet")
install.packages("pls")
install.packages("corrplot")
install.packages("randomForest")
install.packages("gbm")
install.packages("earth")
install.packages("kernlab")
install.packages("nnet")
install.packages("rpart")
install.packages("partykit")
install.packages("rattle")
install.packages("Ckmeans.1d.dp")
install.packages("xgboost")

library(readr) #Load csv data files
library(tidyr)
library(dplyr)
library(plyr)
library(viridis)
library(ggplot2) #Plotting
library(caret)
library(car)
library(MASS)
library(mice)
library(foreign)
library(VIM)
library(naniar)
library(fastDummies)
library(tseries)
library(gridExtra)
library(scales)
library(ggrepel)
library(pls)
library(e1071) # misc library including skewness function
library(elasticnet)
library(randomForest) # Regression with Random Forest Trees
library(corrplot) #Run correlation plots
library(gbm)
library(earth)
library(kernlab)
library(nnet)
library(rpart)
library(partykit)
library(rattle)
library(Ckmeans.1d.dp)
library(xgboost)


##############################################################################################
#  			 Import and Prepare Data for Visualizations
##############################################################################################
start.time <- Sys.time()

setwd("C:/Masters/OR568/Final Project/")
HouseFullDF <- read.csv("train.csv", stringsAsFactors = FALSE)
HouseTestFullDF <- read.csv("test.csv", stringsAsFactors = FALSE)
str(HouseFullDF)
str(HouseTestFullDF)
# Convert to charater datatype
HouseFullDF$MSSubClass <- as.character(HouseFullDF$MSSubClass)
HouseFullDF$OverallQual <- as.character(HouseFullDF$OverallQual)
HouseFullDF$OverallCond <- as.character(HouseFullDF$OverallCond)
HouseFullDF$MoSold <- as.character(HouseFullDF$MoSold)

HouseTestFullDF$MSSubClass <- as.character(HouseTestFullDF$MSSubClass)
HouseTestFullDF$OverallQual <- as.character(HouseTestFullDF$OverallQual)
HouseTestFullDF$OverallCond <- as.character(HouseTestFullDF$OverallCond)
HouseTestFullDF$MoSold <- as.character(HouseTestFullDF$MoSold)

# Set NA(missing) to correct non-NA values for HouseFullDF dataset
NAcol <- which(colSums(is.na(HouseFullDF)) > 0)
sort(colSums(sapply(HouseFullDF[NAcol], is.na)), decreasing = TRUE)

NAcol <- which(colSums(is.na(HouseTestFullDF)) > 0)
sort(colSums(sapply(HouseTestFullDF[NAcol], is.na)), decreasing = TRUE)

HouseFullDF$BsmtQual[is.na(HouseFullDF$BsmtQual)] <- 'NoB'
HouseFullDF$BsmtCond[is.na(HouseFullDF$BsmtCond)] <- 'NoB'
HouseFullDF$BsmtExposure[is.na(HouseFullDF$BsmtExposure)] <- 'NoB'
HouseFullDF$BsmtFinType1[is.na(HouseFullDF$BsmtFinType1)] <- 'NoB'
HouseFullDF$BsmtFinType2[is.na(HouseFullDF$BsmtFinType2)] <- 'NoB'
HouseFullDF$GarageFinish[is.na(HouseFullDF$GarageFinish)] <- 'NoG'
HouseFullDF$GarageType[is.na(HouseFullDF$GarageType)] <- 'NoG'
HouseFullDF$GarageQual[is.na(HouseFullDF$GarageQual)] <- 'NoG'
HouseFullDF$GarageCond[is.na(HouseFullDF$GarageCond)] <- 'NoG'
HouseFullDF$PoolQC[is.na(HouseFullDF$PoolQC)] <- 'NoP'
HouseFullDF$Alley[is.na(HouseFullDF$Alley)] <- 'NoA'
HouseFullDF$Fence[is.na(HouseFullDF$Fence)] <- 'NoF'
HouseFullDF$FireplaceQu[is.na(HouseFullDF$FireplaceQu)] <- 'NoFp'
HouseFullDF$MiscFeature[is.na(HouseFullDF$MiscFeature)] <- 'NoFtr'

HouseTestFullDF$BsmtQual[is.na(HouseTestFullDF$BsmtQual)] <- 'NoB'
HouseTestFullDF$BsmtCond[is.na(HouseTestFullDF$BsmtCond)] <- 'NoB'
HouseTestFullDF$BsmtExposure[is.na(HouseTestFullDF$BsmtExposure)] <- 'NoB'
HouseTestFullDF$BsmtFinType1[is.na(HouseTestFullDF$BsmtFinType1)] <- 'NoB'
HouseTestFullDF$BsmtFinType2[is.na(HouseTestFullDF$BsmtFinType2)] <- 'NoB'
HouseTestFullDF$GarageFinish[is.na(HouseTestFullDF$GarageFinish)] <- 'NoG'
HouseTestFullDF$GarageType[is.na(HouseTestFullDF$GarageType)] <- 'NoG'
HouseTestFullDF$GarageQual[is.na(HouseTestFullDF$GarageQual)] <- 'NoG'
HouseTestFullDF$GarageCond[is.na(HouseTestFullDF$GarageCond)] <- 'NoG'
HouseTestFullDF$PoolQC[is.na(HouseTestFullDF$PoolQC)] <- 'NoP'
HouseTestFullDF$Alley[is.na(HouseTestFullDF$Alley)] <- 'NoA'
HouseTestFullDF$Fence[is.na(HouseTestFullDF$Fence)] <- 'NoF'
HouseTestFullDF$FireplaceQu[is.na(HouseTestFullDF$FireplaceQu)] <- 'NoFp'
HouseTestFullDF$MiscFeature[is.na(HouseTestFullDF$MiscFeature)] <- 'NoFtr'

#unique(HouseTestFullDF$MasVnrType)

# Convert char columns in datasets to factor
col_names <- names(dplyr::select_if(HouseFullDF, is.character))
HouseFullDF[col_names] <- lapply(HouseFullDF[col_names] , factor)

col_names <- names(dplyr::select_if(HouseTestFullDF, is.character))
HouseTestFullDF[col_names] <- lapply(HouseTestFullDF[col_names] , factor)

# Create Ordinal levels for datasets
# Ordinal Level for HouseFullDF
#HouseFullDF$LotShape <- factor(HouseFullDF$LotShape, order = TRUE,
#                                   levels =c('IR3', 'IR2', 'IR1', 'Reg'),
#                                   labels =c('Irregular', 'Moderately irregular', 'Slightly Irregular',
#                                             'Regular	'))
HouseFullDF$LandSlope <- factor(HouseFullDF$LandSlope, order = TRUE,
                                   levels =c('Sev', 'Mod', 'Gtl'))
HouseFullDF$ExterQual <- factor(HouseFullDF$ExterQual, order = TRUE,
                                    levels =c('Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseFullDF$ExterCond <- factor(HouseFullDF$ExterCond, order = TRUE,
                                    levels =c('Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseFullDF$BsmtQual <- factor(HouseFullDF$BsmtQual, order = TRUE,
                                    levels =c('NoB', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseFullDF$BsmtCond <- factor(HouseFullDF$BsmtCond, order = TRUE,
                                    levels =c('NoB', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseFullDF$BsmtExposure <- factor(HouseFullDF$BsmtExposure, order = TRUE,
                                   levels =c('NoB', 'No', 'Mn', 'Av', 'Gd'))
HouseFullDF$BsmtFinType1 <- factor(HouseFullDF$BsmtFinType1, order = TRUE,
                                       levels =c('NoB', 'Unf', 'LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ'))
HouseFullDF$BsmtFinType2 <- factor(HouseFullDF$BsmtFinType2, order = TRUE,
                                       levels =c('NoB', 'Unf', 'LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ'))
HouseFullDF$KitchenQual <- factor(HouseFullDF$KitchenQual, order = TRUE,
                                    levels =c('Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseFullDF$GarageFinish <- factor(HouseFullDF$GarageFinish, order = TRUE,
                                       levels =c('NoG', 'Unf', 'RFn', 'Fin'))
HouseFullDF$GarageCond <- factor(HouseFullDF$GarageCond, order = TRUE,
                             levels =c('NoG', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseFullDF$GarageQual <- factor(HouseFullDF$GarageQual, order = TRUE,
                                 levels =c('NoG', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseFullDF$PavedDrive <- factor(HouseFullDF$PavedDrive, order = TRUE,
                                       levels =c('N', 'P', 'Y'))
HouseFullDF$PoolQC <- factor(HouseFullDF$PoolQC, order = TRUE,
                                  levels =c('NoP', 'Fa', 'TA', 'Gd', 'Ex'))
HouseFullDF$Alley <- factor(HouseFullDF$Alley, order = TRUE,
                                 levels =c('NoA', 'Grvl', 'Pave'))
HouseFullDF$Street <- factor(HouseFullDF$Street, order = TRUE,
                            levels =c('Grvl', 'Pave'))
HouseFullDF$Fence <- factor(HouseFullDF$Fence, order = TRUE,
                                  levels =c('NoF', 'MnWw', 'GdWo', 'MnPrv', 'GdPrv'))
HouseFullDF$FireplaceQu <- factor(HouseFullDF$FireplaceQu, order = TRUE,
                             levels =c('NoFp', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseFullDF$OverallQual <- factor(HouseFullDF$OverallQual, order = TRUE,
                                  levels =c('1', '2', '3', '4', '5', '6','7', '8', '9', '10'))
HouseFullDF$OverallCond <- factor(HouseFullDF$OverallCond, order = TRUE,
                                  levels =c('1', '2', '3', '4', '5', '6','7', '8', '9', '10'))
HouseFullDF$HeatingQC <- factor(HouseFullDF$HeatingQC, order = TRUE,
                                  levels =c('Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseFullDF$Functional <- factor(HouseFullDF$Functional, order = TRUE,
                                levels =c('Sal', 'Sev', 'Maj2', 'Maj1', 'Mod', 'Min2', 'Min1', 'Typ'))

# Ordinal Level for HouseFullTestDF
HouseTestFullDF$LandSlope <- factor(HouseTestFullDF$LandSlope, order = TRUE,
                                   levels =c('Sev', 'Mod', 'Gtl'))
HouseTestFullDF$ExterQual <- factor(HouseTestFullDF$ExterQual, order = TRUE,
                                    levels =c('Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseTestFullDF$ExterCond <- factor(HouseTestFullDF$ExterCond, order = TRUE,
                                    levels =c('Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseTestFullDF$BsmtQual <- factor(HouseTestFullDF$BsmtQual, order = TRUE,
                                    levels =c('NoB', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseTestFullDF$BsmtCond <- factor(HouseTestFullDF$BsmtCond, order = TRUE,
                                    levels =c('NoB', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseTestFullDF$BsmtExposure <- factor(HouseTestFullDF$BsmtExposure, order = TRUE,
                                   levels =c('NoB', 'No', 'Mn', 'Av', 'Gd'))
HouseTestFullDF$BsmtFinType1 <- factor(HouseTestFullDF$BsmtFinType1, order = TRUE,
                                       levels =c('NoB', 'Unf', 'LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ'))
HouseTestFullDF$BsmtFinType2 <- factor(HouseTestFullDF$BsmtFinType2, order = TRUE,
                                       levels =c('NoB', 'Unf', 'LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ'))
HouseTestFullDF$KitchenQual <- factor(HouseTestFullDF$KitchenQual, order = TRUE,
                                    levels =c('Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseTestFullDF$GarageFinish <- factor(HouseTestFullDF$GarageFinish, order = TRUE,
                                       levels =c('NoG', 'Unf', 'RFn', 'Fin'))
HouseTestFullDF$GarageCond <- factor(HouseTestFullDF$GarageCond, order = TRUE,
                             levels =c('NoG', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseTestFullDF$GarageQual <- factor(HouseTestFullDF$GarageQual, order = TRUE,
                                 levels =c('NoG', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseTestFullDF$PavedDrive <- factor(HouseTestFullDF$PavedDrive, order = TRUE,
                                       levels =c('N', 'P', 'Y'))
HouseTestFullDF$PoolQC <- factor(HouseTestFullDF$PoolQC, order = TRUE,
                                  levels =c('NoP', 'Fa', 'TA', 'Gd', 'Ex'))
HouseTestFullDF$Alley <- factor(HouseTestFullDF$Alley, order = TRUE,
                                 levels =c('NoA', 'Grvl', 'Pave'))
HouseTestFullDF$Street <- factor(HouseTestFullDF$Street, order = TRUE,
                            levels =c('Grvl', 'Pave'))
HouseTestFullDF$Fence <- factor(HouseTestFullDF$Fence, order = TRUE,
                                  levels =c('NoF', 'MnWw', 'GdWo', 'MnPrv', 'GdPrv'))
HouseTestFullDF$FireplaceQu <- factor(HouseTestFullDF$FireplaceQu, order = TRUE,
                             levels =c('NoFp', 'Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseTestFullDF$OverallQual <- factor(HouseTestFullDF$OverallQual, order = TRUE,
                                  levels =c('1', '2', '3', '4', '5', '6','7', '8', '9', '10'))
HouseTestFullDF$OverallCond <- factor(HouseTestFullDF$OverallCond, order = TRUE,
                                  levels =c('1', '2', '3', '4', '5', '6','7', '8', '9', '10'))
HouseTestFullDF$HeatingQC <- factor(HouseTestFullDF$HeatingQC, order = TRUE,
                                  levels =c('Po', 'Fa', 'TA', 'Gd', 'Ex'))
HouseTestFullDF$Functional <- factor(HouseTestFullDF$Functional, order = TRUE,
                                levels =c('Sal', 'Sev', 'Maj2', 'Maj1', 'Mod', 'Min2', 'Min1', 'Typ'))

#revalue for better readability
HouseFullDF$MSSubClass <- revalue(HouseFullDF$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', 
                              '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', 
                              '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', 
                              '80'='split/multi level', '85'='split foyer', 
                              '90'='duplex all style/age', '120'='1 story PUD 1946+', 
                              '150'='1,5 story PUD all', '160'='2 story PUD 1946+', 
                              '180'='PUD multilevel', '190'='2 family conversion'))

#revalue for better readability
HouseTestFullDF$MSSubClass <- revalue(HouseTestFullDF$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', 
                              '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', 
                              '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', 
                              '80'='split/multi level', '85'='split foyer', 
                              '90'='duplex all style/age', '120'='1 story PUD 1946+', 
                              '150'='1,5 story PUD all', '160'='2 story PUD 1946+', 
                              '180'='PUD multilevel', '190'='2 family conversion'))


#Check Dim / Structure / missing data
dim(HouseFullDF)
dim(HouseTestFullDF)

str(HouseFullDF)
str(HouseTestFullDF)

summary(HouseFullDF)
summary(HouseTestFullDF)

sum(is.na(HouseFullDF))
sum(is.na(HouseTestFullDF))

NAcol <- which(colSums(is.na(HouseFullDF)) > 0)
sort(colSums(sapply(HouseFullDF[NAcol], is.na)), decreasing = TRUE)

NAcol <- which(colSums(is.na(HouseTestFullDF)) > 0)
sort(colSums(sapply(HouseTestFullDF[NAcol], is.na)), decreasing = TRUE)


gg_miss_var(HouseFullDF)
gg_miss_var(HouseTestFullDF)
res<-summary(aggr(HouseFullDF, sortVar=TRUE))$combinations

gg_miss_var(HouseTestFullDF)
gg_miss_var(HouseTestFullDF)
res<-summary(aggr(HouseTestFullDF, sortVar=TRUE))$combinations

pct_miss(HouseFullDF)
pct_miss(HouseTestFullDF)

##############################################################################################
#			Additional Data Preprocessing
##############################################################################################

# Feature Engineering for HouseFullDF
HouseFullDF$TotBathrooms <- HouseFullDF$FullBath + (HouseFullDF$HalfBath*0.5) +
                            HouseFullDF$BsmtFullBath +
                           (HouseFullDF$BsmtHalfBath*0.5) # Add bathrooms for new predictor

HouseFullDF$Remod <- ifelse(HouseFullDF$YearBuilt==HouseFullDF$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling

HouseFullDF$HouseAge <- as.numeric(HouseFullDF$YrSold)- HouseFullDF$YearRemodAdd # House Age

HouseFullDF$IsNew <- ifelse(HouseFullDF$YrSold==HouseFullDF$YearBuilt, 1, 0) #New House=1, old =0

HouseFullDF$TotalSqFeet <- HouseFullDF$GrLivArea + HouseFullDF$TotalBsmtSF
cor(HouseFullDF$SalePrice, HouseFullDF$TotalSqFeet, use= "pairwise.complete.obs")

#Convert the new variables to factor
str(HouseFullDF)
HouseFullDF$TotBathrooms <-  as.factor(HouseFullDF$TotBathrooms)
HouseFullDF$Remod <-  as.factor(HouseFullDF$Remod)
HouseFullDF$IsNew <-  as.factor(HouseFullDF$IsNew)
HouseFullDF$YrSold <- as.factor(HouseFullDF$YrSold)
HouseFullDF$MoSold <- as.factor(HouseFullDF$MoSold)
str(HouseFullDF)

# Feature Engineering for HouseTestFullDF
HouseTestFullDF$TotBathrooms <- HouseTestFullDF$FullBath + (HouseTestFullDF$HalfBath*0.5) +
                            HouseTestFullDF$BsmtFullBath +
                           (HouseTestFullDF$BsmtHalfBath*0.5) # Add bathrooms for new predictor

HouseTestFullDF$Remod <- ifelse(HouseTestFullDF$YearBuilt==HouseTestFullDF$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling

HouseTestFullDF$HouseAge <- as.numeric(HouseTestFullDF$YrSold)- HouseTestFullDF$YearRemodAdd # House Age

HouseTestFullDF$IsNew <- ifelse(HouseTestFullDF$YrSold==HouseTestFullDF$YearBuilt, 1, 0) #New House=1, old =0

HouseTestFullDF$TotalSqFeet <- HouseTestFullDF$GrLivArea + HouseTestFullDF$TotalBsmtSF

#Convert the new variables to factor
str(HouseTestFullDF)
HouseTestFullDF$TotBathrooms <-  as.factor(HouseTestFullDF$TotBathrooms)
HouseTestFullDF$Remod <-  as.factor(HouseTestFullDF$Remod)
HouseTestFullDF$IsNew <-  as.factor(HouseTestFullDF$IsNew)
HouseTestFullDF$YrSold <- as.factor(HouseTestFullDF$YrSold)
HouseTestFullDF$MoSold <- as.factor(HouseTestFullDF$MoSold)
str(HouseTestFullDF)

#Check for near-zero variance predictors in HouseFullDF dataset.These predictors have very few 
#unique values and should be removed.
nZeroCol <- nearZeroVar(HouseFullDF[,-81]) 
colnames(HouseFullDF)[nZeroCol]
HouseFullDF_Red <- HouseFullDF[,-nZeroCol]
str(HouseFullDF_Red)

# near-zero for HouseTestFullDF
nZeroCol5 <- nearZeroVar(HouseTestFullDF) 
colnames(HouseTestFullDF)[nZeroCol5]
HouseTestFullDF_Red <- HouseTestFullDF[,-nZeroCol5]
str(HouseTestFullDF_Red)

# Add few predictors back to the dataset after NearZero
HouseFullDF_Red$HouseAge <- HouseFullDF$HouseAge
HouseFullDF_Red$LandContour <- HouseFullDF$LandContour
HouseFullDF_Red$BsmtFinSF2 <- HouseFullDF$BsmtFinSF2
HouseTestFullDF_Red$IsNew <- HouseTestFullDF$IsNew

str(HouseFullDF_Red)
str(HouseTestFullDF_Red)

NAcol <- which(colSums(is.na(HouseTestFullDF_Red)) > 0)
sort(colSums(sapply(HouseTestFullDF_Red[NAcol], is.na)), decreasing = TRUE)

sum(is.na(HouseFullDF_Red))
gg_miss_var(HouseFullDF_Red)
gg_miss_var(HouseTestFullDF_Red)

res<-summary(aggr(HouseFullDF_Red, sortVar=TRUE))$combinations
res<-summary(aggr(HouseTestFullDF_Red, sortVar=TRUE))$combinations

# Impute missing values using Mice for HouseFullDF dataset
#original <- HouseFullDF

init = mice(HouseFullDF_Red, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("LotFrontage")]="cart"
meth[c("GarageYrBlt")]="cart"
meth[c("MasVnrType")]="polyreg" 
meth[c("MasVnrArea")]="cart"
meth[c("Electrical")]="polyreg"

set.seed(200)
imputed_DF = mice(HouseFullDF_Red, method=meth, predictorMatrix=predM, m=1)
imputed_DF <- complete(imputed_DF)
sapply(imputed_DF, function(x) sum(is.na(x)))

res<-summary(aggr(imputed_DF, sortVar=TRUE))$combinations

# MasVnrType
predicted_Mas <- imputed_DF$MasVnrType[is.na(HouseFullDF_Red$MasVnrType)] 
table(predicted_Mas)

str(HouseFullDF_Red)
str(imputed_DF)

# Impute missing values using Mice for HouseTestFullDF_Red dataset
#original1 <- HouseTestFullDF_Red

init = mice(HouseTestFullDF_Red, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("LotFrontage")]="cart"
meth[c("GarageYrBlt")]="cart"
meth[c("MasVnrType")]="polyreg" 
meth[c("MasVnrArea")]="cart"
meth[c("MSZoning")]="polyreg"

meth[c("BsmtFullBath")]="cart"
meth[c("BsmtHalfBath")]="cart"
meth[c("TotBathrooms")]="cart"
meth[c("Exterior1st")]=""
meth[c("Exterior2nd")]=""
meth[c("BsmtFinSF1")]=""
meth[c("BsmtFinSF2")]=""
meth[c("BsmtUnfSF")]=""
meth[c("TotalBsmtSF")]=""
meth[c("KitchenQual")]=""
meth[c("GarageCars")]=""
meth[c("GarageArea")]=""
meth[c("SaleType")]=""
meth[c("TotalSqFeet")]=""

set.seed(200)
imputed_DF1 = mice(HouseTestFullDF_Red, method=meth, predictorMatrix=predM, m=1)
imputed_DF1 <- complete(imputed_DF1)
sapply(imputed_DF1, function(x) sum(is.na(x)))

# MasVnrType imputed values
predicted_Mas <- imputed_DF1$MasVnrType[is.na(HouseTestFullDF_Red$MasVnrType)] 
table(predicted_Mas)

res<-summary(aggr(imputed_DF1, sortVar=TRUE))$combinations

#Check Remaining missing values
NAcol <- which(colSums(is.na(imputed_DF1)) > 0)
sort(colSums(sapply(imputed_DF1[NAcol], is.na)), decreasing = TRUE)

imputed_DF1 <- na.omit(imputed_DF1) # Remove 16 remaining missing values

res<-summary(aggr(imputed_DF1, sortVar=TRUE))$combinations

# Reorder columns in imputed_DF dataset for readability
ReOrdered_DF <- imputed_DF[, c( "MSSubClass","MSZoning","LotShape","LotConfig","Neighborhood",
                      "Condition1","BldgType","HouseStyle","OverallQual","OverallCond","RoofStyle",
                      "Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond",
                      "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                      "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                      "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                      "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour",
                      "LotFrontage","LotArea","YearBuilt","YearRemodAdd","MasVnrArea",
                      "BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GrLivArea",
                      "BsmtFullBath","BsmtHalfBath","FullBath","X2ndFlrSF","HalfBath",
                      "BedroomAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars", 
                      "GarageArea","WoodDeckSF","OpenPorchSF","HouseAge","TotalSqFeet","SalePrice")]
str(ReOrdered_DF)

# Reorder columns in imputed_DF1 dataset for readability
ReOrdered_DF1 <- imputed_DF1[, c( "MSSubClass","MSZoning","LotShape","LotConfig","Neighborhood",
                      "Condition1","BldgType","HouseStyle","OverallQual","OverallCond","RoofStyle",
                      "Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond",
                      "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                      "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                      "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                      "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour",
                      "LotFrontage","LotArea","YearBuilt","YearRemodAdd","MasVnrArea",
                      "BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GrLivArea",
                      "BsmtFullBath","BsmtHalfBath","FullBath","X2ndFlrSF","HalfBath",
                      "BedroomAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars", 
                      "GarageArea","WoodDeckSF","OpenPorchSF","HouseAge","TotalSqFeet")]
str(ReOrdered_DF1)

# Reorder columns in imputed_DF dataset for readability for Visualizations
ReOrdered_DF2 <- imputed_DF[, c( "MSSubClass","MSZoning","LotShape","LotConfig","Neighborhood",
                                "Condition1","BldgType","HouseStyle","OverallQual","OverallCond","RoofStyle",
                                "Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond",
                                "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                                "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                                "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                                "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour",
                                "LotFrontage","LotArea","YearBuilt","YearRemodAdd","MasVnrArea",
                                "BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","X1stFlrSF",
                                "BsmtFullBath","BsmtHalfBath","FullBath","X2ndFlrSF","HalfBath",
                                "BedroomAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars", 
                                "GarageArea","WoodDeckSF","OpenPorchSF","HouseAge","TotalSqFeet","SalePrice")]
str(ReOrdered_DF2)

##############################################################################################
#			Data Exploration and Visualization
##############################################################################################

# ************* Applying Random Forest to get Important Predictors ************
set.seed(100)
ReOrdered_RF <- randomForest(x=ReOrdered_DF2[1:1460,-65],y=ReOrdered_DF$SalePrice[1:1460],ntree = 100,importance=TRUE)
varimp_RF <- importance(ReOrdered_RF)
varimpDF <- data.frame(Variables = row.names(varimp_RF), MSE = varimp_RF[,1])
varimpDF <- varimpDF[order(varimpDF$MSE, decreasing = TRUE),]
varimpDF <- varimpDF[-1,]

ggplot(varimpDF[1:25,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + 
  geom_bar(stat = 'identity') + 
  ggtitle("Important Predictors with Random Forest") +
  labs(x = 'Variables', y= '% increase MSE') + 
  coord_flip() + 
  theme(legend.position="none")

# ************* Data Visualizations based on Important Predictors ************
#Histograms
# # Histogram for Sales Price
# Sales Price
H0 <- ggplot(ReOrdered_DF, aes(x=SalePrice))+
  geom_histogram(color="darkblue", fill="lightblue",binwidth = 25000)+
  ggtitle("Frequency Plot for SalePrice") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
H0
##TotalSqFeet  
H1 <- ggplot(ReOrdered_DF, aes(x=TotalSqFeet))+ 
  geom_histogram(color="darkblue", fill="lightblue")+
  ggtitle("Frequency Plot") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
#YearBuilt
H2 <- ggplot(ReOrdered_DF, aes(x=YearBuilt))+ 
  geom_histogram(color="darkblue", fill="lightblue")+
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
#HouseAge
H3 <- ggplot(ReOrdered_DF, aes(x=HouseAge))+ 
  geom_histogram(color="darkblue", fill="lightblue")+
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
#GrLivArea
H4 <- ggplot(ReOrdered_DF, aes(x=GrLivArea))+ 
  geom_histogram(color="darkblue", fill="lightblue")+
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
#YearRemodAdd
H5 <- ggplot(ReOrdered_DF, aes(x=YearRemodAdd))+ 
  geom_histogram(color="darkblue", fill="lightblue")+
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
#BsmtUnfSF
H6 <- ggplot(ReOrdered_DF, aes(x=BsmtUnfSF))+ 
  geom_histogram(color="darkblue", fill="lightblue")+
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
#GarageArea
H7 <- ggplot(ReOrdered_DF, aes(x=GarageArea))+ 
  geom_histogram(color="darkblue", fill="lightblue")+
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
#X2ndFlrSF
H8 <- ggplot(ReOrdered_DF, aes(x=X2ndFlrSF))+ 
  geom_histogram(color="darkblue", fill="lightblue")+
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
#OpenPorchSF
H9 <- ggplot(ReOrdered_DF, aes(x=OpenPorchSF))+ 
  geom_histogram(color="darkblue", fill="lightblue")+
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))

# #Combining Scatter plots in one frame
grid.arrange(H1,H2,H3,H4,H5,H6,H7,H8,H9,ncol = 3, nrow = 3)

#Scatterplots

SP1 <- ggplot(ReOrdered_DF, 
              aes(x = TotalSqFeet , y = SalePrice)) +
  geom_point(aes(color = ReOrdered_DF$SalePrice), size = 2) +
  xlab("TotalSqFeet") + ylab("SalesPrice")+
  ggtitle("SalesPrice - TotalSqFeet")+
  scale_y_reverse(limits = 100000,700000)+
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"),labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "right",legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

SP2 <- ggplot(ReOrdered_DF, 
              aes(x = YearBuilt , y = SalePrice)) +
  geom_point(aes(color = ReOrdered_DF$SalePrice), size = 2) +
  xlab("YearBuilt") + ylab("SalesPrice")+
  ggtitle("SalesPrice - YearBuilt")+
  scale_y_reverse(limits = 100000,700000)+
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"),labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "right",legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

SP3 <- ggplot(ReOrdered_DF, 
              aes(x = HouseAge , y = SalePrice)) +
  geom_point(aes(color = ReOrdered_DF$SalePrice), size = 2) +
  xlab("HouseAge") + ylab("SalesPrice")+
  ggtitle("SalesPrice - HouseAge")+
  scale_y_reverse(limits = 100000,700000)+
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"),labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "right",legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

SP4 <- ggplot(ReOrdered_DF, 
              aes(x = GrLivArea , y = SalePrice)) +
  geom_point(aes(color = ReOrdered_DF$SalePrice), size = 2) +
  xlab("GrLivArea") + ylab("SalesPrice")+
  ggtitle("SalesPrice - GrLivArea")+
  scale_y_reverse(limits = 100000,700000)+
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"),labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "right",legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

SP5 <- ggplot(ReOrdered_DF, 
              aes(x = YearRemodAdd , y = SalePrice)) +
  geom_point(aes(color = ReOrdered_DF$SalePrice), size = 2) +
  xlab("YearRemodAdd") + ylab("SalesPrice")+
  ggtitle("SalesPrice - YearRemodAdd")+
  scale_y_reverse(limits = 100000,700000)+
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"),labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "right",legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

SP6 <- ggplot(ReOrdered_DF, 
              aes(x = BsmtUnfSF , y = SalePrice)) +
  geom_point(aes(color = ReOrdered_DF$SalePrice), size = 2) +
  xlab("BsmtUnfSF") + ylab("SalesPrice")+
  ggtitle("SalesPrice - BsmtUnfSF")+
  scale_y_reverse(limits = 100000,700000)+
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"),labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "right",legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

SP7 <- ggplot(ReOrdered_DF, 
              aes(x = GarageArea , y = SalePrice)) +
  geom_point(aes(color = ReOrdered_DF$SalePrice), size = 2) +
  xlab("GarageArea") + ylab("SalesPrice")+
  ggtitle("SalesPrice - GarageArea")+
  scale_y_reverse(limits = 100000,700000)+
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"),labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "right",legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

SP8 <- ggplot(ReOrdered_DF, 
              aes(x = X2ndFlrSF , y = SalePrice)) +
  geom_point(aes(color = ReOrdered_DF$SalePrice), size = 2) +
  xlab("X2ndFlrSF") + ylab("SalesPrice")+
  ggtitle("SalesPrice - X2ndFlrSF")+
  scale_y_reverse(limits = 100000,700000)+
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"),labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "right",legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

SP9 <- ggplot(ReOrdered_DF, 
              aes(x = OpenPorchSF , y = SalePrice)) +
  geom_point(aes(color = ReOrdered_DF$SalePrice), size = 2) +
  xlab("OpenPorchSF") + ylab("SalesPrice")+
  ggtitle("SalesPrice - OpenPorchSF")+
  scale_y_reverse(limits = 100000,700000)+
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"),labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "right",legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

#Combining Scatter plots in one frame
grid.arrange(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9,ncol = 3, nrow = 3)

# #Boxplots
# 
#Neighborhood vs SalesPrice
BP1 <- ggplot(ReOrdered_DF, 
              aes(x = Neighborhood , y = SalePrice,fill=Neighborhood))+
  geom_boxplot(alpha=0.3)+
  ggtitle("BoxPlot for Neighborhood") +
  xlab("Neighborhood") + ylab("SalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))
#Fireplace Quality vs SalesPrice
BP2 <- ggplot(ReOrdered_DF, 
              aes(x = FireplaceQu , y = SalePrice,fill=FireplaceQu))+
  geom_boxplot(alpha=0.3)+
  ggtitle("BoxPlot") +
  xlab("Fireplace Quality") + ylab("SalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))
#BsmtFinType1 vs SalesPrice
BP3 <- ggplot(ReOrdered_DF, 
              aes(x = BsmtFinType1 , y = SalePrice,fill=BsmtFinType1))+
  geom_boxplot(alpha=0.3)+
  xlab("BsmtFinType1") + ylab("SalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))
#ExterQual vs SalesPrice
BP4 <- ggplot(ReOrdered_DF, 
              aes(x = ExterQual , y = SalePrice,fill=ExterQual))+
  geom_boxplot(alpha=0.3)+
  xlab("ExterQual") + ylab("SalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))
#Fireplaces vs SalesPrice
BP5 <- ggplot(ReOrdered_DF, 
              aes(x = Fireplaces , y = SalePrice,fill=Fireplaces))+
  geom_boxplot(alpha=0.3)+
  xlab("Fireplaces") + ylab("SalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))
#BsmtQual vs SalesPrice
BP6 <- ggplot(ReOrdered_DF, 
              aes(x = BsmtQual , y = SalePrice,fill=BsmtQual))+
  geom_boxplot(alpha=0.3)+
  xlab("BsmtQual") + ylab("SalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))
#GarageType vs SalesPrice
BP7 <- ggplot(ReOrdered_DF, 
              aes(x = GarageType , y = SalePrice,fill=GarageType ))+
  geom_boxplot(alpha=0.3)+
  xlab("GarageType ") + ylab("SalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))
#KitchenQual vs SalesPrice
BP8 <- ggplot(ReOrdered_DF, 
              aes(x = KitchenQual , y = SalePrice,fill=KitchenQual ))+
  geom_boxplot(alpha=0.3)+
  xlab("KitchenQual") + ylab("SalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))
#KBsmtFullBath vs SalesPrice
BP9 <- ggplot(ReOrdered_DF, 
              aes(x = BsmtFullBath , y = SalePrice,fill=BsmtFullBath ))+
  geom_boxplot(alpha=0.3)+
  xlab("BsmtFullBath") + ylab("SalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

#TotalBathRooms vs SalesPrice
BP10 <- ggplot(ReOrdered_DF, 
              aes(x = TotBathrooms , y = SalePrice,fill=TotBathrooms ))+
  geom_boxplot(alpha=0.3)+
  xlab("TotalBathRooms") + ylab("SalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

#Combining Box Plots in one frame
grid.arrange(BP2,BP3,BP4,BP5,BP6,BP7,BP8,BP9,BP10,ncol = 3, nrow = 3)

# Neighbourhood Boxplot
BP1

####### Analyz the new Features added ####################
#Check the Correlation between SalePrice and TotBathrooms
tb1 <- ggplot(data=ReOrdered_DF, aes(x=TotBathrooms, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  ggtitle("Correlation of TotBathrooms with SalePrice") +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

tb2 <- ggplot(data=ReOrdered_DF, aes(x=TotBathrooms)) +
  geom_histogram(stat='count')
grid.arrange(tb1, tb2)

#Check if Houses remodeled are worth less
ggplot(ReOrdered_DF, aes(x=Remod, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  ggtitle("Remod Count") +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice

# Check Cor between SalePrice and HouseAge
ggplot(data=ReOrdered_DF, aes(x=HouseAge, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  ggtitle("Correlation of HouseAge with SalePrice") +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

cor(ReOrdered_DF$SalePrice, ReOrdered_DF$HouseAge)

#Check if New Houses remodeled are worth more
ggplot(ReOrdered_DF, aes(x=IsNew, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  ggtitle("IsNew Count") +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice

# Check Cor between SalePrice and TotalSqFeet
ggplot(data=ReOrdered_DF, aes(x=TotalSqFeet, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  ggtitle("Correlation of TotalSqFeet with SalePrice") +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(ReOrdered_DF$GrLivArea > 4500, rownames(ReOrdered_DF), '')))

cor(ReOrdered_DF$SalePrice, ReOrdered_DF$TotalSqFeet, use= "pairwise.complete.obs")

### Transform (Boxcox, Yeo Johnson transformation, center/scale) ###
# Transform ReOrdered_DF dataset
skewness(ReOrdered_DF$LotArea)
histogram(ReOrdered_DF$LotArea, xlab = "LotArea", type ="count")

ReOrdered_pp <- preProcess(ReOrdered_DF[,40:65], c("BoxCox", "center", "scale"))
House_Trnf <- predict(ReOrdered_pp, ReOrdered_DF)
str(House_Trnf)
skewValBox <- apply(House_Trnf[,40:65], 2, skewness)
skewValBox

skewness(House_Trnf$LotArea)
histogram(House_Trnf$LotArea, xlab = "LotArea", type ="count")

# Transform ReOrdered_DF1 dataset
ReOrdered_pp1 <- preProcess(ReOrdered_DF1[,40:65], c("BoxCox", "center", "scale"))
House_Trnf1 <- predict(ReOrdered_pp1, ReOrdered_DF1)
str(House_Trnf1)

#########   Create Dummy Variables  ############
set.seed(200)
# Dummy variable with one value removed for Linear Models
House_Trnf_Dum_Less <- dummy_cols(House_Trnf, select_columns = c("MSSubClass","MSZoning","LotShape",
                      "LotConfig","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual",
                      "OverallCond","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                      "ExterQual","ExterCond",
                      "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                      "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                      "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                      "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour"),
                      remove_first_dummy = TRUE)

str(House_Trnf_Dum_Less)

#Remove the original categorical columns
House_Trnf_Dum_Less <- dplyr::select(House_Trnf_Dum_Less, c(40:ncol(House_Trnf_Dum_Less)))
str(House_Trnf_Dum_Less)

# Dummy variable with one value removed for Linear Models with NearZero predictors removed
House_Trnf_Dum_Less1 <- dummy_cols(House_Trnf, select_columns = c("MSSubClass","MSZoning","LotShape",
                      "LotConfig","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual",
                      "OverallCond","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                      "ExterQual","ExterCond",
                      "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                      "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                      "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                      "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour"),
                      remove_first_dummy = TRUE)

str(House_Trnf_Dum_Less1)

#Remove the original categorical columns
House_Trnf_Dum_Less1 <- dplyr::select(House_Trnf_Dum_Less1, c(40:ncol(House_Trnf_Dum_Less1)))
str(House_Trnf_Dum_Less1)

#Check for near-zero variance predictors for Linear Models
nZeroCol3 <- nearZeroVar(House_Trnf_Dum_Less1)
colnames(House_Trnf_Dum_Less1)[nZeroCol3]
House_Trnf_Dum_Less1 <- House_Trnf_Dum_Less1[,-nZeroCol3]
str(House_Trnf_Dum_Less1)

# Dummy variable with all values for Tree Models without Near-zero predictors
House_Trnf_DumAll1 <- dummy_cols(House_Trnf, select_columns = c("MSSubClass","MSZoning","LotShape",
                   "LotConfig","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual",
                   "OverallCond","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                   "ExterQual","ExterCond",
                   "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                   "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                   "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                   "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour"),
                   remove_first_dummy = FALSE)
str(House_Trnf_DumAll1)

#Remove the original categorical columns
House_Trnf_DumAll1 <- dplyr::select(House_Trnf_DumAll1, c(40:ncol(House_Trnf_DumAll1)))
str(House_Trnf_DumAll1)

# Dummy variable with all values for Tree Models with Near-zero predictors removed
House_Trnf_DumAll <- dummy_cols(House_Trnf, select_columns = c("MSSubClass","MSZoning","LotShape",
                      "LotConfig","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual",
                      "OverallCond","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                      "ExterQual","ExterCond",
                      "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                      "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                      "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                      "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour"),
                      remove_first_dummy = FALSE)
str(House_Trnf_DumAll)

#Remove the original categorical columns
House_Trnf_DumAll <- dplyr::select(House_Trnf_DumAll, c(40:ncol(House_Trnf_DumAll)))
str(House_Trnf_DumAll)

#Check for near-zero variance predictors for Tree Models
nZeroCol1 <- nearZeroVar(House_Trnf_DumAll)
colnames(House_Trnf_DumAll)[nZeroCol1]
House_Trnf_DumAll <- House_Trnf_DumAll[,-nZeroCol1]
str(House_Trnf_DumAll)

# Dummy variable with one value removed for HouseFullTest dataset for Linear Models without Near-zero predictors
HouseTest_Trnf_Dum_Less <- dummy_cols(House_Trnf1, select_columns = c("MSSubClass","MSZoning","LotShape",
                      "LotConfig","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual",
                      "OverallCond","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                      "ExterQual","ExterCond",
                      "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                      "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                      "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                      "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour"),
                      remove_first_dummy = TRUE)

str(HouseTest_Trnf_Dum_Less)

#Remove the original categorical columns
HouseTest_Trnf_Dum_Less <- dplyr::select(HouseTest_Trnf_Dum_Less, c(40:ncol(HouseTest_Trnf_Dum_Less)))
str(HouseTest_Trnf_Dum_Less)

# Dummy variable with one value removed for HouseFullTest dataset for Linear Models with Near-Zero removed
HouseTest_Trnf_Dum_Less1 <- dummy_cols(House_Trnf1, select_columns = c("MSSubClass","MSZoning","LotShape",
                           "LotConfig","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual",
                           "OverallCond","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                           "ExterQual","ExterCond",
                           "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                           "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                           "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                           "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour"),
                           remove_first_dummy = TRUE)

str(HouseTest_Trnf_Dum_Less1)

#Remove the original categorical columns
HouseTest_Trnf_Dum_Less1 <- dplyr::select(HouseTest_Trnf_Dum_Less1, c(40:ncol(HouseTest_Trnf_Dum_Less1)))
str(HouseTest_Trnf_Dum_Less1)

#Check for near-zero variance predictors for Linear Models
nZeroCol6 <- nearZeroVar(HouseTest_Trnf_Dum_Less1)
colnames(HouseTest_Trnf_Dum_Less1)[nZeroCol6]
HouseTest_Trnf_Dum_Less1 <- HouseTest_Trnf_Dum_Less1[,-nZeroCol6]
str(HouseTest_Trnf_Dum_Less1)

# Dummy variable with one value removed for HouseFullTest dataset for Tree Models without Near-zero predictors
HouseTest_Trnf_DumAll1 <- dummy_cols(House_Trnf1, select_columns = c("MSSubClass","MSZoning","LotShape",
                      "LotConfig","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual",
                      "OverallCond","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                      "ExterQual","ExterCond",
                      "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                      "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                      "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                      "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour"),
                      remove_first_dummy = FALSE)

str(HouseTest_Trnf_DumAll1)

#Remove the original categorical columns
HouseTest_Trnf_DumAll1 <- dplyr::select(HouseTest_Trnf_DumAll1, c(40:ncol(HouseTest_Trnf_DumAll1)))
str(HouseTest_Trnf_DumAll1)

# Dummy variable with one value removed for HouseFullTest dataset for Tree Models with Near-Zero
HouseTest_Trnf_DumAll <- dummy_cols(House_Trnf1, select_columns = c("MSSubClass","MSZoning","LotShape",
                      "LotConfig","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual",
                      "OverallCond","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                      "ExterQual","ExterCond",
                      "Foundation","BsmtQual","BsmtExposure","BsmtFinType1","HeatingQC",
                      "CentralAir","Electrical","KitchenQual","FireplaceQu","GarageType",
                      "GarageFinish","GarageQual","GarageCond","PavedDrive", "Fence", "SaleType",
                      "SaleCondition","MoSold","YrSold","Remod","IsNew","TotBathrooms","LandContour"),
                      remove_first_dummy = FALSE)

str(HouseTest_Trnf_DumAll)

#Remove the original categorical columns
HouseTest_Trnf_DumAll <- dplyr::select(HouseTest_Trnf_DumAll, c(40:ncol(HouseTest_Trnf_DumAll)))
str(HouseTest_Trnf_DumAll)

#Check for near-zero variance predictors for Tree Models
nZeroCol7 <- nearZeroVar(HouseTest_Trnf_DumAll)
colnames(HouseTest_Trnf_DumAll)[nZeroCol7]
HouseTest_Trnf_DumAll <- HouseTest_Trnf_DumAll[,-nZeroCol7]
str(HouseTest_Trnf_DumAll)

#check if some values are absent in the HouseTest_Trnf_Dum_Less / House_Trnf_Dum_Less.
# Structure needs to be same for the datasets for modeling and predicting.
HouseTest_Int <- dplyr::select_if(HouseTest_Trnf_Dum_Less, is.integer)
House_Int <- dplyr::select_if(House_Trnf_Dum_Less, is.integer)
House_Int <- dplyr::select(House_Int, -c('SalePrice'))

trn <- colnames(House_Int)
tst <- colnames(HouseTest_Int)

trn_tst <- setdiff(trn,tst) #Look for columns in Trainng data not present in Test data
tst_trn <- setdiff(tst,trn) #Look for columns in Test data not present in Training data

# Drop dummy colummns from House_Trnf_Dum_Less / HouseTest_Trnf_Dum_Less
House_Trnf_Dum_Less <- House_Trnf_Dum_Less[,!(names(House_Trnf_Dum_Less) %in% trn_tst)]
str(House_Trnf_Dum_Less)

HouseTest_Trnf_Dum_Less <- HouseTest_Trnf_Dum_Less[,!(names(HouseTest_Trnf_Dum_Less) %in% tst_trn)]
str(HouseTest_Trnf_Dum_Less)

# Drop extra numeric column from the datasets
House_Int3 <- dplyr::select(House_Trnf_Dum_Less, -c('SalePrice'))
trn3 <- colnames(House_Int3)
tst3 <- colnames(HouseTest_Trnf_Dum_Less)

trn_tst3 <- setdiff(trn3,tst3) #Look for columns in Trainng data not present in Test data
tst_trn3 <- setdiff(tst3,trn3) #Look for columns in Test data not present in Training data

House_Trnf_Dum_Less <- House_Trnf_Dum_Less[,!(names(House_Trnf_Dum_Less) %in% trn_tst3)]
str(House_Trnf_Dum_Less)

HouseTest_Trnf_Dum_Less <- HouseTest_Trnf_Dum_Less[,!(names(HouseTest_Trnf_Dum_Less) %in% tst_trn3)]
str(HouseTest_Trnf_Dum_Less)

#check if some values are absent in the HouseTest_Trnf_Dum_Less1 / House_Trnf_Dum_Less1.
#With Near-Zero Predictors removed
# Structure needs to be same for the datasets for modeling and predicting.
HouseTest_Int1 <- dplyr::select_if(HouseTest_Trnf_Dum_Less1, is.integer)
House_Int1 <- dplyr::select_if(House_Trnf_Dum_Less1, is.integer)
House_Int1 <- dplyr::select(House_Int1, -c('SalePrice'))

trn1 <- colnames(House_Int1)
tst1 <- colnames(HouseTest_Int1)

trn_tst1 <- setdiff(trn1,tst1) #Look for columns in Trainng data not present in Test data
tst_trn1 <- setdiff(tst1,trn1) #Look for columns in Test data not present in Training data

# Drop dummy colummns from House_Trnf_Dum_Less1 /HouseTest_Trnf_Dum_Less1
House_Trnf_Dum_Less1 <- House_Trnf_Dum_Less1[,!(names(House_Trnf_Dum_Less1) %in% trn_tst1)]
str(House_Trnf_Dum_Less1)

HouseTest_Trnf_Dum_Less1 <- HouseTest_Trnf_Dum_Less1[,!(names(HouseTest_Trnf_Dum_Less1) %in% tst_trn1)]
str(HouseTest_Trnf_Dum_Less1)

# Drop extra numeric column from the datasets
House_Int2 <- dplyr::select(House_Trnf_Dum_Less1, -c('SalePrice'))
trn2 <- colnames(House_Int2)
tst2 <- colnames(HouseTest_Trnf_Dum_Less1)

trn_tst2 <- setdiff(trn2,tst2) #Look for columns in Trainng data not present in Test data
tst_trn2 <- setdiff(tst2,trn2) #Look for columns in Test data not present in Training data

House_Trnf_Dum_Less1 <- House_Trnf_Dum_Less1[,!(names(House_Trnf_Dum_Less1) %in% trn_tst2)]
str(House_Trnf_Dum_Less1)

HouseTest_Trnf_Dum_Less1 <- HouseTest_Trnf_Dum_Less1[,!(names(HouseTest_Trnf_Dum_Less1) %in% tst_trn2)]
str(HouseTest_Trnf_Dum_Less1)

#check if some values are absent in the HouseTest_Trnf_DumAll1 / House_Trnf_DumAll1 for Tree Models
#Without Near-Zero Predictors removed
# Structure needs to be same for the datasets for modeling and predicting.
HouseTestDA_Int1 <- dplyr::select_if(HouseTest_Trnf_DumAll1, is.integer)
HouseDA_Int1 <- dplyr::select_if(House_Trnf_DumAll1, is.integer)
HouseDA_Int1 <- dplyr::select(HouseDA_Int1, -c('SalePrice'))

trnDAl <- colnames(HouseDA_Int1)
tstDAl <- colnames(HouseTestDA_Int1)

trn_tst_DAl <- setdiff(trnDAl,tstDAl) #Look for columns in Trainng data not present in Test data
tst_trn_DAl <- setdiff(tstDAl,trnDAl) #Look for columns in Test data not present in Training data

# Drop dummy colummns from House_Trnf_DumAll /HouseTest_Trnf_DumAll
House_Trnf_DumAll1 <- House_Trnf_DumAll1[,!(names(House_Trnf_DumAll1) %in% trn_tst_DAl)]
str(House_Trnf_DumAll1)

HouseTest_Trnf_DumAll1 <- HouseTest_Trnf_DumAll1[,!(names(HouseTest_Trnf_DumAll1) %in% tst_trn_DAl)]
str(HouseTest_Trnf_DumAll1)

# Drop extra numeric column from the datasets
HouseDA_Int2 <- dplyr::select(House_Trnf_DumAll1, -c('SalePrice'))
trnDA2 <- colnames(HouseDA_Int2)
tstDA2 <- colnames(HouseTest_Trnf_DumAll1)

trn_tst_DA2 <- setdiff(trnDA2,tstDA2) #Look for columns in Trainng data not present in Test data
tst_trn_DA2 <- setdiff(tstDA2,trnDA2) #Look for columns in Test data not present in Training data

House_Trnf_DumAll1 <- House_Trnf_DumAll1[,!(names(House_Trnf_DumAll1) %in% trn_tst_DA2)]
str(House_Trnf_DumAll1)

HouseTest_Trnf_DumAll1 <- HouseTest_Trnf_DumAll1[,!(names(HouseTest_Trnf_DumAll1) %in% tst_trn_DA2)]
str(HouseTest_Trnf_DumAll1)

#check if some values are absent in the HouseTest_Trnf_DumAll / House_Trnf_DumAll for Tree Models
#With Near-Zero Predictors removed
# Structure needs to be same for the datasets for modeling and predicting.
HouseTestDA_Int <- dplyr::select_if(HouseTest_Trnf_DumAll, is.integer)
class(HouseTestDA_Int)
str(HouseTestDA_Int)
dim(HouseTestDA_Int)

HouseDA_Int <- dplyr::select_if(House_Trnf_DumAll, is.integer)
class(HouseDA_Int)
HouseDA_Int <- dplyr::select(HouseDA_Int, -c('SalePrice'))
str(HouseDA_Int)

trnDA <- colnames(HouseDA_Int)
tstDA <- colnames(HouseTestDA_Int)

trn_tst_DA <- setdiff(trnDA,tstDA) #Look for columns in Trainng data not present in Test data
tst_trn_DA <- setdiff(tstDA,trnDA) #Look for columns in Test data not present in Training data

# Drop dummy colummns from House_Trnf_DumAll /HouseTest_Trnf_DumAll
House_Trnf_DumAll <- House_Trnf_DumAll[,!(names(House_Trnf_DumAll) %in% trn_tst_DA)]
str(House_Trnf_DumAll)

HouseTest_Trnf_DumAll <- HouseTest_Trnf_DumAll[,!(names(HouseTest_Trnf_DumAll) %in% tst_trn_DA)]
str(HouseTest_Trnf_DumAll)

# Drop extra numeric column from the datasets
HouseDA_Int1 <- dplyr::select(House_Trnf_DumAll, -c('SalePrice'))
trnDA1 <- colnames(HouseDA_Int1)
tstDA1 <- colnames(HouseTest_Trnf_DumAll)

trn_tst_DA1 <- setdiff(trnDA1,tstDA1) #Look for columns in Trainng data not present in Test data
tst_trn_DA1 <- setdiff(tstDA1,trnDA1) #Look for columns in Test data not present in Training data

House_Trnf_DumAll <- House_Trnf_DumAll[,!(names(House_Trnf_DumAll) %in% trn_tst_DA1)]
str(House_Trnf_DumAll)

HouseTest_Trnf_DumAll <- HouseTest_Trnf_DumAll[,!(names(HouseTest_Trnf_DumAll) %in% tst_trn_DA1)]
str(HouseTest_Trnf_DumAll)

###### Check correlation and remove highly correlated predictors for Regression Models ######
# Without Near-Zero
str(House_Trnf_Dum_Less)
numericVars <- House_Trnf_Dum_Less[,1:27] # numeric variables
cor_numVar <- cor(numericVars, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 
              'TotalRmsAbvGrd', 'BsmtFinSF1')

House_Trnf_Cor_Less <- House_Trnf_Dum_Less[,!(names(House_Trnf_Dum_Less) %in% dropVars)]
str(House_Trnf_Cor_Less)

HouseTest_Trnf_Cor_Less <- HouseTest_Trnf_Dum_Less[,!(names(HouseTest_Trnf_Dum_Less) %in% dropVars)]
str(HouseTest_Trnf_Cor_Less)

# With Near-Zero Predictors removed
str(House_Trnf_Dum_Less1)
numericVars1 <- House_Trnf_Dum_Less1[,1:26] # numeric variables
cor_numVar1 <- cor(numericVars1, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted1 <- as.matrix(sort(cor_numVar1[,'SalePrice'], decreasing = TRUE))

#select only high corelations
CorHigh1 <- names(which(apply(cor_sorted1, 1, function(x) abs(x)>0.5)))
cor_numVar1 <- cor_numVar1[CorHigh1, CorHigh1]

corrplot.mixed(cor_numVar1, tl.col="black", tl.pos = "lt")

dropVars1 <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 
              'TotalRmsAbvGrd', 'BsmtFinSF1')

House_Trnf_Cor_Less1 <- House_Trnf_Dum_Less1[,!(names(House_Trnf_Dum_Less1) %in% dropVars1)]
str(House_Trnf_Cor_Less1)

HouseTest_Trnf_Cor_Less1 <- HouseTest_Trnf_Dum_Less1[,!(names(HouseTest_Trnf_Dum_Less1) %in% dropVars1)]
str(HouseTest_Trnf_Cor_Less1)

###### Check correlation and remove highly correlated predictors for Tree Models ######
# Without Near-Zero
numericVars2 <- House_Trnf_DumAll1[,1:27] # numeric variables
cor_numVar2 <- cor(numericVars2, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted2 <- as.matrix(sort(cor_numVar2[,'SalePrice'], decreasing = TRUE))

#select only high corelations
CorHigh2 <- names(which(apply(cor_sorted2, 1, function(x) abs(x)>0.5)))
cor_numVar2 <- cor_numVar2[CorHigh2, CorHigh2]

corrplot.mixed(cor_numVar2, tl.col="black", tl.pos = "lt")

dropVars2 <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 
              'TotalRmsAbvGrd', 'BsmtFinSF1')

House_Trnf_Cor_DumAll1 <- House_Trnf_DumAll1[,!(names(House_Trnf_DumAll1) %in% dropVars2)]
str(House_Trnf_Cor_DumAll1)

HouseTest_Trnf_Cor_DumAll1 <- HouseTest_Trnf_DumAll1[,!(names(HouseTest_Trnf_DumAll1) %in% dropVars2)]
str(HouseTest_Trnf_Cor_DumAll1)

########### Remove Outliers to check the impact on model performance #####
RmOtlr_House_Trnf_Cor_Less <- House_Trnf_Cor_Less[-c(524, 1299),]
RmOtlr_House_Trnf_Dum_Less <- House_Trnf_Dum_Less[-c(524, 1299),]
RmOtlr_House_Trnf_Cor_Less1 <- House_Trnf_Cor_Less1[-c(524, 1299),]
RmOtlr_House_Trnf_Dum_Less1 <- House_Trnf_Dum_Less1[-c(524, 1299),]
RmOtlr_House_Trnf_Cor_DumAll1 <- House_Trnf_Cor_DumAll1[-c(524, 1299),]
RmOtlr_House_Trnf_DumAll1 <- House_Trnf_DumAll1[-c(524, 1299),]

####### Data partition into Training/Test sets after removing the OUTLIERS #######
#Without Near-Zero. Also Manually removed correlated predictors and outliers for Regression models.
set.seed(200)
RmOtlr_house_Corpart <- createDataPartition(RmOtlr_House_Trnf_Cor_Less[,"SalePrice"], p = 0.75, list =FALSE)

RmOtlr_HouseCor_Train_lin <- RmOtlr_House_Trnf_Cor_Less[RmOtlr_house_Corpart, -22]
RmOtlr_HouseCor_Test_lin <- RmOtlr_House_Trnf_Cor_Less[-RmOtlr_house_Corpart, -22]

RmOtlr_salePriceCor_Train_lin <-RmOtlr_House_Trnf_Cor_Less[RmOtlr_house_Corpart, "SalePrice"]
RmOtlr_salePriceCor_Test_lin <- RmOtlr_House_Trnf_Cor_Less[-RmOtlr_house_Corpart, "SalePrice"]

#With Near-Zero. Also Manually removed correlated predictors and outliers for Regression models.
set.seed(200)
RmOtlr_house_Corpart1 <- createDataPartition(RmOtlr_House_Trnf_Cor_Less1[,"SalePrice"], p = 0.75, list =FALSE)

RmOtlr_HouseCor_Train_lin1 <- RmOtlr_House_Trnf_Cor_Less1[RmOtlr_house_Corpart1, -21]
RmOtlr_HouseCor_Test_lin1 <- RmOtlr_House_Trnf_Cor_Less1[-RmOtlr_house_Corpart1, -21]

RmOtlr_salePriceCor_Train_lin1 <-RmOtlr_House_Trnf_Cor_Less1[RmOtlr_house_Corpart1, "SalePrice"]
RmOtlr_salePriceCor_Test_lin1 <- RmOtlr_House_Trnf_Cor_Less1[-RmOtlr_house_Corpart1, "SalePrice"]

#Without Near-Zero. For Regression Models outliers removed.
set.seed(200)
ro_house_part <- createDataPartition(RmOtlr_House_Trnf_Dum_Less[,"SalePrice"], p = 0.75, list =FALSE)

ro_House_Train_lin <- RmOtlr_House_Trnf_Dum_Less[ro_house_part, -27]
ro_House_Test_lin <- RmOtlr_House_Trnf_Dum_Less[-ro_house_part, -27]

ro_salePrice_Train_lin <-RmOtlr_House_Trnf_Dum_Less[ro_house_part, "SalePrice"]
ro_salePrice_Test_lin <- RmOtlr_House_Trnf_Dum_Less[-ro_house_part, "SalePrice"]

#With Near-Zero. For Regression Models outliers removed.
set.seed(200)
ro_house_part1 <- createDataPartition(RmOtlr_House_Trnf_Dum_Less1[,"SalePrice"], p = 0.75, list =FALSE)

ro_House_Train_lin1 <- RmOtlr_House_Trnf_Dum_Less1[ro_house_part1, -26]
ro_House_Test_lin1 <- RmOtlr_House_Trnf_Dum_Less1[-ro_house_part1, -26]

ro_salePrice_Train_lin1 <-RmOtlr_House_Trnf_Dum_Less1[ro_house_part1, "SalePrice"]
ro_salePrice_Test_lin1 <- RmOtlr_House_Trnf_Dum_Less1[-ro_house_part1, "SalePrice"]

#Without Near-Zero. Also Manually removed correlated predictors for Tree models and outliers removed.
set.seed(200)
ro_house_partTree2 <- createDataPartition(RmOtlr_House_Trnf_Cor_DumAll1[,"SalePrice"], p = 0.75, list =FALSE)

ro_HouseCor_Train_tre1 <- RmOtlr_House_Trnf_Cor_DumAll1[ro_house_partTree2, -22]
ro_HouseCor_Test_tre1 <- RmOtlr_House_Trnf_Cor_DumAll1[-ro_house_partTree2, -22]

ro_salePriceCor_Train_tre1 <- RmOtlr_House_Trnf_Cor_DumAll1[ro_house_partTree2, "SalePrice"]
ro_salePriceCor_Test_tre1 <- RmOtlr_House_Trnf_Cor_DumAll1[-ro_house_partTree2, "SalePrice"]

#Without Near-Zero. For Tree Models outliers removed.
set.seed(200)
ro_house_partTree1 <- createDataPartition(RmOtlr_House_Trnf_DumAll1[,"SalePrice"], p = 0.75, list =FALSE)

ro_House_Train_tre1 <- RmOtlr_House_Trnf_DumAll1[ro_house_partTree1, -27]
ro_House_Test_tre1 <- RmOtlr_House_Trnf_DumAll1[-ro_house_partTree1, -27]

ro_salePrice_Train_tre1 <- RmOtlr_House_Trnf_DumAll1[ro_house_partTree1, "SalePrice"]
ro_salePrice_Test_tre1 <- RmOtlr_House_Trnf_DumAll1[-ro_house_partTree1, "SalePrice"]

####### Data partition into Training/Test sets after removing the correlated predictors #######
#split the data into train and test set for Regression Models
#Without Near-Zero
set.seed(200)
house_Corpart <- createDataPartition(House_Trnf_Cor_Less[,"SalePrice"], p = 0.75, list =FALSE)

HouseCor_Train_lin <- House_Trnf_Cor_Less[house_Corpart, -22]
HouseCor_Test_lin <- House_Trnf_Cor_Less[-house_Corpart, -22]

salePriceCor_Train_lin <-House_Trnf_Cor_Less[house_Corpart, "SalePrice"]
salePriceCor_Test_lin <- House_Trnf_Cor_Less[-house_Corpart, "SalePrice"]

#With Near-Zero predictors removed for Regression Models
set.seed(200)
house_Corpart1 <- createDataPartition(House_Trnf_Cor_Less1[,"SalePrice"], p = 0.75, list =FALSE)

HouseCor_Train_lin1 <- House_Trnf_Cor_Less1[house_Corpart1, -21]
HouseCor_Test_lin1 <- House_Trnf_Cor_Less1[-house_Corpart1, -21]

salePriceCor_Train_lin1 <-House_Trnf_Cor_Less1[house_Corpart1, "SalePrice"]
salePriceCor_Test_lin1 <- House_Trnf_Cor_Less1[-house_Corpart1, "SalePrice"]

########## Data partition into Training/Test sets  ###################
#split the data into train and test set for Linear Regression Without NearZero
set.seed(200)
house_part <- createDataPartition(House_Trnf_Dum_Less[,"SalePrice"], p = 0.75, list =FALSE)

House_Train_lin <- House_Trnf_Dum_Less[house_part, -27]
House_Test_lin <- House_Trnf_Dum_Less[-house_part, -27]

salePrice_Train_lin <-House_Trnf_Dum_Less[house_part, "SalePrice"]
salePrice_Test_lin <- House_Trnf_Dum_Less[-house_part, "SalePrice"]

#### Train/Test Split for Linear Regression With NearZero predictors removed
set.seed(200)
house_part1 <- createDataPartition(House_Trnf_Dum_Less1[,"SalePrice"], p = 0.75, list =FALSE)

House_Train_lin1 <- House_Trnf_Dum_Less1[house_part1, -26]
House_Test_lin1 <- House_Trnf_Dum_Less1[-house_part1, -26]

salePrice_Train_lin1 <-House_Trnf_Dum_Less1[house_part1, "SalePrice"]
salePrice_Test_lin1 <- House_Trnf_Dum_Less1[-house_part1, "SalePrice"]

#split the data into train and test set for Tree Regression without Near-Zero
set.seed(200)
house_partTree1 <- createDataPartition(House_Trnf_DumAll1[,"SalePrice"], p = 0.75, list =FALSE)

House_Train_tre1 <- House_Trnf_DumAll1[house_partTree1, -27]
House_Test_tre1 <- House_Trnf_DumAll1[-house_partTree1, -27]
str(House_Train_tre1)

salePrice_Train_tre1 <- House_Trnf_DumAll1[house_partTree1, "SalePrice"]
salePrice_Test_tre1 <- House_Trnf_DumAll1[-house_partTree1, "SalePrice"]

#split the data into train and test set for Tree Regression With NearZero predictors removed
set.seed(200)
house_partTree <- createDataPartition(House_Trnf_DumAll[,"SalePrice"], p = 0.75, list =FALSE)

House_Train_tre <- House_Trnf_DumAll[house_partTree, -26]
House_Test_tre <- House_Trnf_DumAll[-house_partTree, -26]

salePrice_Train_tre <- House_Trnf_DumAll[house_partTree, "SalePrice"]
salePrice_Test_tre <- House_Trnf_DumAll[-house_partTree, "SalePrice"]

###### Train control for sampling methods
set.seed(200)
ctrl_kfold <- trainControl(method="repeatedcv", repeats=3)
ctrl_lgocv <- trainControl(method = "LGOCV")

##### Diagnostic Plots (QQ Plot, Residuals, Outliers, Homoscadasity)
set.seed(200)
fit <- lm(log(SalePrice)~., data=House_Trnf_Dum_Less)

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

#Remove Outliers
str(House_Trnf_Dum_Less)
House_Trnf_Dum_Less_out <- House_Trnf_Dum_Less
House_Trnf_Dum_Less_out <- House_Trnf_Dum_Less_out[-c(524, 1299),]

##### Diagnostic Plots (QQ Plot, Residuals, Outliers, Homoscadasity)
set.seed(200)
fit1 <- lm(log(SalePrice)~., data=House_Trnf_Dum_Less_out)

par(mfrow=c(2,2))
plot(fit1)
par(mfrow=c(1,1))

# Bonferonni p-value for most extreme obs
outlierTest(fit) 

#Non-constant Variance Score Test
ncvTest(fit)

# Influential Observations added variable plots
cutoff <- 4/((nrow(House_Trnf_Dum_Less)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)

# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# distribution of studentized residuals
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")

# Evaluate Collinearity
#vif(fit) # variance inflation factors
#sqrt(vif(fit)) > 2

# Test for Autocorrelated residuals
durbinWatsonTest(fit)

# Jarque bera / Shapiro test for Normality based on Sample Skewness and Kurtosis
jarque.bera.test(House_Trnf_Dum_Less$SalePrice)
jarque.bera.test(House_Trnf_Dum_Less$LotFrontage)
shapiro.test(House_Trnf_Dum_Less$SalePrice)
shapiro.test(House_Trnf_Dum_Less$LotFrontage)

#Tukey Test to check difference in means of Categories
anova1 <- aov(House_Trnf$SalePrice ~ House_Trnf$Foundation)
summary(anova1)
TukeyHSD(anova1)
plot(TukeyHSD(anova1), las=1)

##############################################################################################
#				Model selection & Tuning
##############################################################################################

#######################  Linear Models ################################
##### Multiple Linear Regression OLS After removing correlated predictors.###
#Without Near Zero
set.seed(200)
lmFit2 <- train(x = HouseCor_Train_lin, y =log(salePriceCor_Train_lin), 
                method = "lm",trControl = ctrl_kfold)
lmFit2

lmFit2sd <- as.data.frame(apply(lmFit2$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(lmFit2sd) <- "Std. Dev"
lmFit2sd
summary(lmFit2)

lmPred <- predict(lmFit2, (HouseCor_Test_lin))
lmPredResult <-data.frame(obs = log(salePriceCor_Test_lin), pred = lmPred)
defaultSummary(lmPredResult)

#Actual vs predicted values
lm_plot <- ggplot()  + 
        geom_point(aes(log(salePriceCor_Test_lin), lmPred)) + 
        geom_smooth(aes(log(salePriceCor_Test_lin), lmPred), method = "lm", se = FALSE, color = "blue") + 
        labs(x = "Actual", y = "Predicted", 
             title =  paste0("Linear Regression - without (Near Zero and Corr Predictors)") ) +
        theme_bw()
lm_plot

#Predict House Prices for TestSet
mlPredTest = predict(lmFit2, HouseTest_Trnf_Cor_Less)
predictions_lm <- exp(mlPredTest)
head(predictions_lm)

##### Multiple Linear Regression OLS After removing correlated predictors.###
#Without Near Zero and removing 2 outliers.
set.seed(200)
lmFitOut <- train(x = RmOtlr_HouseCor_Train_lin, y =log(RmOtlr_salePriceCor_Train_lin), 
                method = "lm",trControl = ctrl_kfold)
lmFitOut

lmFitOutsd <- as.data.frame(apply(lmFitOut$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(lmFitOutsd) <- "Std. Dev"
lmFitOutsd
summary(lmFitOut)

lmPred_Otlr <- predict(lmFitOut, (RmOtlr_HouseCor_Test_lin))
lmPredRes_Otlr <-data.frame(obs = log(RmOtlr_salePriceCor_Test_lin), pred = lmPred_Otlr)
defaultSummary(lmPredRes_Otlr)

#Actual vs predicted values
lm_plot <- ggplot()  + 
  geom_point(aes(log(RmOtlr_salePriceCor_Test_lin), lmPred_Otlr)) + 
  geom_smooth(aes(log(RmOtlr_salePriceCor_Test_lin), lmPred_Otlr), method = "lm", se = FALSE, color = "blue") + 
  labs(x = "Actual", y = "Predicted", 
       title =  paste0("Linear Regression - without (Near Zero, Corr Predictors, & Outliers)") ) +
  theme_bw()
lm_plot

#Predict House Prices for TestSet
lmPredTest_Otlr = predict(lmFitOut, HouseTest_Trnf_Cor_Less)
predictions_lm_Otlr <- exp(lmPredTest_Otlr)
head(predictions_lm_Otlr)

#Add predicted column to the TestDataSet
lmPred_Df <- cbind(ReOrdered_DF1,predictions_lm_Otlr)
str(lmPred_Df)
names(lmPred_Df)[66] <- "PredSalePrice"
str(lmPred_Df)

# ##### Multiple Linear Regression OLS After removing correlated predictors.###
# #With Near Zero and removing 2 outliers.
# set.seed(200)
# lmFitOut1 <- train(x = RmOtlr_HouseCor_Train_lin1, y =log(RmOtlr_salePriceCor_Train_lin1), 
#                   method = "lm",trControl = ctrl_kfold)
# lmFitOut1 
# 
# summary(lmFitOut1)
# 
# lmPred_Otlr1 <- predict(lmFitOut1, (RmOtlr_HouseCor_Test_lin1))
# lmPredRes_Otlr1 <-data.frame(obs = log(RmOtlr_salePriceCor_Test_lin1), pred = lmPred_Otlr1)
# defaultSummary(lmPredRes_Otlr1)
# 
# #Actual vs predicted values
# lm_plot <- ggplot()  + 
#   geom_point(aes(RmOtlr_salePriceCor_Test_lin1, lmPred_Otlr1)) + 
#   geom_smooth(aes(RmOtlr_salePriceCor_Test_lin1, lmPred_Otlr1), method = "lm", se = FALSE, color = "blue") + 
#   labs(x = "Actual", y = "Predicted", 
#        title =  paste0("Regression Type: Linear Regression") ) +
#   theme_bw()
# lm_plot
# 
# #Predict House Prices for TestSet
# lmPredTest_Otlr1 = predict(lmFitOut1, HouseTest_Trnf_Cor_Less1)
# predictions_lm_Otlr1 <- exp(lmPredTest_Otlr1)
# head(predictions_lm_Otlr1)

###### PCR Model with multiple components #####
#Without Near Zero
set.seed(200)
pcrTune = train(x = House_Train_lin, y=log(salePrice_Train_lin), method="pcr", 
                    tuneGrid = expand.grid(ncomp=1:80), trainControl= ctrl_kfold)
pcrTune

pcrTunesd <- as.data.frame(apply(pcrTune$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(pcrTunesd) <- "Std. Dev"
pcrTunesd

testResultsPCR <- data.frame(obs = log(salePrice_Test_lin), pred = predict(pcrTune, House_Test_lin))
defaultSummary(testResultsPCR)

#Gather Results
pcrResamples = pcrTune$results
pcrResamples$Model = "W/O NZ"

#Most important variables PCR
pcrImp = varImp(pcrTune, scale=FALSE)
plot(pcrImp, top=15, scales=list(y=list(cex=.65)),main = 'PCR Without Near Zero')

### PCR Model Without Near Zero. Manually Drop Outlier
set.seed(200)
pcrTune1 = train(x = ro_House_Train_lin, y=log(ro_salePrice_Train_lin), method="pcr", 
                tuneGrid = expand.grid(ncomp=1:80), trainControl= ctrl_kfold)
pcrTune1

pcrTune1sd <- as.data.frame(apply(pcrTune1$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(pcrTune1sd) <- "Std. Dev"
pcrTune1sd

testResultsPCR1 <- data.frame(obs = log(ro_salePrice_Test_lin), pred = predict(pcrTune1, ro_House_Test_lin))
defaultSummary(testResultsPCR1)

#Gather Results
pcrResamples1 = pcrTune1$results
pcrResamples1$Model = "W/O (NZ & Outliers)"

#Most important variables PCR
pcrImp1 = varImp(pcrTune1, scale=FALSE)
plot(pcrImp1, top=15, scales=list(y=list(cex=.65)),main = 'PCR Without (Near Zero & Outliers)')

#Build data for graph to compare the optimal components for Robust Linear
pcr_PlotData = rbind(pcrResamples, pcrResamples1)

xyplot(RMSE ~ ncomp, data = pcr_PlotData, aspect = 1,main = "PCR Comparison",
       xlab = "No Of Components",
       ylab = "(RMSE CV)",
       col=c("blue", "red"),
       auto.key = list(columns=2),
       groups=Model,
       type=c("o","g"))

#### Robust linear regression model with PCA #####
#With Near Zero predictor removed
set.seed(200)
rob_model_PCA1 <- train (x = House_Train_lin1, y =log(salePrice_Train_lin1), method ="rlm", 
                         preProcess=c("pca"),
                         trControl = ctrl_kfold)
rob_model_PCA1

rob_model_PCA1sd <- as.data.frame(apply(rob_model_PCA1$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(rob_model_PCA1sd) <- "Std. Dev"
rob_model_PCA1sd

rob_lmPred_pca <- predict(rob_model_PCA1, House_Test_lin1)
rob_lmPredResult_pca <-data.frame(obs = log(salePrice_Test_lin1), pred = rob_lmPred_pca)
defaultSummary(rob_lmPredResult_pca)

#Predict House Prices for TestSet
RobPredTest = predict(rob_model_PCA1, HouseTest_Trnf_Dum_Less1)
predictions_Rob <- exp(RobPredTest)
head(predictions_Rob)

#### Robust linear regression model with PCA #####
#With Near Zero predictor removed and maually removed outliers.
set.seed(200)
rob_model_PCA2 <- train (x = ro_House_Train_lin1, y =log(ro_salePrice_Train_lin1), method ="rlm", 
                         preProcess=c("pca"),
                         trControl = ctrl_kfold)
rob_model_PCA2

rob_model_PCA2sd <- as.data.frame(apply(rob_model_PCA2$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(rob_model_PCA2sd) <- "Std. Dev"
rob_model_PCA2sd

rob_lmPred_pca2 <- predict(rob_model_PCA2, ro_House_Test_lin1)
rob_lmPredResult_pca2 <-data.frame(obs = log(ro_salePrice_Test_lin1), pred = rob_lmPred_pca2)
defaultSummary(rob_lmPredResult_pca2)

#Predict House Prices for TestSet
RobPredTest2 = predict(rob_model_PCA2, HouseTest_Trnf_Dum_Less1)
predictions_Rob2 <- exp(RobPredTest2)
head(predictions_Rob2)

########## PLS Model with Tuning ###########
#Without Near Zero
set.seed(200)
#plsTune = train(x = House_Train_lin, y=log(salePrice_Train_lin), method="pls",
#                preProc = c("spatialSign"),
#                tuneGrid = expand.grid(ncomp=1:15), trainControl= ctrl_kfold)
#plsTune

plsTune = train(x = House_Train_lin, y=log(salePrice_Train_lin), method="pls", 
                    tuneGrid = expand.grid(ncomp=1:15), trainControl= ctrl_kfold)
plsTune

plsTunesd <- as.data.frame(apply(plsTune$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(plsTunesd) <- "Std. Dev"
plsTunesd

testResultsPLS <- data.frame(obs = log(salePrice_Test_lin), pred = predict(plsTune, House_Test_lin))
defaultSummary(testResultsPLS)
plot(testResultsPLS)

# Gather PLS Tune Results
plsResamples = plsTune$results
plsResamples$Model = "W/O NZ"

#Most important variables PLS
plsImp = varImp(plsTune, scale=FALSE)
plot(plsImp, top=15, scales=list(y=list(cex=.65)),main = 'PLS Without Near Zero')

#Predict House Prices for TestSet
plsPredTest = predict(plsTune, HouseTest_Trnf_Dum_Less)
predictions_PLS <- exp(plsPredTest)
head(predictions_PLS)

##### PLS Model with manually removing correlated predictors and Tuning #####
#Without Near Zero
set.seed(200)
plsTune1 = train(x = HouseCor_Train_lin, y=log(salePriceCor_Train_lin), method="pls", 
                    tuneGrid = expand.grid(ncomp=1:15), trainControl= ctrl_kfold)
plsTune1

plsTunesd1 <- as.data.frame(apply(plsTune1$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(plsTunesd1) <- "Std. Dev"
plsTunesd1

testResultsPLS1 <- data.frame(obs = log(salePriceCor_Test_lin), pred = predict(plsTune1, HouseCor_Test_lin))
defaultSummary(testResultsPLS1)

# Gather PLS Tune Results
plsResamples1 = plsTune1$results
plsResamples1$Model = "W/O (NZ & Corr Predictors)"

#Most important variables PLS
plsImp1 = varImp(plsTune1, scale=FALSE)
plot(plsImp1, top=15, scales=list(y=list(cex=.65)),main = 'PLS Without (Near Zero & Corr Predictors)')

#Predict House Prices for TestSet
plsCorPredTest = predict(plsTune1, HouseTest_Trnf_Dum_Less)
predictions_CorPLS <- exp(plsCorPredTest)
head(predictions_CorPLS)

##### PLS Model with Tuning #####
#Without Near Zero. Manually removed the Outliers
set.seed(200)

ro_plsTune = train(x = ro_House_Train_lin, y=log(ro_salePrice_Train_lin), method="pls", 
                tuneGrid = expand.grid(ncomp=1:15), trainControl= ctrl_kfold)
ro_plsTune

ro_plsTunesd <- as.data.frame(apply(ro_plsTune$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(ro_plsTunesd) <- "Std. Dev"
ro_plsTunesd

ro_testResultsPLS <- data.frame(obs = log(ro_salePrice_Test_lin), pred = predict(ro_plsTune, ro_House_Test_lin))
defaultSummary(ro_testResultsPLS)
plot(ro_testResultsPLS)

# Gather PLS Tune Results
plsResamples2 = ro_plsTune$results
plsResamples2$Model = "W/O (NZ & Outliers)"

#Most important variables PLS
ro_plsImp = varImp(ro_plsTune, scale=FALSE)
plot(ro_plsImp, top=15, scales=list(y=list(cex=.65)),main = 'PLS Without (Near Zero & Outliers)')

#Predict House Prices for TestSet
ro_plsPredTest = predict(ro_plsTune, HouseTest_Trnf_Dum_Less)
ro_predictions_PLS <- exp(ro_plsPredTest)
head(ro_predictions_PLS)

##### PLS Model with Tuning #####
#Without Near Zero. Removed manually Correlated Precitors and the Outliers
set.seed(200)

ro_CorplsTune = train(x = RmOtlr_HouseCor_Train_lin, y=log(RmOtlr_salePriceCor_Train_lin), method="pls", 
                   tuneGrid = expand.grid(ncomp=1:15), trainControl= ctrl_kfold)
ro_CorplsTune

ro_CorplsTunesd <- as.data.frame(apply(ro_CorplsTune$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(ro_CorplsTunesd) <- "Std. Dev"
ro_CorplsTunesd

ro_CortestResultsPLS <- data.frame(obs = log(RmOtlr_salePriceCor_Test_lin), 
                                   pred = predict(ro_CorplsTune, RmOtlr_HouseCor_Test_lin))
defaultSummary(ro_CortestResultsPLS)
plot(ro_CortestResultsPLS)

# Gather PLS Tune Results
plsResamples3 = ro_CorplsTune$results
plsResamples3$Model = "W/O (NZ-Outliers-Cor Pred)"

#Most important variables PLS
ro_CorplsImp = varImp(ro_CorplsTune, scale=FALSE)
plot(ro_CorplsImp, top=15, scales=list(y=list(cex=.65)),main = 'PCR Without (Near Zero, Corr Predictors & Outliers)')

#Predict House Prices for TestSet
ro_CorplsPredTest = predict(ro_CorplsTune, HouseTest_Trnf_Dum_Less)
ro_Corpredictions_PLS <- exp(ro_CorplsPredTest)
head(ro_Corpredictions_PLS)

#Build data for graph to compare the optimal components for PLS
plsPlotData = rbind(plsResamples,plsResamples1,plsResamples2)

xyplot(RMSE ~ ncomp, data = plsPlotData, aspect = 1,main = "PLS Comparison",
       xlab = "No Of Components",
       ylab = "(RMSE CV)",
       col=c("blue", "red", "black"),
       auto.key = list(columns=3),
       groups=Model,
       type=c("o","g"))

########  Ridge Model #############
#with Near Zero

ridgeGrid <- expand.grid(lambda = seq(0, 0.1, length=10))

set.seed(200)
ridgeModel <- train(x = House_Train_lin1, y=log(salePrice_Train_lin1),
                    method = "ridge",
                    tuneGrid = ridgeGrid,
                    trControl = ctrl_kfold)

ridgeModel 
#The final values used for the model lambda = 0

#calculate SD
ridgeModelsd <- as.data.frame(apply(ridgeModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(ridgeModelsd) <- "Std. Dev"
ridgeModelsd

ridgePred = predict(ridgeModel, newdata=House_Test_lin1)
ridgePR = postResample(pred=ridgePred, obs=log(salePrice_Test_lin1))

ridgePR

########  Ridge Model #############
#with Near Zero and outliers Removed
set.seed(200)
ridgeroModel <- train(x = ro_House_Train_lin1, y=log(ro_salePrice_Train_lin1),
                      method = "ridge",
                      tuneGrid = ridgeGrid,
                      trControl = ctrl_kfold)
ridgeroModel

ridgeroPred = predict(ridgeroModel, newdata=ro_House_Test_lin1)
ridgeroPR = postResample(pred=ridgeroPred, obs=log(ro_salePrice_Test_lin1))
ridgeroPR

########  Ridge Model #############
#with Near Zero and Correlated Predictors and outliers Removed
set.seed(200)
ridgeotlrModel <- train(x = RmOtlr_HouseCor_Train_lin1, y=log(RmOtlr_salePriceCor_Train_lin1),
                        method = "ridge",
                        tuneGrid = ridgeGrid,
                        trControl = ctrl_kfold)
ridgeotlrModel

#calculate SD
ridgeotlrModelsd <- as.data.frame(apply(ridgeotlrModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(ridgeotlrModelsd) <- "Std. Dev"
ridgeotlrModelsd

ridgeotlrPred = predict(ridgeotlrModel, newdata=RmOtlr_HouseCor_Test_lin1)
ridgeotlrPR = postResample(pred=ridgeotlrPred, obs=log(RmOtlr_salePriceCor_Test_lin1))
ridgeotlrPR

####  ElasticNet Model #####
#with Near Zero 
enetGrid <- expand.grid(lambda = seq(0, 1, length=10), fraction = seq(0, 1, length = 20))

set.seed(200)
enetModel <- train(x = House_Train_lin1, y=log(salePrice_Train_lin1),
                   method = "enet",
                   tuneGrid = enetGrid,
                   trControl = ctrl_kfold)

enetModel 
#The final values used for the model were fraction = 0.1578947 and lambda = 0.

#calculate SD
enetModelsd <- as.data.frame(apply(enetModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(enetModelsd) <- "Std. Dev"
enetModelsd

enetPred = predict(enetModel, newdata=House_Test_lin1)
enetPR = postResample(pred=enetPred, obs=log(salePrice_Test_lin1))

enetPR

####  ElasticNet Model #####
#with Near Zero and Correlated Predictors and outliers Removed
set.seed(200)
enetotlrModel <- train(x = RmOtlr_HouseCor_Train_lin1, y=log(RmOtlr_salePriceCor_Train_lin1),
                       method = "enet",
                       tuneGrid = enetGrid,
                       trControl = ctrl_kfold)
enetotlrModel
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were fraction = 0.4736842 and lambda = 0.

enetModel$results$lambda <- round(enetModel$results$lambda, 2)
plot(enetModel, sub="ElasticNet Baseline Tuning")

#calculate SD
enetotlrModelsd <- as.data.frame(apply(enetotlrModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(enetotlrModelsd) <- "Std. Dev"
enetotlrModelsd

enetotlrPred = predict(enetotlrModel, newdata=RmOtlr_HouseCor_Test_lin1)
enetotlrPR = postResample(pred=enetotlrPred, obs=log(RmOtlr_salePriceCor_Test_lin1))
enetotlrPR

#######################  Non Linear Models ################################
####  MARS Model #####
#without Near Zero
marsGrid = expand.grid(.degree=1, .nprune= seq(36,38,2)) #MARS OPTIMAL Degree 1, nprune 36,38
#marsGrid = expand.grid(.degree=1:2, .nprune= seq(30,40,2))
#marsGrid = expand.grid(.degree=1:2, .nprune= seq(25,50,5))
#marsGrid = expand.grid(.degree=1:2, .nprune=18:21)
#marsGrid = expand.grid(.degree=1:4, .nprune=15:18)
#marsGrid = expand.grid(.degree=1:4, .nprune=2:15)

set.seed(200)
marsModel = train(x = House_Train_lin, y=log(salePrice_Train_lin), method="earth", 
                  #preProc=c("center", "scale"), 
                  tuneGrid=marsGrid, trControl=ctrl_kfold)
marsModel

plot(marsModel, sub="MARS Baseline Tuning (Degree=1:5, Terms:2:15)")
plot(marsModel, sub="MARS Baseline Tuning (Degree=1)")

# Lets see what variables are most important: 
varImp(marsModel)

#calculate SD
marsModelsd <- as.data.frame(apply(marsModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(marsModelsd) <- "Std. Dev"
marsModelsd

# OverallCond_5                       28.23
marsPred = predict(marsModel, newdata=House_Test_lin)
marsPR = postResample(pred=marsPred, obs=log(salePrice_Test_lin))

marsPR

####  MARS Model #####
#without Near Zero and removed Outliers/Correlated Predictors
set.seed(200)
marsotlrModel = train(x = RmOtlr_HouseCor_Train_lin, y=log(RmOtlr_salePriceCor_Train_lin), method="earth", 
                      tuneGrid=marsGrid, trControl=ctrl_kfold)

marsotlrModel
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were nprune = 36 and degree = 1.

#calculate SD
marsotlrModelsd <- as.data.frame(apply(marsotlrModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(marsotlrModelsd) <- "Std. Dev"
marsotlrModelsd

marsotlrPred = predict(marsotlrModel, newdata=RmOtlr_HouseCor_Test_lin)
marsotlrPR = postResample(pred=marsotlrPred, obs=log(RmOtlr_salePriceCor_Test_lin))
marsotlrPR

####  Support Vector Machine Model with PCA #####
#without Near Zero
set.seed(200)
# tune against the cost C
svmRModel = train(x = House_Train_lin, y=log(salePrice_Train_lin), method="svmRadial", preprocess = "pca", trControl = ctrl_kfold, tuneLength=4)

svmRModel

plot(svmRModel)

#calculate SD
svmRModelsd <- as.data.frame(apply(svmRModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(svmRModelsd) <- "Std. Dev"
svmRModelsd

# Lets see what variables are most important: 
varImp(svmRModel)

svmRPred = predict(svmRModel, newdata=House_Test_lin)
svmPR = postResample(pred=svmRPred, obs=log(salePrice_Test_lin)) 

svmPR

####  Support Vector Machine Model with PCA #####
#without Near Zero Outliers removed Manually
set.seed(200)
svmRroModel = train(x = ro_House_Train_lin, y=log(ro_salePrice_Train_lin), method="svmRadial", preprocess = "pca", trControl = ctrl_kfold, tuneLength=4)

svmRroModel

plot(svmRroModel)

svmRroPred = predict(svmRroModel, newdata=ro_House_Test_lin)
svmroPR = postResample(pred=svmRroPred, obs=log(ro_salePrice_Test_lin)) 

svmroPR

####  Support Vector Machine Model with PCA #####
#without Near Zero Outliers and correlated predictors removed Manually
set.seed(200)
svmRotlrModel = train(x = RmOtlr_HouseCor_Train_lin, y=log(RmOtlr_salePriceCor_Train_lin), 
                      method="svmRadial", preprocess = "pca", trControl = ctrl_kfold, tuneLength=6)

svmRotlrModel
#Tuning parameter 'sigma' was held constant at a value of 0.00439248
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were sigma = 0.00439248 and C = 2.

#calculate SD
svmRotlrModelsd <- as.data.frame(apply(svmRotlrModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(svmRotlrModelsd) <- "Std. Dev"
svmRotlrModelsd

svmRotlrPred = predict(svmRotlrModel, newdata=RmOtlr_HouseCor_Test_lin)
svmRotlrPR = postResample(pred=svmRotlrPred, obs=log(RmOtlr_salePriceCor_Test_lin))
svmRotlrPR

#########  Neural Network #####
#Without Near-zero
nnGrid = expand.grid( .decay=c(0.4,0.42,0.45,0.47,0.5), .size=1) #Neural Network optimal range size=1, decay=0.4 to 0.5 after repeated tuning configurations
#nnGrid = expand.grid( .decay=c(0.4,0.42,0.45,0.47,0.5), .size=1:2)
#nnGrid = expand.grid( .decay=c(0,0.2,0.3,0.4), .size=1:2)
#nnGrid = expand.grid( .decay=c(0,0.01,0.1,0.2), .size=1:2)
#nnGrid = expand.grid( .decay=c(0,0.01,0.1,0.2), .size=1:3)
#nnGrid = expand.grid( .decay=c(0,0.01,0.1), .size=1:10 )

set.seed(200)

nnetModel = train(x = House_Train_lin, y=salePrice_Train_lin, method="nnet", 
                  #preProc=c("center", "scale"), 
                  linout=TRUE, trace=FALSE, 
                  MaxNWts=1000, 
                  maxit=200, tuneGrid = nnGrid)

nnetModel

#RMSE was used to select the optimal model using the smallest value
#The final values used for the model were size = 1 and decay = 0.45.

#calculate SD
nnetModelsd <- as.data.frame(apply(nnetModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(nnetModelsd) <- "Std. Dev"
nnetModelsd

# Lets see what variables are most important: 
varImp(nnetModel)

nnetPred = predict(nnetModel, newdata=House_Test_lin)
nnetPR = postResample(pred=nnetPred, obs=log(salePrice_Test_lin))

nnetPR

#########  Neural Network #####
#Without Near-zero. Correlated predictors and Outliers Removed
set.seed(200)
nnetotlrModel = train(x = RmOtlr_HouseCor_Train_lin, y=log(RmOtlr_salePriceCor_Train_lin), method="nnet", 
                      #preProc=c("center", "scale"), 
                      linout=TRUE, trace=FALSE, 
                      MaxNWts=2000, 
                      maxit=200, tuneGrid = nnGrid)

nnetotlrModel 

nnetotlrModelsd <- as.data.frame(apply(nnetotlrModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(nnetotlrModelsd) <- "Std. Dev"
nnetotlrModelsd

nnetotlrPred = predict(nnetotlrModel, newdata=RmOtlr_HouseCor_Test_lin)

nnetotlrPR = postResample(pred=nnetotlrPred, obs=log(RmOtlr_salePriceCor_Test_lin))
nnetotlrPR

####  Average Neural Networks #####
#Without Near-zero
avgGrid <- expand.grid(size = c(2), decay = c(0.6, 0.65, 0.7))  #Average Neural Networks Optimal c=2, decay 0.65 after repeated trials
#avgGrid <- expand.grid(size = c(1:2), decay = c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9))
#avgGrid <- expand.grid(size = c(1:2), decay = c(0.1, 0.6, 0.65, 0.7, 0.75))
#avgGrid <- expand.grid(size = c(1:2), decay = c(0.1, 0.5, 0.6, 0.65))
#avgGrid <- expand.grid(size = c(1:4), decay = c(0.1, 0.5, 0.6, 0.65))
maxSize <- max(avgGrid$size)
avgGrid$bag <- FALSE

#nnGrid = expand.grid( .decay=c(0,0.01,0.1,0.2), .size=1:3)

set.seed(200)
avNNetModel = train(x = House_Train_lin, y=log(salePrice_Train_lin), 
                    method="avNNet", 
                    #preProc=c("center", "scale"), 
                    linout=TRUE,trace=FALSE,
                    MaxNWts=6 * (ncol(House_Train_lin)+1) + 10 + 1, 
                    maxit=200, tuneGrid = avgGrid)

avNNetModel
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were size = 2, decay = 0.65 and bag = FALSE

#calculate SD
avNNetModelsd <- as.data.frame(apply(avNNetModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(avNNetModelsd) <- "Std. Dev"
avNNetModelsd

# Lets see what variables are most important: 
varImp(avNNetModel)

avNNetPred = predict(avNNetModel, newdata=House_Test_lin)
avNNetPR = postResample(pred=avNNetPred, obs=log(salePrice_Test_lin))

avNNetPR

summary(avNNetPR)

#########  Average Neural Network #####
#Without Near-zero. Manually removed Outliers
set.seed(200)
avNNetroModel = train(x = ro_House_Train_lin, y=log(ro_salePrice_Train_lin), 
                      method="avNNet", 
                      #preProc=c("center", "scale"), 
                      linout=TRUE,trace=FALSE,
                      MaxNWts=6 * (ncol(ro_House_Train_lin)+1) + 10 + 1, 
                      maxit=200, tuneGrid = avgGrid)

avNNetroModel

avNNetroPred = predict(avNNetroModel, newdata=ro_House_Test_lin)
avNNetroPR = postResample(pred=avNNetroPred, obs=log(ro_salePrice_Test_lin))

avNNetroPR

#########  Average Neural Network #####
#Without Near-zero. Manually removed Outliers and Correlated Predictors
set.seed(200)
avnetotlrModel = train(x = RmOtlr_HouseCor_Train_lin, y=log(RmOtlr_salePriceCor_Train_lin), 
                       method="avNNet", 
                       #preProc=c("center", "scale"), 
                       linout=TRUE,trace=FALSE,
                       MaxNWts=6 * (ncol(RmOtlr_HouseCor_Train_lin)+1) + 10 + 1, 
                       maxit=200, tuneGrid = avgGrid)

avnetotlrModel 
#Tuning parameter 'bag' was held constant at a value of FALSE
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were size = 2, decay = 0.65 and bag = FALSE.

#calculate SD
avnetotlrModelsd <- as.data.frame(apply(avnetotlrModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(avnetotlrModelsd) <- "Std. Dev"
avnetotlrModelsd

set.seed(200)
avnetotlrPred = predict(avnetotlrModel, newdata=RmOtlr_HouseCor_Test_lin)

avnetotlrPR = postResample(pred=avnetotlrPred, obs=log(RmOtlr_salePriceCor_Test_lin))
avnetotlrPR

#Plot Obs vs Pred with Logarithmic Units
avnet_obs_pred_plot <- ggplot(data=data.frame(x=log(RmOtlr_salePriceCor_Test_lin), y=avnetotlrPred), aes(x=x, y=y)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Average Neural Network - Observed vs Predicted Sales Price (Log) ') +
  xlab("Observed Sales Price") + ylab("Predicted Sales Price") + 
  theme(plot.title = element_text(size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


avnetDF <- as.data.frame(cbind(RmOtlr_salePriceCor_Test_lin, exp(avnetotlrPred)))
colnames(avnetDF) <- c('Observed', 'Predicted')
class(avnetDF)
avnetDF$Residual <- abs(avnetDF$Observed - avnetDF$Predicted)
avnetDF$PercDev <- 100*avnetDF$Residual/avnetDF$Observed
avnetDF[order(avnetDF$PercDev),]

avnetDF1 <- avnetDF[order(avnetDF$PercDev),]

nrow(avnetDF1)

nrow(avnetDF1[avnetDF1$PercDev > 10,])

nrow(avnetDF1[avnetDF1$PercDev <= 10,])

#Variable Importance Comparision of Regression Models

varImp(svmRotlrModel)

#enetbaseplot <- plot(varImp(enetotlrModel), top = 15, scales = list(y = list(cex = .95)), font.main = 1, main = 'Elastic Net Model')
svmotlrplot <- plot(varImp(svmRotlrModel), top = 15, scales = list(y = list(cex = .95)), font.main = 1, main = 'Support Vector Machine Model')
marsotlrplot <- plot(varImp(marsotlrModel), top = 15, scales = list(y = list(cex = .95)), font.main = 1, main = 'MARS Model')
nnetotlrplot <- plot(varImp(nnetotlrModel), top = 15, scales = list(y = list(cex = .95)), font.main = 1, main = 'Neural Networks Model')
avnetotlrplot <- plot(varImp(avnetotlrModel), top = 15, scales = list(y = list(cex = .95)), font.main = 1, main = 'Average Neural Networks Model')

grid.arrange(svmotlrplot,marsotlrplot,avnetotlrplot, ncol=3,
             top=textGrob("Variable Importance - Non-Linear Models (Outliers & Correlated Predictors Removed)",
             gp = gpar(fontsize = 14, fontface = 'bold'), rot=0, vjust=0.1))

####  KNN Model #####
#Without Near-zero
set.seed(200)

ktune <- 7 #KNN OPTIMAL TUNE at 5, 7 after trying upto tunelength = 30

knnModel = train(x = House_Train_lin, y=log(salePrice_Train_lin), 
                 method="knn",
                 #preProc=c("center","scale"),
                 tuneLength=ktune, trControl=ctrl_kfold)
knnModel

plot(knnModel)
#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was k = 5

#calculate SD
knnModelsd <- as.data.frame(apply(knnModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(knnModelsd) <- "Std. Dev"
knnModelsd

knnPred = predict(knnModel, newdata=House_Test_lin)
## The function 'postResample' can be used to get the test set performance values
knnPR = postResample(pred=knnPred, obs=log(salePrice_Test_lin))

knnPR

####  KNN Model #####
#Without Near-zero. Manually removed Outliers and Correlated Predictors
set.seed(200)
knnotlrModel = train(x = RmOtlr_HouseCor_Train_lin, y=log(RmOtlr_salePriceCor_Train_lin),
                     method="knn",
                     tuneLength=ktune, trControl=ctrl_kfold)

knnotlrModel

plot(knnotlrModel)
#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was k = 7.

#calculate SD
knnotlrModelsd <- as.data.frame(apply(knnotlrModel$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(knnotlrModelsd) <- "Std. Dev"
knnotlrModelsd

knnotlrPred = predict(knnotlrModel, newdata=RmOtlr_HouseCor_Test_lin)

knnotlrPR = postResample(pred=knnotlrPred, obs=log(RmOtlr_salePriceCor_Test_lin))
knnotlrPR

#######################  Tree Models #####################################
### Simple Regression Tree #####
#Without Near-zero predictors removed
set.seed=200
srtTune1 = train(x = House_Train_tre1, y=log(salePrice_Train_tre1),method="rpart",
                 tuneGrid = expand.grid(cp = seq(0,0.1,0.01)),
                 trControl= ctrl_kfold)

srtTune1

# Calculate SD
srtTune1sd <- as.data.frame(apply(srtTune1$resample[, 1:2], 2, sd)) 
colnames(srtTune1sd) <- "Std. Dev"
srtTune1sd

testResultsSRT1 <- data.frame(obs = log(salePrice_Test_tre1), 
                              pred = predict(srtTune1, House_Test_tre1))
defaultSummary(testResultsSRT1)

#Simple Regression Tree Without Near-zero and Outliers Removed
set.seed=200
ro_srtTune1 = train(x = ro_House_Train_tre1, y=log(ro_salePrice_Train_tre1),method="rpart",
                    tuneGrid = expand.grid(cp = seq(0, 0.1,0.01)),
                    trControl= ctrl_kfold)

ro_srtTune1

# Calculate SD
ro_srtTune1sd <- as.data.frame(apply(ro_srtTune1$resample[, 1:2], 2, sd)) 
colnames(ro_srtTune1sd) <- "Std. Dev"
ro_srtTune1sd

ro_testResultsSRT1 <- data.frame(obs = log(ro_salePrice_Test_tre1), 
                                 pred = predict(ro_srtTune1, ro_House_Test_tre1))
defaultSummary(ro_testResultsSRT1)


#Simple Regression Tree Without Near-zero removed, Outliers Removed.Correlation Removed
set.seed=200
ro_srtTune2 = train(x = ro_HouseCor_Train_tre1, y=log(ro_salePriceCor_Train_tre1),method="rpart",
                    tuneGrid = expand.grid(cp = seq(0, 0.1,0.01)),
                    trControl= ctrl_kfold)

ro_srtTune2

# Calculate SD
ro_srtTune2sd <- as.data.frame(apply(ro_srtTune2$resample[, 1:2], 2, sd)) 
colnames(ro_srtTune2sd) <- "Std. Dev"
ro_srtTune2sd

ro_testResultsSRT2 <- data.frame(obs = log(ro_salePriceCor_Test_tre1), 
                                 pred = predict(ro_srtTune2, ro_HouseCor_Test_tre1))
defaultSummary(ro_testResultsSRT1)

# Actual vs Predicted Plots (Simple Regression Tree)
# Without Near-zero predictors removed
AP1 <- ggplot(testResultsSRT1,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("Regression Tree - W/O Near-Zero") +
  xlab("Predicted Output") + ylab("Observed Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
#Without Near-zero and Outliers Removed
AP2 <- ggplot(ro_testResultsSRT1,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("W/O Near-Zero and Outliers") +
  xlab("Predicted Output") + ylab("Observed Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
#Without Near-zero removed, Outliers Removed.Correlation Removed
AP3 <- ggplot(ro_testResultsSRT2,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("W/O Near-Zero, Outliers,Corr Predictors") +
  xlab("Predicted Output") + ylab("Observed Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

# Combining plots into grid
grid.arrange(AP1,AP2,AP3,ncol = 2, nrow = 2)

# Variable Importance Plot
# Without Near-zero predictors removed
VIP1 <- plot(varImp(srtTune1),15,main="Regression Tree - W/O Near-Zero")
#Without Near-zero and Outliers Removed
VIP2 <- plot(varImp(ro_srtTune1),15,main="W/O Near-Zero and Outliers")
#Without Near-zero removed, Outliers Removed.Correlation Removed
VIP3 <- plot(varImp(ro_srtTune2),15,main="W/O Near-Zero, Outliers,Corr Predictors")
# Combining plots into grid
grid.arrange(VIP1,VIP2,VIP3,ncol = 2, nrow = 2)

####### Random Forest Regression #####
#Without Near-zero
set.seed=200
rfTune1 = train(x = House_Train_tre1, y=log(salePrice_Train_tre1),ntree=120, method="rf", 
                 tuneGrid = expand.grid(mtry=5:8),trainControl= ctrl_kfold)
rfTune1

rfTune1sd <- as.data.frame(apply(rfTune1$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(rfTune1sd) <- "Std. Dev"
rfTune1sd

testResultsRf1 <- data.frame(obs = log(salePrice_Test_tre1), 
                              pred = predict(rfTune1, House_Test_tre1))
defaultSummary(testResultsRf1)

#Predict House Prices for TestSet
rfPredTest1 = predict(rfTune1, HouseTest_Trnf_DumAll1)
predictions_rf1 <- exp(rfPredTest1)
head(predictions_rf1)

#Without Near-zero. Outliers Removed
set.seed=200
ro_rfTune1 = train(x = ro_House_Train_tre1, y=log(ro_salePrice_Train_tre1),ntree=120, method="rf", 
                tuneGrid = expand.grid(mtry=5:8),trainControl= ctrl_kfold)
ro_rfTune1

ro_rfTune1sd <- as.data.frame(apply(ro_rfTune1$resample[, 1:2], 2, sd)) #Calculate Std Dev
colnames(ro_rfTune1sd) <- "Std. Dev"
ro_rfTune1sd

ro_testResultsRf1 <- data.frame(obs = log(ro_salePrice_Test_tre1), 
                             pred = predict(ro_rfTune1, ro_House_Test_tre1))
defaultSummary(ro_testResultsRf1)

# Actual vs Predicted Plots (Random Forest)
# Without Near-zero predictors removed
AP4 <- ggplot(testResultsRf1,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("Random Forest - W/O Near-Zero") +
  xlab("Predicted Output") + ylab("Observed Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
#Without Near-zero and Outliers Removed
AP5 <- ggplot(ro_testResultsRf1,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("W/O Near-Zero and Outliers Removed") +
  xlab("Predicted Output") + ylab("Observed Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

# Combining plots into grid
grid.arrange(AP4,AP5,ncol = 1, nrow = 2)

# Variable Importance Plot (Random Forest)
# Without Near-zero predictors removed
VIP4 <- plot(varImp(rfTune1),15,main="Random Forest - W/O Near-Zero")
#Without Near-zero and Outliers Removed
VIP5 <- plot(varImp(ro_rfTune1),15,main="W/O Near-Zero and Outliers Removed")
# Combining plots into grid
grid.arrange(VIP4,VIP5,ncol = 1, nrow = 2)

#Predict House Prices for TestSet
ro_rfPredTest1 = predict(ro_rfTune1, HouseTest_Trnf_DumAll1)
ro_predictions_rf1 <- exp(ro_rfPredTest1)
head(ro_predictions_rf1)

###### Boosted Tree Model #####
#Without Near-zero
set.seed=200
gbmModel1 = gbm.fit( House_Train_tre1, salePrice_Train_tre1, distribution="gaussian",
                     n.trees =2000, interaction.depth=6, shrinkage=0.1,
                     verbose = FALSE,bag.fraction = .75)
gbmModel1

gbm_yHat1 = predict(gbmModel1,n.trees = 2000, newdata = data.frame(x=House_Test_tre1))

## performance evaluation
gbmPR1 = postResample(pred=gbm_yHat1, obs=salePrice_Test_tre1)
gbmPR1

#Create DataFrame for Plots
gbm_yHat1_DF<- data.frame(obs = log(salePrice_Test_tre1), 
                          pred = gbm_yHat1)

#Most important variables Boosted Tree
par(mar = c(5, 8, 1, 1))
summary(
  gbmModel1,
  cBars = 15,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

#Predict House Prices for TestSet
gbmPredTest1 = predict(gbmModel1, HouseTest_Trnf_DumAll1)
head(gbmPredTest1)
gbm.perf(gbmModel1)

#Without Near-zero. Outliers Removed
set.seed=200
gbmModel2 = gbm.fit( ro_House_Train_tre1, ro_salePrice_Train_tre1, distribution="gaussian",
                     n.trees =2000, interaction.depth=6, shrinkage=0.1,
                     verbose = FALSE,bag.fraction = .75)

gbm_yHat2 = predict(gbmModel2,n.trees = 2000, newdata = data.frame(x=ro_House_Test_tre1))

## performance evaluation
gbmPR2 = postResample(pred=gbm_yHat2, obs=ro_salePrice_Test_tre1)
gbmPR2

#Create DataFrame for Plots
gbm_yHat2_DF<- data.frame(obs = log(ro_salePrice_Test_tre1), 
                          pred = gbm_yHat2)

#Most important variables Boosted Tree
par(mar = c(5, 8, 1, 1))
summary(
  gbmModel2, 
  cBars = 15,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

#Predict House Prices for TestSet
gbmPredTest2 = predict(gbmModel2, HouseTest_Trnf_DumAll1)
head(gbmPredTest2)
gbm.perf(gbmModel2)

#Without Near-zero. Correlated predictors and Outliers Removed
set.seed=200
gbmModel3 = gbm.fit( ro_HouseCor_Train_tre1, ro_salePriceCor_Train_tre1, distribution="gaussian",
                     n.trees =2000, interaction.depth=6, shrinkage=0.1,
                     verbose = FALSE,bag.fraction = .75)

gbm_yHat3 = predict(gbmModel3,n.trees = 2000, newdata = data.frame(x=ro_HouseCor_Test_tre1))

## performance evaluation
gbmPR3 = postResample(pred=gbm_yHat3, obs=ro_salePriceCor_Test_tre1)
gbmPR3

#Create DataFrame for Plots
gbm_yHat3_DF<- data.frame(obs = log(ro_salePriceCor_Test_tre1), 
                          pred = gbm_yHat3)

#Most important variables Boosted Tree
par(mar = c(5, 8, 1, 1))
summary(
  gbmModel3, 
  cBars = 15,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

#Predict House Prices for TestSet
gbmPredTest3 = predict(gbmModel3, HouseTest_Trnf_Cor_DumAll1)
head(gbmPredTest3)
gbm.perf(gbmModel3)

# Actual vs Predicted Plots (Gradient Boosting Tree)
# Without Near-zero predictors removed
ggplot(gbm_yHat1_DF,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("Boosted Tree - W/O Near-Zero") +
  xlab("Predicted Sale Price") + ylab("Observed Sale Price") + 
  theme(plot.title = element_text(color="black",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

#Without Near-zero and Outliers Removed
ggplot(gbm_yHat2_DF,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("Boosted Tree - W/O Near-Zero and Outliers Removed") +
  xlab("Predicted Sale Price") + ylab("Observed Sale Price") + 
  theme(plot.title = element_text(color="black",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

#Without Near-zero. Correlated predictors and Outliers Removed
ggplot(gbm_yHat3_DF,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("Boosted Tree - Observed Vs Predicted Sales Price (Log)") +
  xlab("Predicted Sale Price") + ylab("Observed Sale Price") + 
  theme(plot.title = element_text(color="black",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

#Add predicted column to the TestDataSet
gbmPred_Df <- cbind(ReOrdered_DF1,gbmPredTest3)
str(gbmPred_Df)
names(gbmPred_Df)[66] <- "PredSalePrice"
str(gbmPred_Df)

# #Without Near-zero. Correlated predictors and Outliers Removed (Cross Validation)
# set.seed=2000
# ro_HouseCor_Train_tre1 <- RmOtlr_House_Trnf_Cor_DumAll1[ro_house_partTree2,]
# ro_HouseCor_Test_tre1 <- RmOtlr_House_Trnf_Cor_DumAll1[-ro_house_partTree2,]
# 
# set.seed=2000
# gbmWithCrossValidation = gbm(formula = ro_HouseCor_Train_tre1$SalePrice ~ .,
#                              distribution = "gaussian",data = ro_HouseCor_Train_tre1,
#                              n.trees = 2000, interaction.depth=6, shrinkage = .1,
#                              #                             n.minobsinnode = 200, 
#                              cv.folds = 10,verbose = FALSE,bag.fraction = .75, n.cores = 2)
# 
# 
# gbmHoldoutPredictions = predict(object = gbmWithCrossValidation,
#                                 n.trees = 2000,
#                                 newdata = ro_HouseCor_Test_tre1,
#                                 type = "response")
# ## performance evaluation
# gbmPR4 = postResample(pred=gbmHoldoutPredictions, obs=ro_HouseCor_Test_tre1$SalePrice)
# gbmPR4
# gbm.perf(gbmWithCrossValidation, method="cv")

########### XGBoost Model ############
#Without Near-zero predictors removed
#ctrl_kfold2 <- trainControl(method="repeatedcv", repeats=5)

xgbGrid <- expand.grid(nrounds = 200,
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1)

# set.seed(200)
# xgbTune1<- train(x = House_Train_tre1, y=log(salePrice_Train_tre1),method="xgbTree",
#                  trControl= ctrl_kfold,tuneGrid = xgbGrid)
# 
# xgbTune1
# 
# # Calculate SD
# xgbTune1sd <- as.data.frame(apply(xgbTune1$resample[, 1:2], 2, sd)) 
# colnames(xgbTune1sd) <- "Std. Dev"
# xgbTune1sd
# 
# testResultsXGB1 <- data.frame(obs = log(salePrice_Test_tre1), 
#                               pred = predict(xgbTune1, House_Test_tre1))
# defaultSummary(testResultsXGB1)
# 
# #XGBoost Model Without Near-zero removed. Outliers Removed
# set.seed(200)
# ro_xgbTune1<- train(x = ro_House_Train_tre1, y=log(ro_salePrice_Train_tre1),method="xgbTree",
#                     trControl= ctrl_kfold,tuneGrid = xgbGrid)
# 
# ro_xgbTune1
# 
# # Calculate SD
# ro_xgbTune1sd <- as.data.frame(apply(ro_xgbTune1$resample[, 1:2], 2, sd)) 
# colnames(ro_xgbTune1sd) <- "Std. Dev"
# ro_xgbTune1sd
# 
# ro_testResultsXGB1 <- data.frame(obs = log(ro_salePrice_Test_tre1), 
#                                  pred = predict(ro_xgbTune1, ro_House_Test_tre1))
# defaultSummary(ro_testResultsXGB1)

#XGBoost Model Without Near-zero removed, Outliers Removed.Correlation Removed
set.seed(200)
ro_xgbTune2<- train(x = ro_HouseCor_Train_tre1, y=log(ro_salePriceCor_Train_tre1),method="xgbTree",
                    trControl= ctrl_kfold,tuneGrid = xgbGrid)

ro_xgbTune2

# Calculate SD
ro_xgbTune2sd <- as.data.frame(apply(ro_xgbTune2$resample[, 1:2], 2, sd)) 
colnames(ro_xgbTune2sd) <- "Std. Dev"
ro_xgbTune2sd

ro_testResultsXGB2 <- data.frame(obs = log(ro_salePriceCor_Test_tre1), 
                                 pred = predict(ro_xgbTune2, ro_HouseCor_Test_tre1))
defaultSummary(ro_testResultsXGB2)

# Actual vs Predicted Plots (XgBoost Tree)
# Without Near-zero predictors removed
# AP6 <- ggplot(testResultsXGB1,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
#   geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("XgBoost W/O Near-Zero Removed") +
#   xlab("Predecited Output") + ylab("Observed Output") + 
#   theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
#         axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
#         axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

# #Without Near-zero and Outliers Removed
# AP7 <- ggplot(ro_testResultsXGB1,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
#   geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("W/O Near-Zero Removed and Outliers Removed") +
#   xlab("Predecited Output") + ylab("Observed Output") + 
#   theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
#         axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
#         axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

#Without Near-zero removed, Outliers Removed.Correlation Removed
AP8 <- ggplot(ro_testResultsXGB2,aes(pred, obs)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Tree Regression ') + ggtitle("XgBoost W/O Near-Zero, Outliers,Corr Predictors") +
  xlab("Predecited Output") + ylab("Observed Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

# Variable Importance Plot (XgBoost Tree)
# Without Near-zero predictors removed
# VIP6 <- plot(varImp(xgbTune1),15,main="W/O Near-Zero")
# #Without Near-zero and Outliers Removed
# VIP7 <- plot(varImp(ro_xgbTune1),15,main="W/O Near-Zero and Outliers Removed")
# #Without Near-zero removed, Outliers Removed.Correlation Removed\
# VIP8 <- plot(varImp(ro_xgbTune2),15,main="W/O Near-Zero and Outliers,Corelated Predictors Removed")
# Combining plots into grid
#grid.arrange(VIP6,VIP7,VIP8,ncol = 2, nrow = 2)

#Variable Importance Comparison of Tree Models
#dev.off()

VIP_SRT <- plot(varImp(srtTune1), top = 15, scales = list(y = list(cex = .95)), font.main = 1, main = 'Simple Regression Tree')
VIP_RF <- plot(varImp(ro_rfTune1), top = 15, scales = list(y = list(cex = .95)), font.main = 1, main = 'Random Forest')

VIP_XGB <- plot(varImp(ro_xgbTune2), top = 15, scales = list(y = list(cex = .95)), font.main = 1, main = 'XgBoost')

grid.arrange(VIP_SRT,VIP_RF,VIP_XGB,ncol=2, 
             top=textGrob("Variable Importance - Tree Models (Outliers & Corelated Predictors Removed)",
            gp = gpar(fontsize = 14, fontface = 'bold'), rot=0, vjust=0.1))

######### Compare Trainset and Testset model performance with Best GBM Model
#Neighborhood vs SalesPrice
BP1gbmP <- ggplot(gbmPred_Df, 
                  aes(x = Neighborhood , y = PredSalePrice,fill=Neighborhood))+
  geom_boxplot(alpha=0.3)+
  ggtitle("BoxPlot for Neighborhood") +
  xlab("Neighborhood") + ylab("PredSalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))
BP1gbmP

# Trainset Neighborhood == "StoneBr" & HouseAge == 0
# Reorder columns in ReOrdered_DF dataset for readability
ReOrdered_DFt <- filter(ReOrdered_DF, Neighborhood == "StoneBr" & HouseAge == 0)[, c(
  "TotalSqFeet","ExterQual","HouseAge","GrLivArea","TotalBsmtSF","GarageArea",
  "LotArea","Fireplaces","KitchenQual","OverallQual","Remod","IsNew","TotBathrooms",
  "SalePrice")]
ReOrdered_DFt

# Testset Neighborhood == "StoneBr" & HouseAge == 0
# Reorder columns in ReOrdered_DF dataset for readability
gbmPred_Dfp <- filter(gbmPred_Df, Neighborhood == "StoneBr" & HouseAge == 0)[, c(
  "TotalSqFeet","ExterQual","HouseAge","GrLivArea","TotalBsmtSF","GarageArea",
  "LotArea","Fireplaces","KitchenQual","OverallQual","Remod","IsNew","TotBathrooms",
  "PredSalePrice")]
gbmPred_Dfp

######### Compare Trainset and Testset model performance with Best Linear Model
#Neighborhood vs SalesPrice
BP1lmP <- ggplot(lmPred_Df, 
                  aes(x = Neighborhood , y = PredSalePrice,fill=Neighborhood))+
  geom_boxplot(alpha=0.3)+
  ggtitle("BoxPlot for Neighborhood") +
  xlab("Neighborhood") + ylab("PredSalesPrice")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))
BP1lmP

# Trainset Neighborhood == "StoneBr" & HouseAge == 0
# Reorder columns in ReOrdered_DF dataset for readability
ReOrdered_DFt <- filter(ReOrdered_DF, Neighborhood == "StoneBr" & HouseAge == 0)[, c(
  "TotalSqFeet","ExterQual","HouseAge","GrLivArea","TotalBsmtSF","GarageArea",
  "LotArea","Fireplaces","KitchenQual","OverallQual","Remod","IsNew","TotBathrooms",
  "SalePrice")]
ReOrdered_DFt

# Testset Neighborhood == "StoneBr" & HouseAge == 0
# Reorder columns in ReOrdered_DF dataset for readability
lmPred_Dfp <- filter(lmPred_Df, Neighborhood == "StoneBr" & HouseAge == 0)[, c(
  "TotalSqFeet","ExterQual","HouseAge","GrLivArea","TotalBsmtSF","GarageArea",
  "LotArea","Fireplaces","KitchenQual","OverallQual","Remod","IsNew","TotBathrooms",
  "PredSalePrice")]
lmPred_Dfp

#Code Execution Time
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
