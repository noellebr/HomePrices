#Multiple Linear Regression #install.packages("mice") #install.packages("lattice")
  #library(lattice)
  #library(mice)
  #install.packages("mlr") # Impute Package library(mlr) # Impute Library
  # To Increase the no of lines to print in console #options(max.print=1000000)
  #-----Handelling NA----------
# Handelling NAs, Use Impute Function
train_d = read.csv("train.csv", stringsAsFactors = FALSE)
test_d = read.csv("testCleaned.csv", stringsAsFactors = FALSE)
train_imp <- impute(train_d, cols = list(LotFrontage = imputeMean())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(MSZoning = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
#?as.data.frame
train_imp <- impute(train, cols = list(MasVnrType = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(MasVnrArea = imputeMean())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(BsmtQual = imputeMode()))
train <- as.data.frame(train_imp[1]) colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(BsmtCond = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(BsmtExposure = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(BsmtFinType1 = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(BsmtFinType2 = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(Electrical = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(FireplaceQu = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(GarageType = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(GarageYrBlt = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(GarageFinish = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
train_imp <- impute(train, cols = list(GarageQual = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)

train_imp <- impute(train, cols = list(GarageCond = imputeMode())) train <- as.data.frame(train_imp[1])
colnames(train) <- colnames(train_d)
str(train) head(train, 10)
#----test-----
test_imp <- impute(test_d, cols = list(LotFrontage = imputeMean())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(MSZoning = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
#?as.data.frame
test_imp <- impute(test, cols = list(MasVnrType = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(MasVnrArea = imputeMean())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(BsmtQual = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(BsmtCond = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(BsmtExposure = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(BsmtFinType1 = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)

test_imp <- impute(test, cols = list(BsmtFinType2 = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(Electrical = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(FireplaceQu = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(GarageType = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(GarageYrBlt = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(GarageFinish = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(GarageQual = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
test_imp <- impute(test, cols = list(GarageCond = imputeMode())) test <- as.data.frame(test_imp[1])
colnames(test) <- colnames(test_d)
str(test)
head(test, 10)
#--------------------test impute end-------
#-------------------------Handelling NA--------
#---------------DATA PREP-----------START------------------------------------------------------ # Exploratory Data Analysis

#train = read.csv("train.csv")
#test = read.csv("testCleaned.csv")
dim(train) dim(test)
str(train)
#As we can see the training data contains 1460 records with 81 variables including an ID and the sale price. The test data has 1 fewer variable because
#it should not contain the prediction target SalePrice:
str(test)
#Now the factor levels should be identical across both data sets. Let's continue our exploration by looking at a summary of the training data.
summary(train)
#Inspecting the output above reveals that our data is not entirely clean. First, certain variables contain NA values which could
#cause problems when we make predictive models later on. Second, the levels of some of the factor variables are not the same across the training
#set and test set. For instance:
levels(train$MiscFeature) levels(test$MiscFeature)
#Differing factor levels could cause problems with predictive modeling later on so we need to resolve these issues before going further.
#We can make sure the train and test sets have the same factor levels by loading each data set again without converting strings to factors,
#combining them into one large data set, converting strings to factors for the combined data set and then separating them. Let's change any
#NA values we find in the character data to a new level called "missing" while we're at it:
#train = read.csv("train.csv", stringsAsFactors = FALSE)
#test = read.csv("testCleaned.csv", stringsAsFactors = FALSE)
# Remove the target variable not found in test set SalePrice = train$SalePrice
train$SalePrice = NULL
# Remove extreme values from training set------------------------------------------------------------

# Combine data sets full_data = rbind(train,test)
# Convert character columns to factor, filling NA values with "NoData" for (col in colnames(full_data)){
if (typeof(full_data[,col]) == "character"){ new_col = full_data[,col] new_col[is.na(new_col)] = "NoData" full_data[col] = as.factor(new_col)
} }
str(full_data)
# Separate out our train and test sets
train = full_data[1:nrow(train),]
train$SalePrice = SalePrice
test = full_data[(nrow(train)+1):nrow(full_data),]
# Delete extreme values------------------------------------- train <- subset(train, GrLivArea < 4000)
train <- subset(train, BsmtFinSF1 < 5600)
train <- subset(train, MiscVal < 8000)
train <- subset(train, LotFrontage < 200) #train <- subset(train, LotArea < 7000) train <- subset(train, MasVnrArea < 1500)
# Fill remaining numeric columns with NA values with -1 train[is.na(train)] = -1
test[is.na(test)] = -1
# Add variable that combines above grade living area with basement sq footage train$total_sq_footage = train$GrLivArea + train$TotalBsmtSF test$total_sq_footage = test$GrLivArea + test$TotalBsmtSF
# Add variable that combines above ground and basement full and half baths train$total_baths = train$BsmtFullBath + train$FullBath + (0.5 * (train$BsmtHalfBath + train$HalfBath))
test$total_baths = test$BsmtFullBath + test$FullBath + (0.5 * (test$BsmtHalfBath + test$HalfBath))
# Add variable that combines Total SF
#train$TotalSF = train$TotalBsmtSF + train$X1stFlrSF + train$X2ndFlrSF #test$TotalSF = test$TotalBsmtSF + test$X1stFlrSF + test$X2ndFlrSF
# Remove Id since it should have no value in prediction train$Id = NULL
test$Id = NULL
#Handle NoData in Test
test$KitchenQual[test$KitchenQual %in% c("NoData")] <- "TA" test$MSZoning[test$MSZoning %in% c("NoData")] <- "RL" test$Functional[test$Functional %in% c("NoData")] <- "Typ"
test_temp = read.csv("testCleaned.csv")
#We want to log certain variables based on assumptions-------------------------------------------
train$logsaleprice <- log(train$SalePrice+1) train$logBSMTFinSF1 <- log(train$BsmtFinSF1+1) train$logTotalBSMTSF <- log(train$TotalBsmtSF+1) train$logGrLivArea <- log(train$GrLivArea)
test$logBSMTFinSF1 <- log(test$BsmtFinSF1+1) test$logTotalBSMTSF <- log(test$TotalBsmtSF+1) test$logGrLivArea <- log(test$GrLivArea)
#write.csv(train , file = "train-pro.csv", row.names = FALSE) #write.csv(test , file = "test-pro.csv", row.names = FALSE)
# Splitting training data to train and test for internal rmse validation---------------- x <-sample(1:1189,200)
train_test <- train[x,]
train_train <- train[-x,]
train_testlogprice <- train_test$logsaleprice train_test$logsaleprice = NULL train_test$SalePrice = NULL
#---------------DATA PREP-----------END------------------------------------------------------

#--------------------------------------------------------------------------------ASE Test---------------------
# -----------------Final Models-------------------
# Remove Functional, MiscFeature doe test predictions , kaggle 0.12502, Condition2
model_b <- lm(logsaleprice ~ MSZoning + Street + LotConfig + LandSlope + Neighborhood + Condition1 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + CentralAir + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + TotRmsAbvGrd + Fireplaces + GarageArea + GarageCond + WoodDeckSF + ScreenPorch + Fence + SaleCondition + total_sq_footage + total_baths + logBSMTFinSF1 + logTotalBSMTSF + logGrLivArea , data = train_train)
# --------------------Best Model------------------
# Custom from Backward Elim Remove Street, MasVnrArea, X1stFlrSF, X2ndFlrSF, Fence, BsmtFinSF2, logBSMTFinSF1 , logTotalBSMTSF, BsmtFinType2 , BedroomAbvGr , TotRmsAbvGrd
# Best kaggle -> 0.12322 Custom from Backward Elim, removed Functional,Condition2 model_b <- lm(logsaleprice ~ MSZoning + LotConfig + LandSlope + Neighborhood + Condition1 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + ExterQual + ExterCond + Foundation + BsmtQual + BsmtUnfSF + Heating + HeatingQC + CentralAir + Fireplaces + GarageArea + GarageCond + WoodDeckSF + ScreenPorch + SaleCondition + total_sq_footage + total_baths + logGrLivArea , data = train_train)
summary(model_b)
predictions <- predict(model_b, newdata = train_test) #?rmse
rmse(train_testlogprice, predictions)
rmse_cv <- sqrt(sum(((train_testlogprice - predictions)^2)/200)) rmse_cv
#install.packages(Metrics)
#library(ModelMetrics)
ASE_Test <- sum(((train_testlogprice - predictions)^2)/200) ASE_Test
exp(ASE_Test)
#---------------------------------------------------------ASE Test------------------------------------------------------------------
#---------------olsrr Package---------------------Linear Model-------------------------------------#

#install.packages("olsrr") library(olsrr)
#Full Model
model <-lm(SalePrice ~ . #Residual vs Fitted Values Plot ols_rvsp_plot(model) #Residual Fit Spread Plot ols_rfs_plot(model)
           # Collinearity Diagnostics ols_coll_diag(model)
           , data = train)
#Stepwise AIC Backward Elimination Model --------------Regression----------------------- b <- ols_stepaic_backward(model,details = TRUE)
b
# ?ols_stepaic_backward
# -----------------Final Models-------------------
# Remove Functional, MiscFeature doe test predictions , kaggle 0.12502
model_b <- lm(logsaleprice ~ MSZoning + Street + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + CentralAir + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + TotRmsAbvGrd + Fireplaces + GarageArea + GarageCond + WoodDeckSF + ScreenPorch + Fence + SaleCondition + total_sq_footage + total_baths + logBSMTFinSF1 + logTotalBSMTSF + logGrLivArea , data = train)
# --------------------Best Model------------------
# Custom from Backward Elim Remove Street, MasVnrArea, X1stFlrSF, X2ndFlrSF, Fence, BsmtFinSF2, logBSMTFinSF1 , logTotalBSMTSF, BsmtFinType2 , BedroomAbvGr , TotRmsAbvGrd
# Best kaggle -> 0.12322 Custom from Backward Elim, removed Functional
model_b <- lm(logsaleprice ~ MSZoning + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + ExterQual + ExterCond + Foundation + BsmtQual + BsmtUnfSF + Heating + HeatingQC + CentralAir + Fireplaces + GarageArea + GarageCond + WoodDeckSF + ScreenPorch + SaleCondition + total_sq_footage + total_baths + logGrLivArea , data = train)
summary(model_b)
plot(model_b) ols_cooksd_barplot(model_b) #ols_cooksd_chart(model_b) ols_srsd_chart(model_b) # Residual ols_rsdlev_plot(model_b) # Leverage
predictions_b <- predict(model_b, newdata = test) #Exponentiate the predictions
predictions_b <-exp(predictions_b)
predictions_b<- cbind(test_temp$Id, predictions_b) colnames(predictions_b) <- c("Id", "SalePrice") predictions_b <-as.data.frame(predictions_b)
predictions_b$SalePrice[predictions_b$SalePrice <= 0] <- 150000
# Write CSV in R
write.csv(predictions_b , file = "Result_b_25.csv", row.names = FALSE)
#----------------End------------------- #--------------------------------------------------------------------------------
Model 3: LASSO (SAS Code)
/*Data was cleaned in R. This cleaned data was imported into SAS for this analysis (see Analysis 2 for data cleaning code)*/
  /*add saleprice column to test set*/ data testpro;
set testpro;
SalePrice = .;
logSalePrice = .; run;
/*generate random sample from training set to split into train and test set*/ proc surveyselect data=trainpro samprate=0.50 out=Sample outall method=srs;
run;

/*set training set & test set using random samples*/ data train;
set Sample;
where selected=1;
run;
data test;
set Sample; where selected=0; run;
data train;
set train testpro; run;
/*lasso selection using external cross validation*/
  proc glmselect data=train testdata=test plots(stepaxis=number)=(criterionpanel ASEPlot);
class MSZoning Street LotShape LandContour Utilities LotConfig OverallQual LandSlope Neighborhood Condition1 Condition2 BldgType HouseStyle RoofStyle RoofMatl Exterior1st Exterior2nd MasVnrType ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinType2 Heating HeatingQC CentralAir Electrical KitchenQual Functional FireplaceQu GarageType GarageFinish GarageQual GarageCond PavedDrive PoolArea MiscVal SaleType SaleCondition;
model logSalePrice = Total_Sq_Footage MSSubClass LotFrontage LotArea OverallQual OverallCond YearBuilt YearRemodAdd MasVnrArea BsmtFinSF1 BsmtFinSF2 BsmtUnfSF TotalBsmtSF X1stFlrSF X2ndFlrSF LowQualFinSF GrLivArea BsmtFullBath BsmtHalfBath FullBath HalfBath BedroomAbvGr KitchenAbvGr TotRmsAbvGrd Fireplaces GarageYrBlt GarageCars GarageArea WoodDeckSF OpenPorchSF EnclosedPorch X3SsnPorch ScreenPorch PoolArea MiscVal MoSold YrSold MSZoning Street LotShape LandContour Utilities LotConfig LandSlope Neighborhood Condition1 Condition2 BldgType HouseStyle RoofStyle RoofMatl Exterior1st Exterior2nd MasVnrType ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinType2 Heating HeatingQC CentralAir Electrical KitchenQual Functional FireplaceQu GarageType GarageFinish GarageQual GarageCond PavedDrive PoolArea MiscVal SaleType SaleCondition/ selection=lasso(choose=cv stop=cv) / stats=bic;
output out = results2 p=predict;
run;
/*export data and un-log results*/
  data results3;
set results2;
if predict > 0 then SalePrice = exp(Predict); if predict < 0 then SalePrice = 100000;

keep id SalePrice; where id > 1460; Run;