#Loading libraries
library(ggplot2)
library(splines)

#Loading training and testing sets
train <- read.csv("/Users/allisonking/Downloads/house-prices-advanced-regression-techniques/train.csv", stringsAsFactors = F) 
test <- read.csv("/Users/allisonking/Downloads/house-prices-advanced-regression-techniques/test.csv", stringsAsFactors = F) 
dim(train)
dim(test)

#Making SalePrice column so train & test can be combined with same number of columns
test$SalePrice <- NA

#Combine train and test sets
train.id <- train$Id
test.id <- test$Id
total <- rbind(train, test)
str(total)

#First - look for missing data
miss_var_summary(total) %>% print(n=2919)
names(which(colSums(is.na(total)) > 0))

#Second - fix missing data
#Recode variables with NA as "None" or something else
total$Alley <- ifelse(is.na(total$Alley), "None", total$Alley)
total$BsmtQual <- ifelse(is.na(total$BsmtQual), "None", total$BsmtQual)
total$BsmtCond <- ifelse(is.na(total$BsmtCond), "None", total$BsmtCond)
total$BsmtExposure <- ifelse(is.na(total$BsmtExposure), "None", total$BsmtExposure)
total$BsmtFinType1 <- ifelse(is.na(total$BsmtFinType1), "None", total$BsmtFinType1)
total$BsmtFinType2 <- ifelse(is.na(total$BsmtFinType2), "None", total$BsmtFinType2)
total$BsmtFinSF1 <- ifelse(is.na(total$BsmtFinSF1), 0, total$BsmtFinSF1)
total$BsmtFinSF2 <- ifelse(is.na(total$BsmtFinSF2), 0, total$BsmtFinSF2)
total$BsmtFullBath <- ifelse(is.na(total$BsmtFullBath), 0, total$BsmtFullBath)
total$BsmtHalfBath <- ifelse(is.na(total$BsmtHalfBath), 0, total$BsmtHalfBath)
total$BsmtUnfSF <- ifelse(is.na(total$BsmtUnfSF), 0, total$BsmtUnfSF)
total$FireplaceQu <- ifelse(is.na(total$FireplaceQu), "None", total$FireplaceQu)
total$GarageType <- ifelse(is.na(total$GarageType), "None", total$GarageType)
total$GarageFinish <- ifelse(is.na(total$GarageFinish), "None", total$GarageFinish)
total$GarageQual <- ifelse(is.na(total$GarageQual), "None", total$GarageQual)
total$GarageCond <- ifelse(is.na(total$GarageCond), "None", total$GarageCond)
total$LotFrontage <- ifelse(is.na(total$LotFrontage), 0, total$LotFrontage)
total$PoolQC <- ifelse(is.na(total$PoolQC), "None", total$PoolQC)
total$Fence <- ifelse(is.na(total$Fence), "None", total$Fence)
total$MiscFeature <- ifelse(is.na(total$MiscFeature), "None", total$MiscFeature)
total$Functional <- ifelse(is.na(total$Functional), "Typ", total$Functional)
total$GarageArea <- ifelse(is.na(total$GarageArea), 0, total$GarageArea)
total$SaleType <- ifelse(is.na(total$SaleType), "Oth", total$SaleType)
total$TotalBsmtSF <- ifelse(is.na(total$TotalBsmtSF), 0, total$TotalBsmtSF)

#Data imputation for the truly missing variables 
#MasVnrType - KNN imputation
ames_imp_3 <- kNN(total, variable = "MasVnrType")

#MasVnrArea
ames_imp_4 <- kNN(ames_imp_3, variable = "MasVnrArea")

#Electrical
ames_imp_5 <- kNN(ames_imp_4, variable = "Electrical")

#KitchenQual 
ames_imp_6 <- kNN(ames_imp_5, variable = "KitchenQual")

#MSZoning
ames_imp_7 <- kNN(ames_imp_6, variable="MSZoning")

#Exterior1st
ames_imp_8 <- kNN(ames_imp_7, variable="Exterior1st")

#Exterior2nd
ames_imp_9 <- kNN(ames_imp_8, variable="Exterior2nd")

#GarageCars
ames_imp_10 <- kNN(ames_imp_9, variable="GarageCars")

#Remove utilities as it has one level and GarageYrBlt, throws an error
ames_imp_11 <- ames_imp_10 %>% dplyr::select(-Utilities)
ames_imp_12 <- ames_imp_11 %>% dplyr::select(-GarageYrBlt)

#Check for missing valus
miss_var_summary(ames_imp_12) %>% print(n=2919)

#Convert to factor
ames_imp_12[sapply(ames_imp_12, is.character)] <- lapply(ames_imp_12[sapply(ames_imp_12, is.character)], 
                                                         as.factor)

#Log sales price
ames_imp_12$logSP <- log(ames_imp_12$SalePrice)

#Convert MSSubClass, OverallQual, and OverallCond to factor
ames_imp_12$MSSubClass <- as.factor(ames_imp_12$MSSubClass)
ames_imp_12$OverallQual <- as.factor(ames_imp_12$OverallQual)
ames_imp_12$OverallCond <- as.factor(ames_imp_12$OverallCond)

#Problem 1
#Features 1-3 - I liked Bruin's house age idea w/ penalty. How old is the house from the year sold? Creating a column for the age of the house.
#0 = no remodeling done, 1 = remodeling done
ames_imp_12$Remodel <- ifelse(ames_imp_12$YearBuilt == ames_imp_12$YearRemodAdd, 0, 1) 
ames_imp_12$HouseAge <- as.numeric(ames_imp_12$YrSold) - ames_imp_12$YearRemodAdd

#Creating "IsNew" column to see how many houses are new, 0 = Not new, 1 = new
ames_imp_12$IsNew <- ifelse(ames_imp_12$YrSold == ames_imp_12$YearBuilt, 1, 0)

#Graphing median sale price of home along with if the home has been remodeled or not
ggplot(ames_imp_12[!is.na(ames_imp_12$SalePrice),], aes(x=as.factor(Remodel), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='pink') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=4) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 16) +
  geom_hline(yintercept=163000, linetype="solid") + ggtitle("Remodeled Home by Sale Price")

#Graphing median sale price of home along with if the home is new or not
ggplot(ames_imp_12[!is.na(ames_imp_12$SalePrice),], aes(x=as.factor(IsNew), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='orange') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=4) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 16) +
  geom_hline(yintercept=163000, linetype="solid") + ggtitle("New Home by Sale Price")

#Feature 4 
#Also from Eric Bruin, adding a total square footage column
ames_imp_12$TotalSqFt <- ames_imp_12$GrLivArea + ames_imp_12$TotalBsmtSF

#Checking correlation - not taking out outliers
cor(ames_imp_12$SalePrice, ames_imp_12$TotalSqFt, use= "pairwise.complete.obs")

#Depict correlation graphically
ggplot(data=ames_imp_12[!is.na(ames_imp_12$SalePrice),], aes(x=TotalSqFt, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(ames_imp_12$GrLivArea[!is.na(ames_imp_12$SalePrice)]>4500, rownames(ames_imp_12), ''))) + 
  ggtitle("Sale Price vs. Total Square Footage")

#Feature 5
#From Eric Bruin - consolidating the four porch variables
ames_imp_12$TotalPorchSF <- ames_imp_12$OpenPorchSF + ames_imp_12$EnclosedPorch + ames_imp_12$X3SsnPorch + ames_imp_12$ScreenPorch

#Feature 6
#From Eric Bruin - Adding up bathrooms
ames_imp_12$TotBathrooms <- ames_imp_12$FullBath + (ames_imp_12$HalfBath*0.5) + ames_imp_12$BsmtFullBath + (ames_imp_12$BsmtHalfBath*0.5)

#Problem 2
#Calculate skewness for every numeric column in the dataset
num_ames_imp <- ames_imp_12[,c(1,4,5,19,20,26,34,36:38,43:52,54,56,60,61,65:70,74:76,79:85)]
skewValues <- apply(num_ames_imp, 2, skewness)
head(skewValues)

#Identify and remove predictors with near-zero variance
nearZeroVar(ames_imp_12)
colnames(ames_imp_12)[nearZeroVar(ames_imp_12)]
total.2 <- ames_imp_12[,-nearZeroVar(ames_imp_12)]
totCorr <- cor(total.2[,c(1,4,5,14,15,20,27:29,33:40,4,43,47,48,52,54,55,58:63)])


#Natural cubic spline here
#TotalBsmtSF & SalePrice has a curved/exponential shape in the scatterplot - good candidate for NS
ggplot(total.2, aes(x=TotalBsmtSF, y=SalePrice)) +
  geom_point(size=2, shape=23)

#Create a natural spline with 5 df for the "TotalBsmtSF" column
TotalBsmtSF.spline <- ns(total.2$TotalBsmtSF, df=5)

#Combine the original data with the spline data
ames_imp_12.2 <- cbind(total.2, TotalBsmtSF.spline)

#Give the basis functions more reasonable column names
colnames(ames_imp_12.2)[65:69] <- c('TotalBsmtSF.NS1', 'TotalBsmtSF.NS2', 'TotalBsmtSF.NS3',
                                    'TotalBsmtSF.NS4', 'TotalBsmtSF.NS5')

#Check to make sure everything worked
head(ames_imp_12.2)

#Removing original column 
ames_imp_12.2 <- ames_imp_12.2 %>% dplyr::select(-TotalBsmtSF)

#Lastly, need to dummy encode categorical variables for data
dmy <- dummyVars(" ~ .", data = ames_imp_12.2, fullRank=T)
ames2 <- data.frame(predict(dmy, newdata = ames_imp_12.2))

#Separate train and test set back to original form
trsf <- ames2 %>% filter(Id %in% train.id) %>% dplyr::select(-Id)
trsf_test <- ames2 %>% filter(Id %in% test.id) %>% dplyr::select(-Id, -SalePrice, -logSP)

#Remove salePrice from training set and save as it's own variable
salePriceVar <- trsf$SalePrice
salePriceLog <- trsf$logSP
trsf <- trsf %>% dplyr::select(-SalePrice, -logSP)

#Numeric column difference
dplyr::all_equal(trsf_test, trsf)

#Which columns are missing
compare_df_cols(trsf, trsf_test)

#Add in columns to testing set
x <- c(setdiff(colnames(trsf), colnames(trsf_test)))
trsf_test[,x] <- 0

compare_df_cols(trsf_test, trsf)

#Add in comulms to training set
x <- c(setdiff(colnames(trsf_test), colnames(trsf)))
trsf[,x] <- 0


#Ridge Regression
ames.ridge <- cv.glmnet(x = as.matrix(trsf), y = salePriceLog, alpha=0)

plot(ames.ridge$glmnet.fit, "lambda", label=FALSE)

#Optimal tuning parameter
best.lambda.ridge <- ames.ridge$lambda.min

#RMSE
mse.min <- ames.ridge$cvm[ames.ridge$lambda == ames.ridge$lambda.min]
sqrt(mse.min)

#Check parameter estimates for the optimal model
coef(ames.ridge, s=best.lambda.ridge)

ames.predict.ridge <- predict(ames.ridge, newx=as.matrix(trsf_test), s=best.lambda.ridge)

exp.ames.predict.ridge <- apply(ames.predict.ridge, 2, exp)

submission <- cbind.data.frame(test$Id,exp.ames.predict.ridge)
names(submission)[names(submission) == "test$Id"] <- "Id"
names(submission)[names(submission) == "s1"] <- "SalePrice"

#export files for submission to kaggle
which(is.na(submission), arr.ind=TRUE) #check for NA
write.csv(submission, file="/Users/allisonking/Desktop/submission-ridge.csv",row.names = FALSE)

#Elastic Net
tcontrol <- trainControl(method="repeatedcv", number=10, repeats=5)

ames.en <- train(as.matrix(trsf), salePriceLog,
                 trControl=tcontrol, method="glmnet",metric = "RMSE", tuneLength=10)

ames.en$bestTune

ames.en.best <- ames.en$finalModel

plot(ames.en.best)

coef(ames.en.best, s=ames.en$bestTune$lambda)

ames.predict.en <- predict(ames.en, trsf_test)

ames.predict.en <- as.matrix(sapply(ames.predict.en, as.numeric))

exp.ames.predict.en <- apply(ames.predict.en, 2, exp)

submission <- cbind.data.frame(test$Id,exp.ames.predict.en)
names(submission)[names(submission) == "test$Id"] <- "Id"
names(submission)[names(submission) == "exp.ames.predict.en"] <- "SalePrice"

#export files for submission to kaggle
which(is.na(submission), arr.ind=TRUE) #check for NA
write.csv(submission, file="/Users/allisonking/Desktop/submission-elasticnet.csv",row.names = FALSE)

#LASSO
#LASSO for feature selection
lasso_fit <- cv.glmnet(x = as.matrix(trsf), y = salePriceLog, alpha = 1)

plot(lasso_fit$glmnet.fit, "lambda", label=FALSE)

#Optimal tuning parameter
best.lambda.lasso <- lasso_fit$lambda.min

#RMSE
mse.min.lasso <- lasso_fit$cvm[lasso_fit$lambda == lasso_fit$lambda.min]
sqrt(mse.min.lasso)

#Check parameter estimates for the optimal model
coef(lasso_fit, s=best.lambda.lasso)

ames.predict.lasso <- predict(lasso_fit, newx=as.matrix(trsf_test), s=best.lambda.lasso)

exp.ames.predict.lasso <- apply(ames.predict.lasso, 2, exp)

submission <- cbind.data.frame(test$Id,exp.ames.predict.lasso)
names(submission)[names(submission) == "test$Id"] <- "Id"
names(submission)[names(submission) == "s1"] <- "SalePrice"

#export files for submission to kaggle
which(is.na(submission), arr.ind=TRUE) #check for NA
write.csv(submission, file="/Users/allisonking/Desktop/submission-lasso.csv",row.names = FALSE)


