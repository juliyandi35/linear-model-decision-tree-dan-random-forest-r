# load libraries and data
library(dplyr)
library(readr)
library(ggplot2)
# load data
Dataset <- read.csv('train.csv')
Dataset <- data.frame(Dataset$SalePrice,Dataset$LotArea,Dataset$OverallQual,
                      Dataset$TotalBsmtSF,Dataset$X1stFlrSF,Dataset$X2ndFlrSF,
                      Dataset$FullBath,Dataset$YearBuilt,Dataset$YearRemodAdd)
colnames(Dataset) <- c("SalePrice","LotArea","OverallQual",
                       "TotalBsmtSF","X1stFlrSF","X2ndFlrSF",
                       "FullBath","YearBuilt","YearRemodAdd")
is.na(Dataset) # Check if the NA exist
str(Dataset) # Check the data structure

Dataset$OverallQual <- as.factor(Dataset$OverallQual)
Dataset$FullBath <- as.factor(Dataset$FullBath)

str(Dataset) # Recheck the data structure

# Plot the data relationships
ggplot(Dataset, aes(x = LotArea, y = SalePrice)) +
  geom_point()+ ggtitle("Hubungan Harga Jual dan Lot Size")
ggplot(Dataset, aes(x = OverallQual, y = SalePrice)) +
  geom_point()+ ggtitle("Hubungan Harga Jual dan Penilaian Hasil Pembangunan")
ggplot(Dataset, aes(x = TotalBsmtSF, y = SalePrice)) +
  geom_point()+ ggtitle("Hubungan Harga Jual dan Luas Basement")
ggplot(Dataset, aes(x = X1stFlrSF, y = SalePrice)) +
  geom_point()+ ggtitle("Hubungan Harga Jual dan Luas Lantai Pertama")
ggplot(Dataset, aes(x = X2ndFlrSF, y = SalePrice)) +
  geom_point()+ ggtitle("Hubungan Harga Jual dan Luas Lantai Kedua")
ggplot(Dataset, aes(x = FullBath, y = SalePrice)) +
  geom_point()+ ggtitle("Hubungan Harga Jual dan Kamar Mandi di Atas Rata-rata")
ggplot(Dataset, aes(x = YearBuilt, y = SalePrice)) +
  geom_point() + ggtitle("Hubungan Harga Jual dan Tahun Pembangunan")
ggplot(Dataset, aes(x = YearRemodAdd, y = SalePrice)) +
  geom_point()+ ggtitle("Hubungan Harga Jual dan Tahun Renovasi")

# Detect outliers in Y
boxplot(Dataset$SalePrice)

# Delete outliers
Dataset <- subset(Dataset, SalePrice <=300000)
boxplot(Dataset$SalePrice)

# set sampling seed
set.seed(123)
# specify 60/40 split
data_sample <- sample(c(TRUE, FALSE), nrow(Dataset), replace = T, prob = c(0.6, 0.4))

# subset data points into train and test sets
train <- Dataset[data_sample, ]
test <- Dataset[!data_sample, ]

## Multiple Linear Regression
model <- lm(SalePrice ~ LotArea+OverallQual+TotalBsmtSF+X1stFlrSF+
              X2ndFlrSF+FullBath+YearBuilt+YearRemodAdd, data = train)
summary(model)

model_2 <- lm(SalePrice ~ LotArea+OverallQual+TotalBsmtSF+X1stFlrSF+
                X2ndFlrSF+YearBuilt+YearRemodAdd, data = train)
summary(model_2)

model_3 <- lm(SalePrice ~ LotArea+TotalBsmtSF+X1stFlrSF+
                X2ndFlrSF+YearBuilt+YearRemodAdd, data = train)
summary(model_3)

# compute r-squared below
rsq_model <- summary(model)$r.squared
rsq_model2 <- summary(model_2)$r.squared
rsq_model3 <- summary(model_3)$r.squared

rsq_model
rsq_model2
rsq_model3

# define best_fit below
best_fit <- rsq_model

Predict_value_1 <- predict(model,newdata = test)
Predict_value_1
Predict_value_2 <- predict(model,newdata = test)
Predict_value_2
Predict_value_3 <- predict(model,newdata = test)
Predict_value_3

# MAPE
MAPE <- function(actual,predict){
  mean(abs(actual-predict)/predict)*100
}
Mape_model_1 <- MAPE(test$SalePrice,Predict_value_1)
Mape_model_2 <- MAPE(test$SalePrice,Predict_value_2)
Mape_model_3 <- MAPE(test$SalePrice,Predict_value_3)
Mape_model_1
Mape_model_2
Mape_model_3

# RMSE
RMSE <- function(actual,predict){
  sqrt(mean((actual-predict)^2))
}
RMSE_model_1 <- RMSE(test$SalePrice,Predict_value_1)
RMSE_model_2 <- RMSE(test$SalePrice,Predict_value_2)
RMSE_model_3 <- RMSE(test$SalePrice,Predict_value_3)
RMSE_model_1
RMSE_model_2
RMSE_model_3

# Tree-based Classification
# Decision Tree
library(caret)
library(DescTools)
library(rpart)
library(rpart.plot)

SalePrice_Average <- mean(Dataset$SalePrice,na.rm = TRUE)
Dataset$SalePrice <- ifelse(Dataset$SalePrice > SalePrice_Average,"Mahal","Murah")
Dataset$SalePrice <- as.factor(Dataset$SalePrice)

train$SalePrice <- ifelse(train$SalePrice > SalePrice_Average,"Mahal","Murah")
train$SalePrice <- as.factor(train$SalePrice)

test$SalePrice <- ifelse(test$SalePrice > SalePrice_Average,"Mahal","Murah")
test$SalePrice <- as.factor(test$SalePrice)

plot(x = Dataset$SalePrice,
     main = "Kategori Harga")

summary(Dataset)

PlotMiss(Dataset)

set.seed(831)

Dataset.rpart <- rpart(formula = SalePrice ~ ., # Y ~ all other variables in dataframe
                       data = train, # include only relevant variables
                       method = "class") # classification
Dataset.rpart

prp(x = Dataset.rpart, # rpart object
    extra = 2) # include proportion of correct predictions

base.trpreds <- predict(object = Dataset.rpart, # DT model
                        newdata = train,type = "class")  # training data

DT_train_conf <- confusionMatrix(data = base.trpreds, # predictions
                                 reference = train$SalePrice, # actual
                                 positive = "Mahal",
                                 mode = "everything")
DT_train_conf

base.tepreds <- predict(object = Dataset.rpart, # DT model
                        newdata = test, # testing data
                        type = "class")

DT_test_conf <- confusionMatrix(data = base.tepreds, # predictions
                                reference = test$SalePrice, # actual
                                positive = "Mahal",
                                mode = "everything")
DT_test_conf

cbind(Training = DT_train_conf$overall,
      Testing = DT_test_conf$overall)

cbind(Training = DT_train_conf$byClass,
      Testing = DT_test_conf$byClass)

plotcp(x = Dataset.rpart)
printcp(x = Dataset.rpart)

min_cp <- Dataset.rpart$cptable[which.min(Dataset.rpart$cptable[,"xerror"]),"CP"]
min_cp

pruneTree <- prune(tree = Dataset.rpart, # original DT object
                   cp = min_cp) # optimal cp value

prp(x = pruneTree, # prune object
    extra = 2) # include correct predictions on terminal nodes

tune.trpreds <- predict(object = pruneTree,
                        newdata = train,
                        type = "class")

DT_trtune_conf <- confusionMatrix(data = tune.trpreds, # predictions
                                  reference = train$SalePrice, # actual
                                  positive = "Mahal",
                                  mode = "everything")
DT_trtune_conf
tune.tepreds <- predict(object = pruneTree,
                        newdata = test,
                        type = "class")

DT_tetune_conf <- confusionMatrix(data = tune.tepreds, # predictions
                                  reference = test$SalePrice, # actual
                                  positive = "Mahal",
                                  mode = "everything")
DT_tetune_conf

cbind(Training = DT_trtune_conf$overall,
      Testing = DT_tetune_conf$overall)
cbind(Training = DT_trtune_conf$byClass,
      Testing = DT_tetune_conf$byClass)

# Random Forest Classification
##Modeling train Data (default parameter)
library(randomForest)

set.seed(222)
rf_model1<-randomForest(SalePrice~., data = train)
rf_model1

##Predict (default parameter)
library(caret)

p1_test <- predict(rf_model1, newdata = test)
p1_test_cm<-confusionMatrix(p1_test, test$SalePrice)
p1_test_cm

#Random Search CV
set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        search = "random")
rf_random <- train(SalePrice~.,
                   data = Dataset,
                   method = "rf",
                   metric = "Accuracy",
                   tuneLength = 10,
                   trControl = control)
rf_random

#Grid Search CV
control <- trainControl(method ="repeatedcv",
                        number = 10,
                        repeats = 3,
                        search = "grid")
set.seed(222)
tunegrid <- expand.grid(.mtry=c(1:10))
rf_gridsearch <- train(SalePrice~.,
                       data = Dataset,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tunegrid,
                       trControl = control)
rf_gridsearch

##Modeling train Data (mtry=4)
library(randomForest)
set.seed(222)
rf_model2<-randomForest(SalePrice~., data = train, mtry=4)
rf_model2

##Predict (mtry=2)
library(caret)
p2_test <- predict(rf_model2, newdata = test)
p2_test_cm<-confusionMatrix(p2_test, test$SalePrice)
p2_test_cm

##Mengatasi spesificity yang rendah karena imbalance data
library(ROSE)

##OverSampling
table(train$SalePrice)

over <- ovun.sample(SalePrice~., data = train, method = "over", N=455*2)$data
table(over$SalePrice)

#Modeling train Data (oversampling)
library(randomForest)
set.seed(222)
rf_model2_over<-randomForest(SalePrice~., data = over, mtry=4)
rf_model2_over

#Predict (oversampling)
library(caret)
p2_test_over <- predict(rf_model2_over, newdata = test)
p2_test_over_cm<-confusionMatrix(factor(p2_test_over,levels = c("Mahal","Murah")), test$SalePrice)
p2_test_over_cm

##UnderSampling
table(train$SalePrice)
under <- ovun.sample(SalePrice~., data = train, method = "under", N=374*2)$data
table(under$SalePrice)

#Modeling train Data (undersampling)
library(randomForest)
set.seed(222)
rf_model2_under<-randomForest(SalePrice~., data = under, mtry=4)
rf_model2_under

#Predict (undersampling)
library(caret)
p2_test_under <- predict(rf_model2_under, newdata = test)
p2_test_under_cm<-confusionMatrix(factor(p2_test_under,levels = c("Mahal","Murah")), test$SalePrice)
p2_test_under_cm

##SMOTE
table(train$SalePrice)

library(performanceEstimation)

smote <- smote(SalePrice~. , data =  train, perc.over = 10, perc.under = 10)
table(smote$SalePrice)

#Modeling train Data (smote)
library(randomForest)
set.seed(222)
rf_model2_smote<-randomForest(SalePrice~., data = smote, mtry=4)
rf_model2_smote

#Predict (smote)
library(caret)
p2_test_smote<- predict(rf_model2_smote, newdata = test)
p2_test_smote_cm<-confusionMatrix(p2_test_smote, test$SalePrice)
p2_test_smote_cm

library(pROC)
auc<- roc(test$SalePrice, factor(p2_test, ordered =  TRUE))
auc

auc_over <- roc(test$SalePrice, factor(p2_test_over, ordered =  TRUE))
auc_over

auc_under <- roc(test$SalePrice, factor(p2_test_under, ordered =  TRUE))
auc_under

auc_smote <- roc(test$SalePrice, factor(p2_test_smote, ordered =  TRUE))
auc_smote

nama_metode<-c("Tanpa Handling", "OverSampling", "UnderSampling", "SMOTE")
accuracy <- c(p2_test_cm$overall[1], p2_test_over_cm$overall[1], p2_test_under_cm$overall[1], p2_test_smote_cm$overall[1])
sensitivity <- c(p2_test_cm$overall[2], p2_test_over_cm$overall[2], p2_test_under_cm$overall[2], p2_test_smote_cm$overall[2])
specitifity <- c(p2_test_cm$overall[3], p2_test_over_cm$overall[3], p2_test_under_cm$overall[3], p2_test_smote_cm$overall[3])
auc <- c(auc$auc, auc_over$auc, auc_under$auc, auc_smote$auc)
data.frame(nama_metode, accuracy, sensitivity, specitifity, auc)
