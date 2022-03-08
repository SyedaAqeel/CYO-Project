#CYO PROJECT
#####################################################################
#Maternal Health Risk Prediction
#####################################################################


# Install all required libraries 


if(!require(readr)) install.packages("readr") 
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(caret)) install.packages("caret") 
if(!require(dplyr)) install.packages("dplyr") 
if(!require(matrixStats)) install.packages("matrixStats") 
if(!require(xgboost)) install.packages("xgboost") 
if(!require(Ckmeans.1d.dp)) install.packages("Ckmeans.1d.dp") 
if(!require(ggridges)) install.packages("ggridges") 
if(!require(patchwork)) install.packages("patchwork") 
if(!require(corrplot)) install.packages("corrplot") 
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(GGally)) install.packages("GGally")



# load all required libraries 

library(readr)
library(tidyverse)
library(caret)
library(dplyr)
library(matrixStats)
library(xgboost)
library(Ckmeans.1d.dp)
library(ggridges)
library(patchwork)
library(corrplot)
library(ggplot2)
library(GGally)

# load Maternal Health Risk data set from UCI Machine Learning Repository 

Data <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00639/Maternal%20Health%20Risk%20Data%20Set.csv")


# Number of rows and columns 

dim(Data)

# view first six entries of Data set  
head(Data)

# check missing values in Data set
sapply(Data, function(x) sum(is.na(x)))


# split the "Data set" in to train set(dat) and test set(Validation)

set.seed(1, sample.kind="Rounding")

Validation_index <- createDataPartition(y = Data$RiskLevel, times = 1, p = 0.1, list = FALSE)

dat <- Data[-Validation_index,]
dat_x <- dat[,-7]
dat_y <- dat$RiskLevel


Validation <- Data[Validation_index,]
val_x <- Validation[,-7]
val_y <- Validation$RiskLevel



# Further split "dat set" in to train set and test set 

test_index <- createDataPartition(y = dat_y, times = 1, p = 0.1, list = FALSE)

train <- dat[-test_index,]
train_x <- train[,-7]
train_y <- train$RiskLevel


test <- dat[test_index,]
test_x <- test[,-7]
test_y <- test$RiskLevel



# Number of rows and columns in dat 

dim(dat)


# Structure of dat
str(dat)



##########################################
# DATA EXPLORATION & VISUALIZATION 
##########################################

#-------------------------------------------
# explore RiskLevel 
#-------------------------------------------

# type of RiskLevel
  
levels(factor(dat$RiskLevel))


# count of each RiskLevel 

table(dat$RiskLevel) 


# Visualize RisKLevel distribution 

dat %>% ggplot(aes(RiskLevel, 
                   fill = RiskLevel)) +  geom_bar(width = 1) + coord_polar(theta = "x")



#----------------------------------------------
# explore Age
#----------------------------------------------

# summary statistics of Age Distribution
  
summary(dat$Age)


# plot Bar Graph

A1 <- dat %>%  ggplot( aes(Age,  fill = RiskLevel)) +
  geom_bar() + facet_grid(RiskLevel~ . )


# plot Density Plot

A2 <- dat %>%  ggplot( aes(Age,  RiskLevel, fill = RiskLevel)) +
  geom_density_ridges(alpha = 0.2) 

# plot Boxplot
A3 <- dat %>%  ggplot( aes(RiskLevel, Age,  fill = RiskLevel)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())


# display these plots together 

( A1 | (A2 / A3) ) + plot_annotation(title = "Age Distribution")



#----------------------------------------------------
# explore SystolicBP
#----------------------------------------------------

  
# summary statistics of SystolicBP distribution
  
summary(dat$SystolicBP)


# plot Bar Graph

S1 <- dat %>%  ggplot( aes(SystolicBP,  fill = RiskLevel)) +
  geom_bar() + facet_grid(RiskLevel~ . )


# plot Density Plot

S2 <- dat %>%  ggplot( aes(SystolicBP, RiskLevel, fill = RiskLevel)) +
  geom_density_ridges(alpha = 0.2) 


# plot  Boxplot 

S3 <- dat %>%  ggplot( aes(RiskLevel, SystolicBP,  fill = RiskLevel)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())


# display these plots together

(S1 | (S2 / S3)) + plot_annotation(title = "SystolicBP Distribution")




#------------------------------------------------------
# explore DiastolicBP
#------------------------------------------------------

# summary statistics of DiastolicBP Distribution
  
summary(dat$DiastolicBP)


# plot Bar Graph

D1 <- dat %>%  ggplot( aes(DiastolicBP,  fill = RiskLevel)) +
  geom_bar() + facet_grid(RiskLevel~ . )


# plot Density plot


D2 <- dat %>%  ggplot( aes(DiastolicBP, RiskLevel, fill = RiskLevel)) +
  geom_density_ridges(alpha = 0.2) 


# plot Boxplot 

D3 <- dat %>%  ggplot( aes(RiskLevel, DiastolicBP,  fill = RiskLevel)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())


# display these plots together 

(D1 | (D2 / D3)) + plot_annotation(title = "DiastolicBP Distribution")


#------------------------------------------
# explore BS
#-------------------------------------------

# summary of BS

summary(dat$BS)


# plot Bar Graph

B1 <- dat %>%  ggplot( aes(BS,  fill = RiskLevel)) +
  geom_bar(width = 0.1) + 
  facet_grid(RiskLevel~ ., scales = "free")


# plot Density Plot

B2 <- dat %>%  ggplot( aes(BS, RiskLevel,  fill = RiskLevel)) +
  geom_density_ridges(alpha = 0.2) 


# plot Boxplot

B3 <- dat %>%  ggplot( aes(RiskLevel, BS,  fill = RiskLevel)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())


# display these plots together

( B1 | (B2 / B3)) + plot_annotation(title = "BS Distribution")



#-----------------------------------------------------
# explore BodyTemp
#----------------------------------------------------

# summary statistics of BodyTemp Distribution 
  
summary(dat$BodyTemp)


# plot Bar Graph

T1 <- dat %>%  ggplot( aes(BodyTemp,  fill = RiskLevel)) +
  geom_bar() + facet_grid(RiskLevel~ . )


# plot Density Plot

T2 <-  dat %>%  ggplot( aes(BodyTemp, RiskLevel, fill = RiskLevel)) +
  geom_density_ridges(alpha = 0.2)


# plot Boxplot

T3 <- dat %>%  ggplot( aes(RiskLevel, BodyTemp,  fill = RiskLevel)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())


# display these plots together 

(T1 | (T2 / T3)) + plot_annotation(title = "BodyTemp Distribution")



#-----------------------------------------------------
# explore HeartRate 
#------------------------------------------------------

  
# summary statistics of HeartRate Distribution
  
summary(dat$HeartRate)

# plot Bar Graph

H1 <- dat %>%  ggplot( aes(HeartRate,  fill = RiskLevel)) +
  geom_bar() + facet_grid(RiskLevel~ . )


# plot Density Plot

H2 <- dat %>%  ggplot( aes(HeartRate, RiskLevel,  fill = RiskLevel)) +
  geom_density_ridges(alpha = 0.2) 

# plot Boxplot

H3 <- dat %>%  ggplot( aes(RiskLevel, HeartRate,  fill = RiskLevel)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# display these plots together 
(H1 | (H2 / H3)) + plot_annotation(title = "HeartRate Distribution")





###################
#Heat map
###################

# convert into matrix 

m <- as.matrix(dat_x)


# center entries 

x_centered <- sweep(m, 2, colMeans(m))


# scale entries 

x_scaled <- sweep(x_centered, 2, colSds(x_centered), FUN = "/")


# calculate distance 
d <- dist(x_scaled)


# plot heatmap 
heatmap(as.matrix(d), labRow = NA, labCol = NA)



##################################################
# Correlation Heat map & Hierarchical Clustering
##################################################



# determine correlation between predictors 

x <- cor(dat_x, method = "spearman")

# plot correlation matrix 
# Hierarchical Clustering is based on average distance of features

corrplot(x, method = 'shade', order = 'hclust', addrect = 2, hclust.method = "average",
         addCoef.col ='black', col = COL2('PiYG', 10),tl.col = 'black' )



##################
# Scatter Matrix  
##################


# plot scatter matrix 

ggpairs(dat_x,
        aes(color = dat$RiskLevel ,  
            alpha = 0.5), upper = list(continuous = wrap("cor", method= "spearman"))) 




##############################
#Principal Component Analysis
##############################


x <- as.matrix(dat_x)


# center entries 

x_centered <- sweep(x, 2, colMeans(x))


# scale entries 

x_scaled <- sweep(x_centered, 2, colSds(x_centered), FUN = "/")


# calculate distance

d <- dist(x_scaled)


# visualize distance 

image(as.matrix(d), col = rev(RColorBrewer::brewer.pal(9, "PiYG")))


# perform principal component analysis

pca <- prcomp(x_scaled)

summary(pca)


# plot Standard Deviation 

plot(pca$sdev, xlab = "PC")


# calculate  Variance Explained 

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))


# plot Varaince Explained 

plot(var_explained, xlab = "PC") 


 
# Plot PC1 & PC2  

data.frame(pca$x[,1:2], type = dat_y) %>%
  ggplot(aes(PC1, PC2, fill = type)) +
  geom_point(cex=3, pch=21)+
  ggtitle(" PC1 & PC2 Plot") + 
  coord_fixed(ratio = 1)


# Plot Principal Component Boxplot 

data.frame(type = dat_y, pca$x) %>%
      gather(key = "PC", value = "value", -type) %>%
      ggplot(aes(PC, value, fill = type)) + 
      geom_boxplot() +
      ggtitle("Principal Component Boxplot ") 


#-------------------------------------------------------
# Model 1:  Quadratic Discriminant Analysis (QDA)
#-------------------------------------------------------
  
# train model
  
train_qda <- train(train_x, train_y, method = "qda")


# calculate predictions 
qda_preds <- predict(train_qda, test_x)


# Confusion Matrix and Statistics

confusionMatrix(data = qda_preds, reference = factor(test_y))


# calculate accuracy

qda_result <- mean(qda_preds == factor(test_y))


result <- data.frame(Model = "Quadratic Discriminant Analysis (QDA)", Accuracy = qda_result )

result


#---------------------------------------------------------
# Model 2: Linear Discriminant Analysis (LDA)
#---------------------------------------------------------

# train model
  
train_lda <- train(train_x, train_y, method = "lda")


# calculate predictions

lda_preds <- predict(train_lda, test_x)

# Confusion Matrix and Statistics

confusionMatrix(data = lda_preds, reference = factor(test_y))

# calculate accuracy 

lda_result <- mean(lda_preds == factor(test_y))

result <- bind_rows(result, tibble(Model = "Linear Discriminant Analysis (LDA)",
                                   Accuracy = lda_result))
result


#-------------------------------------------------------
# Model 3: K - Nearest Neighbors 
#-------------------------------------------------------

# define tuning parameter k
  
tuning <- data.frame(k = seq(3,27,2))

# train model

train_knn <- train(train_x, train_y, method = "knn", 
                   tuneGrid = tuning)
# find best tune 
train_knn$bestTune

# plot accuracy for each k
ggplot(train_knn, highlight = TRUE) + ggtitle("K - Nearest Neighbors Accuracy") + theme_bw()

# plot error bar for each k
train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD)) + ggtitle("K - Nearest Neighbors Error Bar") +
                    theme_bw()

# training set outcome distribution of final model

train_knn$finalModel

# calculate predictions 

knn_preds <- predict(train_knn, test_x )


# Confusion Matrix and Statistics
confusionMatrix(knn_preds, factor(test_y))



# calculate accuracy 
knn_result <- mean(knn_preds == factor(test_y))

result <- bind_rows(result, tibble(Model = "K - Nearest Neighbors",
                                   Accuracy = knn_result))
 

result


#---------------------------------------------------------------
#Model 4: Random Forest  
#---------------------------------------------------------------
# define tuning parameter - number of variables randomly sampled as candidates at each split 

tuning <- data.frame(mtry = c(3, 5, 7))

# train model
train_rf <- train(train_x, train_y, method = "rf", tuneGrid = tuning,
             importance = TRUE)


# find best tune 

train_rf$bestTune


# calculate predictions 

rf_preds <- predict(train_rf, test_x)

#Confusion Matrix and Statistics

confusionMatrix(rf_preds, factor(test_y))

# calculate accuracy 
rf_result <- mean(rf_preds == factor(test_y))


# evaluate Variable Importance 

varImp(train_rf)

# plot Variable Importance 

plot(varImp(train_rf))

result <- bind_rows(result, tibble(Model = "Random Forrest",
                                   Accuracy = rf_result))
result


#-----------------------------------------------------------
# Model 5: Extreme Gradient Boosting (xgboost)
#-----------------------------------------------------------

# convert train data in to required class
set.seed(1, sample.kind="Rounding")
train_x <- as.matrix(train_x)
train_y <- as.integer(factor(train$RiskLevel)) - 1

# convert test data in to required class

test_x <- as.matrix(test_x)
test_y <- as.integer(factor(test$RiskLevel)) - 1


#  define train data input as dense matrix

xgb_train <- xgb.DMatrix(data = train_x, label = train_y)


# define test data input as dense matrix

xgb_test <- xgb.DMatrix(data = test_x, label = test_y)


# tune parameters
# gbtree - tree is grown one after other and attempts to reduce misclassification rate in subsequent iterations
# objective is multiclassification using softmax objective which returns predicted class probabilities
# evaluate model's accuracy with  multiclass logloss function

xgb_params <- list(
  booster = "gbtree",
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = length(levels(factor(train$RiskLevel))))

# train model

xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  verbose = 1,
  nrounds = 200)


# calculate predictions 

xgb_preds <- predict(xgb_model, as.matrix(test_x), reshape = TRUE)

xgb_preds <- as.data.frame(xgb_preds)

colnames(xgb_preds) <- levels(factor(test$RiskLevel))


# determine Predicted Risk

Predicted_Risk <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])

# determine Actual Risk

Actual_Risk <- levels(factor(test$RiskLevel))[test_y + 1]


# Confusion Matrix and Statistics

confusionMatrix(factor(Predicted_Risk), factor(Actual_Risk))

# calculate accuracy 

xgb_result <- mean(Predicted_Risk == Actual_Risk)


# determine feature importance 

importance_matrix <- xgb.importance(feature_names = colnames(xgb_train), model = xgb_model)

importance_matrix


# plot feature importance 

xgb.ggplot.importance(importance_matrix,measure = "Frequency",rel_to_first = TRUE)


result <- bind_rows(result, tibble(Model = "Extreme Gradient Boosting (xgboost)",
                                   Accuracy = xgb_result))
result



#-------------------------------------------------
# Final Model: Extreme Gradient Boosting (xgboost)
#-------------------------------------------------

# convert dat data in to required class  
  
dat_x <- as.matrix(dat_x)
dat_y <- as.integer(factor(dat$RiskLevel)) - 1



# convert Validation data in to required class

val_x <- as.matrix(val_x)
val_y <- as.integer(factor(Validation$RiskLevel)) - 1


# define dat data input as dense matrix

xgb_train <- xgb.DMatrix(data = dat_x, label = dat_y)

# define Validation data input as dense matrix

xgb_test <- xgb.DMatrix(data = val_x, label = val_y)

# tune parameters
# gbtree - tree is grown one after other and attempts to reduce misclassification rate in subsequent iterations
# objective is multiclassification using softmax objective which returns predicted class probabilities
# evaluate model's accuracy with  multiclass logloss function

xgb_params <- list(
  booster = "gbtree",
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = length(levels(factor(dat$RiskLevel))))

# train model

xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  verbose = 1,
  nrounds = 200)


# calculate predictions 

xgb_preds <- predict(xgb_model, as.matrix(val_x), reshape = TRUE)

xgb_preds <- as.data.frame(xgb_preds)

colnames(xgb_preds) <- levels(factor(Validation$RiskLevel))

# determine Predicted Risk

Predicted_Risk <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])

# determine Actual Risk

Actual_Risk <- levels(factor(Validation$RiskLevel))[val_y + 1]


# Confusion Matrix and Statistics

confusionMatrix(factor(Predicted_Risk), factor(Actual_Risk))


# calculate accuracy 

Final_xgb_result <- mean(Predicted_Risk == Actual_Risk)


# determine feature importance

importance_matrix <- xgb.importance(feature_names = colnames(xgb_train), model = xgb_model)

importance_matrix


# plot feature importance 

xgb.ggplot.importance(importance_matrix,measure = "Frequency",rel_to_first = TRUE)


result <- bind_rows(result, tibble(Model = "Final Model - Extreme Gradient Boosting (xgboost)",
                                   Accuracy = Final_xgb_result))
result

