# Clear the environment 
rm(list=ls())

# Calling the libraries
library(DescTools)
library(ggplot2)
library(dplyr)
library(readxl)
library(Hmisc)
library(pastecs)
library(corrplot)

#############################################################################
# DATA LOADING
#############################################################################

# We have a xlsx file with multiple sheets so we have to join them into a single dataframe
setwd("/Users/andreiblindu/Desktop/Università/Laurea Magistrale/Corsi/2° anno/1° semestre/Financial Data Science/Project/credit-risk-analysis/")
getwd()
multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

Complete_Data <- multiplesheets("Complete_Data.xlsx")

Dataset <- rbind(Complete_Data$`1-20k`, Complete_Data$`20k-40k`)
Dataset <- rbind(Dataset, Complete_Data$`40k-60k`)
Dataset <- rbind(Dataset, Complete_Data$`60k-80k`)
Dataset <- rbind(Dataset, Complete_Data$`80k-100k`)
Dataset <- rbind(Dataset, Complete_Data$`100k-121k`)

# View dataset
View(Dataset)
# View summary and column names
summary(Dataset)
colnames(Dataset)

#############################################################################
# DATA PRE-PROCESSING
#############################################################################
# Remove missing values with na.omit
data <- na.omit(Dataset)
colnames(data)
summary(data)

# We want the MScores to be a binary variables with
# value 0 for the levels from A to B (AAA, AA, A, BBB, BB, B)
# value 1 for the levels from C to D (CCC,CC, C and D)
unique(data$MScore.2020) # first things first, see all the possible values of the column
# binarize
data1 <- data
data1$MScore.2020 <- ifelse(data$MScore.2020 >= "C", 1, 0)
data1$MScore.2019 <- ifelse(data$MScore.2019 >= "C", 1, 0)
data1$MScore.2018 <- ifelse(data$MScore.2018 >= "C", 1, 0)
data1$MScore.2017 <- ifelse(data$MScore.2017 >= "C", 1, 0)
data1$MScore.2016 <- ifelse(data$MScore.2016 >= "C", 1, 0)
data1$MScore.2015 <- ifelse(data$MScore.2015 >= "C", 1, 0)

colnames(data1)

column_list <- c( 'Turnover_Prev', 
                  'EBIT_Prev', 
                  'PLTax_Prev', 
                  'Region', 
                  'Country', 
                  'NACE code',
                  'Sector 1',
                  'Sector 2',
                  'Leverage_Prev', 
                  'ROE_Prev', 
                  'TAsset_Prev',
                  'MScore_Curr',
                  'MScore_Prev')

df <- data1

# Rename the columns
names(df)[names(df) == "Turnover.2019"] <- "Turnover_Prev"
names(df)[names(df) == "EBIT.2019"] <- "EBIT_Prev"
names(df)[names(df) == "PLTax.2019"] <- "PLTax_Prev"
names(df)[names(df) == "MScore.2020"] <- "MScore_Curr"
names(df)[names(df) == "MScore.2019"] <- "MScore_Prev"
names(df)[names(df) == "Leverage.2019"] <- "Leverage_Prev"
names(df)[names(df) == "ROE.2019"] <- "ROE_Prev"
names(df)[names(df) == "TAsset.2019"] <- "TAsset_Prev"

# Keep only the columns we need
df <- df[column_list]
colnames(df)

for (y in 2019:2016) {
  d <- data1
  # Rename the columns
  names(d)[names(d) == paste("Turnover.", y-1, sep = "")] <- "Turnover_Prev"
  names(d)[names(d) == paste("EBIT.", y-1, sep = "")] <- "EBIT_Prev"
  names(d)[names(d) == paste("PLTax.", y-1, sep = "")] <- "PLTax_Prev"
  names(d)[names(d) == paste("MScore.", y, sep = "")] <- "MScore_Curr"
  names(d)[names(d) == paste("MScore.", y-1, sep = "")] <- "MScore_Prev"
  names(d)[names(d) == paste("Leverage.", y-1, sep = "")] <- "Leverage_Prev"
  names(d)[names(d) == paste("ROE.", y-1, sep = "")] <- "ROE_Prev"
  names(d)[names(d) == paste("TAsset.", y-1, sep = "")] <- "TAsset_Prev"
  
  # Keep only the columns we need
  d <- d[column_list]
  
  # Bind dataframe to the one from the previous year
  df <- rbind(df, d)
  
  nrow(df)
}
nrow(df)
colnames(df)

#############################################################################
# DATA EXPLORATION AND PRE-PROCESSING
#############################################################################

# See the distribution of the values for the MScore_Curr
# We can notice that the values are very imbalanced since there's a 89% of 0 and 11% of 1
hist(df$MScore_Curr)
table(df$MScore_Curr)
prop.table(table(df$MScore_Curr))

# Show a Map with the distribution of credit risk across the different countries
country_score_count <- table(df$Country, df$MScore_Curr)
country_score_count
france_ratio <- country_score_count[1,2] / (country_score_count[1,1] + country_score_count[1,2])  
germany_ratio <- country_score_count[2,2] / (country_score_count[2,1] + country_score_count[2,2])  
italy_ratio <- country_score_count[3,2] / (country_score_count[3,1] + country_score_count[3,2])  
spain_ratio <- country_score_count[4,2] / (country_score_count[4,1] + country_score_count[4,2])  
#https://stackoverflow.com/questions/30076553/r-choropleth-maps-choroplethr-package
#https://stackoverflow.com/questions/58961926/how-to-select-specific-countries-to-make-a-choropleth-map
#install.packages("choroplethr")
#install.packages("choroplethrMaps")
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)

data_iso = data.frame(region=c("italy", "germany", "france", "spain"),
                      value = c(italy_ratio, germany_ratio, france_ratio, spain_ratio))

gg <- country_choropleth(data_iso, legend="%",num_colors=1,zoom=data_iso$region)
gg <- gg + xlim(-31.266001, 39.869301)
gg <- gg + ylim(27.636311, 81.008797)
gg <- gg + coord_map("lambert", lat0=27.636311, lat1=81.008797)
gg

# Use One-hot encoding for the Country column
df$Italy <- ifelse(df$Country == "Italy", 1, 0)
df$France <- ifelse(df$Country == "France", 1, 0)
df$Spain <- ifelse(df$Country == "Spain", 1, 0)
df$Germany <- ifelse(df$Country == "Germany", 1, 0)
# Check if the countries are balanced, they are not
prop.table(table(df$Italy))
prop.table(table(df$France))
prop.table(table(df$Spain))
prop.table(table(df$Germany))

# Group NACE codes by the different sections and encode them as One-hot 
# Source: https://nacev2.com/it
# 0100-0322 -> A
# 0500-0990 -> B
# 1000-3320 -> C
# 3500-3530 -> D
# 3600-3900 -> E
# 4100-4399 -> F
# 4500-4799 -> G
# 4900-5320 -> H
# 5500-5630 -> I
# 5800-6399 -> J
# 6400-6630 -> K
# 6800-6832 -> L
# 6900-7500 -> M
# 7700-8299 -> N
# 8400-8430 -> O
# 8500-8560 -> P
# 8600-8899 -> Q
# 9000-9329 -> R
# 9400-9609 -> S
# 9700-9820 -> T
# >= 9900 -> U
df$NaceA <- ifelse(df$`NACE code` < 0500, 1, 0)
df$NaceB <- ifelse(df$`NACE code` >= 0500 & df$`NACE code` < 1000, 1, 0)
df$NaceC <- ifelse(df$`NACE code` >= 1000 & df$`NACE code` < 3500, 1, 0)
df$NaceD <- ifelse(df$`NACE code` >= 3500 & df$`NACE code` < 3600, 1, 0)
df$NaceE <- ifelse(df$`NACE code` >= 3600 & df$`NACE code` <= 3900, 1, 0)
df$NaceF <- ifelse(df$`NACE code` >= 4100 & df$`NACE code` <= 4399, 1, 0)
df$NaceG <- ifelse(df$`NACE code` >= 4500 & df$`NACE code` <= 4799, 1, 0)
df$NaceH <- ifelse(df$`NACE code` >= 4900 & df$`NACE code` < 5500, 1, 0)
df$NaceI <- ifelse(df$`NACE code` >= 5500 & df$`NACE code` < 5800, 1, 0)
df$NaceJ <- ifelse(df$`NACE code` >= 5800 & df$`NACE code` < 6400, 1, 0)
df$NaceK <- ifelse(df$`NACE code` >= 6400 & df$`NACE code` < 6800, 1, 0)
df$NaceL <- ifelse(df$`NACE code` >= 6800 & df$`NACE code` < 6900, 1, 0)
df$NaceM <- ifelse(df$`NACE code` >= 6900 & df$`NACE code` <= 7500, 1, 0)
df$NaceN <- ifelse(df$`NACE code` >= 7700 & df$`NACE code` < 8300, 1, 0)
df$NaceO <- ifelse(df$`NACE code` >= 8400 & df$`NACE code` < 8500, 1, 0)
df$NaceP <- ifelse(df$`NACE code` >= 8500 & df$`NACE code` < 8600, 1, 0)
df$NaceQ <- ifelse(df$`NACE code` >= 8600 & df$`NACE code` < 8900, 1, 0)
df$NaceR <- ifelse(df$`NACE code` >= 9000 & df$`NACE code` < 9400, 1, 0)
df$NaceS <- ifelse(df$`NACE code` >= 9400 & df$`NACE code` < 9700, 1, 0)
df$NaceT <- ifelse(df$`NACE code` >= 9700 & df$`NACE code` < 9900, 1, 0)
df$NaceU <- ifelse(df$`NACE code` >= 9900, 1, 0)

colnames(df)

# Remove having SD equal to 0 (for example NACE columns that never occur)
cols_sd_zero <- list()
for (i in 1:length(colnames(df))) {
  sd_ <- sd(df[[i]])
  if ( sd_ == 0 & !is.na(sd_) ) {
    cols_sd_zero <- append(cols_sd_zero, colnames(df)[i])
  }
}
cols_sd_zero
df <- df[, !names(df) %in% cols_sd_zero]
colnames(df)

# Keep only the useful variables, that are the numerical and One-hot encoded ones
# For now we don't consider the info about 
# "Company name", "Region", "Sector 1" and "Sector 2" since they don't bring useful info
# and also remove "Country" and "NACE code" columns since we already One-hot encoded them
df <- df[, !names(df) %in% c("Company name", "Region", "Sector 1", "Sector 2",
                             "Country", "NACE code")]
colnames(df)

# Normalize our data (only columns about Turnover, EBIT, PLTax, Leverage, ROE and TAsset)
# We apply min-max normalization to make them be between 0 and 1
for (col in colnames(df)) {
  if (startsWith(col, "Turnover") | 
      startsWith(col, "EBIT") |
      startsWith(col, "PLTax") |
      startsWith(col, "Leverage") |
      startsWith(col, "ROE") |
      startsWith(col, "TAsset")) 
  {
    df[col] = (df[col] - min(df[col])) / (max(df[col]) - min(df[col]))
    print(summary(df[col]))
  }
}

#############################################################################
# CORRELATIONS
#############################################################################
#?cor

# Check all correlations by using the cor function 
correlations <- cor(df, use = "complete.obs", method = "spearman")
correlations

# Visualize the correlation plot
corrplot(correlations, method="square")

# Compute partial correlations
#install.packages("ppcor")
library(ppcor)
#?pcor

partial_correlations <- pcor(df, method = "spearman")
rownames(partial_correlations$estimate) <- colnames(df)
colnames(partial_correlations$estimate) <- colnames(df)
partial_correlations$estimate

# Visualize the partial correlation plot
corrplot(partial_correlations$estimate, method = "square")

#############################################################################
# LOGISTIC REGRESSION MODEL
#############################################################################

# We divide our dataset in training and test set by random sampling
# We put 80% of our data in train and the remaining in test
# Set a random seed so that your results can be reproduced

## MScore inbalanced and distrubuted as in the full dataset in train and test
set.seed(300)
perc        <- 0.8
n_train     <- round(perc*nrow(df))
n_test  <- nrow(df) - n_train
data_sample <- df[sample(nrow(df)), ]          
df.train  <- data_sample[1:n_train, ]              
df.test   <- data_sample[(n_train+1):nrow(data_sample), ]    
# Check if the response variable MSCore_Curr is equally distributed in the
# training and test set
prop.table(table(df.train$MScore_Curr))
hist(df.train$MScore_Curr)
prop.table(table(df.test$MScore_Curr))
hist(df.test$MScore_Curr)

## MScore balanced: to avoid the classifier being biased towards one class we
## sample in such a way that we have an equal number of samples with MScore=1 and
## MScore=0 in both train and test
## https://www.rdocumentation.org/packages/ROSE/versions/0.0-4/topics/ovun.sample
## https://www.r-bloggers.com/2021/05/class-imbalance-handling-imbalanced-data-in-r/
#install.packages("ROSE")
library(ROSE)
df.train <- ovun.sample(MScore_Curr~., data = df.train, method = "both", N = n_train)$data
df.test <- ovun.sample(MScore_Curr~., data = df.test, method = "both", N = n_test)$data 
# Check if the response variable MScore_Curr is balanced in class 1 and 0 
# and equally distributed in the training and test set
table(df.train$MScore_Curr)
prop.table(table(df.train$MScore_Curr))
hist(df.train$MScore_Curr)
table(df.test$MScore_Curr)
prop.table(table(df.test$MScore_Curr))
hist(df.test$MScore_Curr)

## FUNCTIONS FOR MODEL EVALUATION
evaluate_model <- function(fit, df) {
  ## Get predicted default probabilities on both train and test set
  train_score <- predict(fit, type='response', df.train)
  test_score <- predict(fit, type='response', df.test)
  
  # Decide a cut-off and get predictions
  cut_off <- prop.table(table(df.train$MScore_Curr))[2]
  train_pred <- ifelse(train_score<=cut_off, 0, 1)
  test_pred <- ifelse(test_score<=cut_off, 0, 1)
  
  ## Train MSE
  mse_train <- mean((train_pred-df.train$MScore_Curr)^2)
  print(paste("Train MSE: ", mse_train))
  ## Test MSE
  mse_test <- mean((test_pred-df.test$MScore_Curr)^2)
  print(paste("Test MSE: ", mse_test))
  
  ## Train accuracy
  n_train <- nrow(df.train)
  train_correct <- ifelse(train_pred == df.train$MScore_Curr, 1, 0)
  train_acc <- sum(train_correct)/n_train
  print(paste("Train accuracy: ", train_acc))
  
  ## Test accuracy
  n_test <- nrow(df.test)
  test_correct <- ifelse(test_pred == df.test$MScore_Curr, 1, 0)
  test_acc <- sum(test_correct)/n_test
  print(paste("Test accuracy: ", test_acc))
  
  ##false positive rate
  n_neg <- nrow(df.test[df.test$MScore_Curr==0,])
  fp_flag <- ifelse(test_pred==1 & df.test$MScore_Curr==0, 1, 0)
  fpr <- sum(fp_flag)/n_neg #false positive rate
  print(paste("False positive rate: ", fpr))
  
  ##false negative rate
  n_pos <- nrow(df.test[df.test$MScore_Curr==1,])
  fn_flag <- ifelse(test_pred==0 & df.test$MScore_Curr==1, 1, 0)
  fnr <- sum(fn_flag)/n_pos #false negative rate
  print(paste("False negative rate: ", fnr))
  
  ##Precision
  tp_flag <- ifelse(test_pred==1 & df.test$MScore_Curr==1, 1, 0)
  fp_flag <- ifelse(test_pred==1 & df.test$MScore_Curr==0, 1, 0)
  precision <- sum(tp_flag) / (sum(tp_flag) + sum(fp_flag))
  print(paste("Precision: ", precision))
  
  ##sensitivity
  recall <- 1- fnr
  print(paste("Sensitivity (recall): ", recall))
  
  ##specificity
  print(paste("Specificity: ", 1- fpr))
  
  ##F1
  f1 <- 2*((precision*recall) / (precision + recall))
  print(paste("F1: ", f1))
}

# FULL MODEL

## For now let's try to use the model with all variables to see which coefficients
## are significant and how well it performs. We expect that not all variables will
## be useful to explain the response (as already seen from the correlations) and
## the full model is also likely to overfit and perform badly on the test set.
fit_full <- glm(MScore_Curr ~ ., data=df.train, family=binomial())
summary(fit_full)

## Odds ratios 
exp(coefficients(fit_full))

## Evaluate model
evaluate_model(fit_full, df)

# REDUCED MODEL

## By looking at the magnitude of the coefficients we can see that the Country and NACE
## are less important than the other features so we try to reduce the model
fit_red <- glm(MScore_Curr ~ MScore_Prev + Turnover_Prev + EBIT_Prev + PLTax_Prev + Leverage_Prev + ROE_Prev + TAsset_Prev, 
               data=df.train, family=binomial())
summary(fit_red)

## Odds ratios 
exp(coefficients(fit_red))

## Evaluate model
evaluate_model(fit_red, df)

# Calling the libraries
library(caret)
library(ROCR)
library(pROC) # package for the computation of the AUROC measure and the implementation of the DeLong test


## Get predicted default probabilities
score_full  <- predict(fit_full, type='response', df.test)
score_red <- predict(fit_red, type='response', df.test)

## Plot AUROC
perf_auroc_full <- performance(prediction(score_full, df.test$MScore_Curr),"auc")
auroc_full      <- as.numeric(perf_auroc_full@y.values)

perf_plot_full  <- performance(prediction(score_full,df.test$MScore_Curr),"tpr","fpr")

plot(perf_plot_full, main='ROC', col='blue',lwd=2)


## Compare AUROC
### note: in this case the two AUROCs are very close, so the two ROC curves are overlapping
perf_auroc_red <- performance(prediction(score_red, df.test$MScore_Curr),"auc")
auroc_red      <- as.numeric(perf_auroc_red@y.values)

perf_plot_red  <- performance(prediction(score_red, df.test$MScore_Curr),"tpr","fpr")


plot(perf_plot_full, col='blue', lwd=2) 
plot(perf_plot_red, add=TRUE, col='red', lwd=2) 
legend("right", legend=c("Full model", "Reduced model"), lty=(1:1), col=c("blue", "red"))

roc_full<-roc(df.test$MScore_Curr, score_full)
roc_red<-roc(df.test$MScore_Curr, score_red)


dl<-roc.test(roc_red, roc_full, method="delong") # DeLong test
dl

# Model that tries to predict MScore_Curr with only MScore_Prev
fit_red1 <- glm(MScore_Curr ~ MScore_Prev, 
                data=df.train, family=binomial())
summary(fit_red1)

evaluate_model(fit_red1, df)

# Model that tries to predict MScore_Curr with EBIT_Prev, PLTax_Prev, ROE_Prev
# We notice that this improves the recall by reducing the false negative rate
fit_red2 <- glm(MScore_Curr ~ EBIT_Prev + PLTax_Prev + ROE_Prev, 
                data=df.train, family=binomial())
summary(fit_red2)

evaluate_model(fit_red2, df)

# Compare AUROC
score_red_1 <- predict(fit_red1, type='response', df.test)
perf_auroc_red_1 <- performance(prediction(score_red_1, df.test$MScore_Curr),"auc")
auroc_red_1      <- as.numeric(perf_auroc_red_1@y.values)
perf_plot_red_1  <- performance(prediction(score_red_1, df.test$MScore_Curr),"tpr","fpr")

score_red_2 <- predict(fit_red2, type='response', df.test)
perf_auroc_red_2 <- performance(prediction(score_red_2, df.test$MScore_Curr),"auc")
auroc_red_2      <- as.numeric(perf_auroc_red_2@y.values)
perf_plot_red_2  <- performance(prediction(score_red_2, df.test$MScore_Curr),"tpr","fpr")

plot(perf_plot_full, col='blue', lwd=2) 
plot(perf_plot_red, add=TRUE, col='red', lwd=2) 
plot(perf_plot_red_1, add=TRUE, col='orange', lwd=2)
plot(perf_plot_red_2, add=TRUE, col='green', lwd=2)
legend("bottom", legend=c("Full model", "Reduced model", "~ MScore_Prev", "~ EBIT_Prev + PLTax_Prev + ROE_Prev"), lty=(1:1), col=c("blue", "red", "orange", "green"))


#############################################################################
# TREE MODELS
#############################################################################
library(rpart) 
library(partykit) 
library(rattle) 
library(rpart.plot) 
library(ROCR) 
library(randomForest) 
library(pROC) 

## CART Tree
## The CART algorithm for classification trees minimizes the Gini impurity in each group
fit_tree <- rpart(MScore_Curr ~ ., data=df.train, method = "class")

## Print tree detail
printcp(fit_tree)

## Plot the tree
plot(fit_tree, margin = 0.2, main="Tree: Recursive Partitioning")
text(fit_tree, cex=0.8) 

prp(fit_tree, type=2, extra=1,  main="Tree: Recursive Partitioning") # type=2 draws the split labels below the node labels
# extra=1 displays the number of observations that fall in the node 

fancyRpartPlot(fit_tree)

# Evaluate Tree performance
score_tree <- as.numeric(predict(fit_tree, type='class', df.test))
perf_auroc_tree <- performance(prediction(score_tree, df.test$MScore_Curr),"auc")
auroc_tree      <- as.numeric(perf_auroc_tree@y.values)
perf_plot_tree  <- performance(prediction(score_tree, df.test$MScore_Curr),"tpr","fpr")

# Compare AUROC with the other models
plot(perf_plot_full, col='blue', lwd=2) 
plot(perf_plot_red, add=TRUE, col='red', lwd=2) 
plot(perf_plot_red_1, add=TRUE, col='orange', lwd=2)
plot(perf_plot_red_2, add=TRUE, col='yellow', lwd=2)
plot(perf_plot_tree, add=TRUE, col='green', lwd=2)

legend("bottom", legend=c("Full model", "Reduced model", "~ MScore_Prev", "~ EBIT_Prev + PLTax_Prev + ROE_Prev", "CART Tree"), lty=(1:1), col=c("blue", "red", "orange", "yellow", "green"))

# Random Forest
set.seed(150)
fit3 <- randomForest(MScore_Curr ~ ., data = df.train, na.action=na.roughfix)

fit3_fitForest <- predict(fit3, newdata = df.test, type="prob")[,2]
fit3_fitForest.na <- as.data.frame(cbind(df.test$MScore_Curr, fit3_fitForest))
colnames(fit3_fitForest.na) <- c('MScore_Curr','pred')
fit3_fitForest.narm <- as.data.frame(na.omit(fit3_fitForest.na)) ##remove na
fit3_pred <- prediction(fit3_fitForest.narm$pred, fit3_fitForest.narm$MScore_Curr)
fit3_perf <- performance(fit3_pred, "tpr", "fpr")

#Plot variable importance
varImpPlot(fit3, main="Random Forest: Variable Importance")

