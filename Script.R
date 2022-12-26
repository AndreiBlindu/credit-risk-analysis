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

# Import and view dataset
View(Complete_Data)
# View summary and column names
summary(Complete_Data)
colnames(Complete_Data)

#############################################################################
# DATA PRE-PROCESSING
#############################################################################

# Consider Turnover, EBIT, PLTax, Leverage, ROE and TAsset only for year 2019 
# in order to predict MScore (response) for 2020
feature_list <- c('Company name', 
                  'Turnover.2019', 
                  'EBIT.2019', 
                  'PLTax.2019', 
                  'Region', 
                  'Country', 
                  'NACE code',
                  'Sector 1',
                  'Sector 2',
                  'Leverage.2019', 
                  'ROE.2019', 
                  'TAsset.2019', 
                  'MScore.2020')

feature_list <- c('Company name', 
                  'Turnover.2020',
                  'Turnover.2019', 
                  'Turnover.2018',
                  'Turnover.2017',
                  'Turnover.2016',
                  'Turnover.2015',
                  'EBIT.2020',
                  'EBIT.2019', 
                  'EBIT.2018',
                  'EBIT.2017',
                  'EBIT.2016', 
                  'EBIT.2015',
                  'PLTax.2020',
                  'PLTax.2019',
                  'PLTax.2018',
                  'PLTax.2017',
                  'PLTax.2016',
                  'PLTax.2015',
                  'Region', 
                  'Country', 
                  'NACE code',
                  'Sector 1',
                  'Sector 2',
                  'Leverage.2020',
                  'Leverage.2019',
                  'Leverage.2018',
                  'Leverage.2017',
                  'Leverage.2016',
                  'Leverage.2015',
                  'ROE.2020',
                  'ROE.2019',
                  'ROE.2018',
                  'ROE.2017',
                  'ROE.2016',
                  'ROE.2015',
                  'TAsset.2020',
                  'TAsset.2019',
                  'TAsset.2018',
                  'TAsset.2017',
                  'TAsset.2016',
                  'TAsset.2015',
                  'MScore.2020',
                  'MScore.2019',
                  'MScore.2018',
                  'MScore.2017',
                  'MScore.2016',
                  'MScore.2015'
                  )

feature_list <- c('Company name', 
                  'Region', 
                  'Country', 
                  'NACE code',
                  'Sector 1',
                  'Sector 2',
                  'MScore.2020',
                  'MScore.2019',
                  'MScore.2018',
                  'MScore.2017',
                  'MScore.2016',
                  'MScore.2015'
)

# Remove missing values with na.omit
data <- na.omit(Complete_Data[feature_list])
colnames(data)
summary(data)

sapply(data, class)

# We want the response variable MScore.2020 to be a binary variable with
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

# See the distribution of the values for the MScore.2020
# We can notice that the values are very imbalanced since there's a 88% of 0 and 12% of 1
hist(data1$MScore.2020)
table(data1$MScore.2020)
prop.table(table(data1$MScore.2020))

# Use One-hot encoding for the Country column
data1$Italy <- ifelse(data1$Country == "Italy", 1, 0)
data1$France <- ifelse(data1$Country == "France", 1, 0)
data1$Spain <- ifelse(data1$Country == "Spain", 1, 0)
data1$Germany <- ifelse(data1$Country == "Germany", 1, 0)
# Check if the countries are balanced, they are not
prop.table(table(data1$Italy))
prop.table(table(data1$France))
prop.table(table(data1$Spain))
prop.table(table(data1$Germany))

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
data1$NaceA <- ifelse(data1$`NACE code` < 0500, 1, 0)
data1$NaceB <- ifelse(data1$`NACE code` >= 0500 & data1$`NACE code` < 1000, 1, 0)
data1$NaceC <- ifelse(data1$`NACE code` >= 1000 & data1$`NACE code` < 3500, 1, 0)
data1$NaceD <- ifelse(data1$`NACE code` >= 3500 & data1$`NACE code` < 3600, 1, 0)
data1$NaceE <- ifelse(data1$`NACE code` >= 3600 & data1$`NACE code` <= 3900, 1, 0)
data1$NaceF <- ifelse(data1$`NACE code` >= 4100 & data1$`NACE code` <= 4399, 1, 0)
data1$NaceG <- ifelse(data1$`NACE code` >= 4500 & data1$`NACE code` <= 4799, 1, 0)
data1$NaceH <- ifelse(data1$`NACE code` >= 4900 & data1$`NACE code` < 5500, 1, 0)
data1$NaceI <- ifelse(data1$`NACE code` >= 5500 & data1$`NACE code` < 5800, 1, 0)
data1$NaceJ <- ifelse(data1$`NACE code` >= 5800 & data1$`NACE code` < 6400, 1, 0)
data1$NaceK <- ifelse(data1$`NACE code` >= 6400 & data1$`NACE code` < 6800, 1, 0)
data1$NaceL <- ifelse(data1$`NACE code` >= 6800 & data1$`NACE code` < 6900, 1, 0)
data1$NaceM <- ifelse(data1$`NACE code` >= 6900 & data1$`NACE code` <= 7500, 1, 0)
data1$NaceN <- ifelse(data1$`NACE code` >= 7700 & data1$`NACE code` < 8300, 1, 0)
data1$NaceO <- ifelse(data1$`NACE code` >= 8400 & data1$`NACE code` < 8500, 1, 0)
data1$NaceP <- ifelse(data1$`NACE code` >= 8500 & data1$`NACE code` < 8600, 1, 0)
data1$NaceQ <- ifelse(data1$`NACE code` >= 8600 & data1$`NACE code` < 8900, 1, 0)
data1$NaceR <- ifelse(data1$`NACE code` >= 9000 & data1$`NACE code` < 9400, 1, 0)
data1$NaceS <- ifelse(data1$`NACE code` >= 9400 & data1$`NACE code` < 9700, 1, 0)
data1$NaceT <- ifelse(data1$`NACE code` >= 9700 & data1$`NACE code` < 9900, 1, 0)
data1$NaceU <- ifelse(data1$`NACE code` >= 9900, 1, 0)

colnames(data1)

# Remove having SD equal to 0 (for example NACE columns that never occur)
cols_sd_zero <- list()
for (i in 1:length(colnames(data1))) {
  sd_ <- sd(data1[[i]])
  if ( sd_ == 0 & !is.na(sd_) ) {
    cols_sd_zero <- append(cols_sd_zero, colnames(data1)[i])
  }
}
cols_sd_zero
data1 <- data1[, !names(data1) %in% cols_sd_zero]
colnames(data1)

# Keep only the useful variables, that are the numerical and One-hot encoded ones
# For now we don't consider the info about 
# "Company name", "Region", "Sector 1" and "Sector 2" since they don't bring useful info
# and also remove "Country" and "NACE code" columns since we already One-hot encoded them
df <- data1[, !names(data1) %in% c("Company name", "Region", "Sector 1", "Sector 2",
                                   "Country", "NACE code")]
colnames(df)

#############################################################################
# CORRELATIONS
#############################################################################
?cor

# Check all correlations by using the cor function 
# Used use = "complete.obs" because to avoid problem due to
# sd of PLTax.2019 and ROE.2019 being NA
correlations <- cor(df, use = "complete.obs", method = "spearman")
correlations

# Visualize the correlation plot
corrplot(correlations, method="square")

# Compute partial correlations
#install.packages("ppcor")
library(ppcor)
?pcor

partial_correlations <- pcor(df, method = "spearman")
partial_correlations

# Visualize the partial correlation plot
corrplot(partial_correlations$estimate, method = "square")


#############################################################################
# LOGISTIC REGRESSION MODEL
#############################################################################

# We divide our dataset in training and test set by random sampling
# We put 70% of our data in train and the remaining in test
# Set a random seed so that your results can be reproduced

## MScore inbalanced and distrubuted as in the full dataset in train and test
set.seed(300)
perc        <- 0.7
n_train     <- round(perc*nrow(df))
n_test  <- nrow(df) - n_train
data_sample <- df[sample(nrow(df)), ]          
df.train  <- data_sample[1:n_train, ]              
df.test   <- data_sample[(n_train+1):nrow(data_sample), ]    
# Check if the response variable MSCore.2020 is equally distributed in the
# training and test set
prop.table(table(df.train$MScore.2020))
hist(df.train$MScore.2020)
prop.table(table(df.test$MScore.2020))
hist(df.test$MScore.2020)

## MScore balanced: to avoid the classifier being biased towards one class we
## sample in such a way that we have an equal number of samples with MScore=1 and
## MScore=0 in both train and test
## https://www.rdocumentation.org/packages/ROSE/versions/0.0-4/topics/ovun.sample
## https://www.r-bloggers.com/2021/05/class-imbalance-handling-imbalanced-data-in-r/
#install.packages("ROSE")
library(ROSE)
df.train <- ovun.sample(MScore.2020~., data = df.train, method = "both", N = n_train)$data
df.test <- ovun.sample(MScore.2020~., data = df.test, method = "both", N = n_test)$data 
# Check if the response variable MSCore.2020 is balanced in class 1 and 0 
# and equally distributed in the training and test set
table(df.train$MScore.2020)
prop.table(table(df.train$MScore.2020))
hist(df.train$MScore.2020)
table(df.test$MScore.2020)
prop.table(table(df.test$MScore.2020))
hist(df.test$MScore.2020)

## FUNCTIONS FOR MODEL EVALUATION
evaluate_model <- function(fit, df) {
  ## Get predicted default probabilities on both train and test set
  train_score <- predict(fit, type='response', df.train)
  test_score <- predict(fit, type='response', df.test)
  
  # Decide a cut-off and get predictions
  cut_off <- prop.table(table(df.train$MScore.2020))[2]
  train_pred <- ifelse(train_score<=cut_off, 0, 1)
  test_pred <- ifelse(test_score<=cut_off, 0, 1)
  
  ## Train MSE
  mse_train <- mean((train_pred-df.train$MScore.2020)^2)
  print(paste("Train MSE: ", mse_train))
  ## Test MSE
  mse_test <- mean((test_pred-df.test$MScore.2020)^2)
  print(paste("Test MSE: ", mse_test))
  
  ## Train accuracy
  n_train <- nrow(df.train)
  train_correct <- ifelse(train_pred == df.train$MScore.2020, 1, 0)
  train_acc <- sum(train_correct)/n_train
  print(paste("Train accuracy: ", train_acc))
  
  ## Test accuracy
  n_test <- nrow(df.test)
  test_correct <- ifelse(test_pred == df.test$MScore.2020, 1, 0)
  test_acc <- sum(test_correct)/n_test
  print(paste("Test accuracy: ", test_acc))
  
  ##false positive rate
  n_neg <- nrow(df.test[df.test$MScore.2020==0,])
  fp_flag <- ifelse(test_pred==1 & df.test$MScore.2020==0, 1, 0)
  fpr <- sum(fp_flag)/n_neg #false positive rate
  print(paste("False positive rate: ", fpr))
  
  ##false negative rate
  n_pos <- nrow(df.test[df.test$MScore.2020==1,])
  fn_flag <- ifelse(test_pred==0 & df.test$MScore.2020==1, 1, 0)
  fnr <- sum(fn_flag)/n_pos #false negative rate
  print(paste("False negative rate: ", fnr))
  
  ##sensitivity
  print(paste("Sensitivity: ", 1- fnr))
  
  ##specificity
  print(paste("Specificity: ", 1- fpr))
}

# FULL MODEL

## For now let's try to use the model with all variables to see which coefficients
## are significant and how well it performs. We expect that not all variables will
## be useful to explain the response (as already seen from the correlations) and
## the full model is also likely to overfit and perform badly on the test set.
fit_full <- glm(MScore.2020 ~ ., data=df.train, family=binomial())
summary(fit_full)

## Odds ratios 
exp(coefficients(fit_full))

## Evaluate model
evaluate_model(fit_full, df)

# REDUCED MODEL
## Let's now consider the model fitted on only the most significant variables
## that from the summary of the full model are:
## Turnover.2019; EBIT.2019; PLTax.2019; TAsset.2019; 
fit_r <- glm(MScore.2020 ~ Turnover.2019 + EBIT.2019 + PLTax.2019 + TAsset.2019, 
             data=df.train, family=binomial())
summary(fit_r)

## Odds ratios 
exp(coefficients(fit_r))

## Evaluate model
evaluate_model(fit_r, df)


#############################################################################
# TREE MODELS
#############################################################################

# We use the CART decision tree algorithm
# The CART algorithm for classification trees minimizes the Gini impurity in each group
#install.packages("readxl")
#install.packages("rpart")
#install.packages("partykit")
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("ROCR")
#install.packages("randomForest")
#install.packages("pROC")
library(readxl) 
library(rpart) 
library(partykit) 
library(rattle) 
library(rpart.plot) 
library(caret) 
library(ggplot2)
library(ROCR) 
library(randomForest) 
library(pROC) 

evaluate_tree <- function(fit, df) {
  # Make predictions on the test sample
  score <- predict(fit,type='prob',df.test)
  pred <- prediction(score[,2],df.test$MScore.2020)
  perf <- performance(pred,"tpr","fpr")
  
  # Model performance plot
  plot(perf, lwd=2, colorize=TRUE, main="ROC Fit: Recursive Partitioning")
  lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3)
  
  # AUROC, KS and GINI
  # The KS statistic is the maximum difference between the cumulative percentage of "yes" (cumulative true positive rate)
  # and the cumulative percentage of "no" (cumulative false positive rate)
  # The Gini coefficient is measured in values between 0 and 1, where a score of 1 means that the model is 100% accurate
  # in predicting the outcome, While a Gini score equal to 0 means that the model is entirely inaccurate (random model).
  AUROC <- round(performance(pred, measure = "auc")@y.values[[1]]*100, 2)
  KS <- round(max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])*100, 2)
  Gini <- (2*AUROC - 100)
  cat("AUROC: ",AUROC,"\tKS: ", KS, "\tGini:", Gini, "\n")
}

## CART Tree
## The CART algorithm for classification trees minimizes the Gini impurity in each group
fit_tree <- rpart(MScore.2020 ~ ., data=df.train, method = "class")

## Print tree detail
printcp(fit_tree)

## Plot the tree
plot(fit_tree, margin = 0.2, main="Tree: Recursive Partitioning")
text(fit_tree, cex=0.8) 

prp(fit_tree, type=2, extra=1,  main="Tree: Recursive Partitioning") # type=2 draws the split labels below the node labels
# extra=1 displays the number of observations that fall in the node 

fancyRpartPlot(fit_tree)

## evaluate tree model
evaluate_tree(fit_tree, df)

## CTREE
## Differently from the CART, ctree uses a significance test procedure 
## in order to select variables instead of selecting the variables that minimize the Gini impurity.
fit_ctree <- ctree(MScore.2020 ~ ., data=df.train)

## Summary
fit_ctree

# This is essentially a decision tree but with extra information in the terminal nodes.
plot(fit_ctree, gp = gpar(fontsize = 6),     
     ip_args=list(abbreviate = FALSE, 
                  id = FALSE))

evaluate_tree(fit_ctree, df)

