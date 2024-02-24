dataset <- read.csv("last_combined.csv")
head(dataset)
dataset <- dataset[, !(names(dataset) %in% c("id", "X"))]
str(dataset)
summary(dataset)

#------------------------------------------------DATA QUALITY MATRIX FUNCTION----------------------------------------
data_quality_matrix <- function(data) {
  numeric_data <- data[, sapply(data, is.numeric)]
  categorical_data <- data[, sapply(data, function(x) is.factor(x) | is.character(x))]
  missing_values_numeric <- colSums(is.na(numeric_data))
  unique_values_numeric <- sapply(numeric_data, function(x) length(unique(x)))
  total_values_numeric <- sapply(numeric_data, function(x) length(x))
  mean_values_numeric <- colMeans(numeric_data, na.rm = TRUE)
  std_deviation_numeric <- sapply(numeric_data, function(x) sd(x, na.rm = TRUE))
  min_values_numeric <- sapply(numeric_data, function(x) min(x, na.rm = TRUE))
  max_values_numeric <- sapply(numeric_data, function(x) max(x, na.rm = TRUE))
  median_values_numeric <- sapply(numeric_data, function(x) median(x, na.rm = TRUE))
  quantile25_numeric <- sapply(numeric_data, function(x) quantile(x, 0.25, na.rm = TRUE))
  quantile75_numeric <- sapply(numeric_data, function(x) quantile(x, 0.75, na.rm = TRUE))
  missing_values_categorical <- colSums(is.na(categorical_data))
  unique_values_categorical <- sapply(categorical_data, function(x) length(unique(x)))
  total_values_categorical <- sapply(categorical_data, function(x) length(x))
  
  quality_matrix <- data.frame(
    Type = rep(c("Numeric", "Categorical"), c(ncol(numeric_data), ncol(categorical_data))),
    Missing_Values = c(missing_values_numeric, missing_values_categorical),
    Unique_Values = c(unique_values_numeric, unique_values_categorical),
    Total_Values = c(total_values_numeric, total_values_categorical),
    Mean_Value = c(mean_values_numeric, rep(NA, ncol(categorical_data))),
    Standard_Deviation = c(std_deviation_numeric, rep(NA, ncol(categorical_data))),
    Min_Value = c(min_values_numeric, rep(NA, ncol(categorical_data))),
    Max_Value = c(max_values_numeric, rep(NA, ncol(categorical_data))),
    Median = c(median_values_numeric, rep(NA, ncol(categorical_data))),
    Quantile25 = c(quantile25_numeric, rep(NA, ncol(categorical_data))),
    Quantile75 = c(quantile75_numeric, rep(NA, ncol(categorical_data)))
  )
  
  return(quality_matrix)
}


data_quality_matrix_result <- data_quality_matrix(dataset)
data_quality_matrix_result

#----------------------------------------------------------------------------------------------------------------
#Unique Value Check
for (col in names(dataset)[sapply(dataset, is.character)]) {
  unique_values <- unique(dataset[[col]])
  cat("Unique values for", col, ":", toString(unique_values), "\n\n")
}

#Null Check
sum(is.na(dataset))

#Duplicate Row Check
duplicate_rows <- dataset[duplicated(dataset), ]
cat("Number of duplicated rows : ",sum(duplicate_rows))
rows_with_missing_values <- dataset[apply(dataset, 1, function(x) any(is.na(x))), ]
rows_with_missing_values

#Remove the missing values
dataset <- na.omit(dataset)
sum(is.na(dataset))
data_quality_matrix_result <- data_quality_matrix(dataset)
data_quality_matrix_result


#Age Binning
dataset$age_bins <- cut(dataset$Age, breaks = seq(0, max(dataset$Age) + 10, by = 10), labels = FALSE)
dataset <- dataset[, !(names(dataset) %in% c("Age"))]
satisfaction <- dataset[,22]
dataset <- cbind(dataset[,-22], satisfaction)
column_names <- colnames(dataset)

#Boxplot and Histogram Graphs
for (i in seq(ncol(dataset))) {
  if (is.numeric(dataset[[i]])){
    par(mfrow=c(2,1))
    boxplot(dataset[[i]], horizontal=TRUE, outline=TRUE, frame=F, col="lightcoral", main=paste("Boxplot of ",names(dataset)[i]), xlab="Values",axes = FALSE)
    hist(dataset[[i]], col="lightblue", freq=FALSE, main=paste("Histogram of", names(dataset)[i]), xlab=names(dataset)[i])
    lines(density(dataset[[i]]), col="blue")
  }
}

#Barplot Graphs
for (col in names(dataset)[sapply(dataset, is.factor)]) {
  barplot_data <- table(dataset[[col]])
  bar_heights <- barplot_data / sum(barplot_data)
  bp <- barplot(bar_heights, main = col, col = "skyblue", xlab = col, ylab = "Frequency", ylim = c(0, 1))
  abline(h = 1, col = "black", lty = 1)
  
  text(x = bp, y = bar_heights, labels = paste0(round(bar_heights * 100, 1), "%"), pos = 3, col = "black", cex = 0.8)
}

numeric_feature_names <- names(dataset)[sapply(dataset, is.numeric)]

#Clamp Transformation
clamp_outlier <- function(x, threshold, column_name) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- Q3 - Q1
  lower_limit <- Q1 - threshold * IQR_value
  upper_limit <- Q3 + threshold * IQR_value
  
  outlier_count <- sum(x < lower_limit | x > upper_limit)
  outlier_ratio <- outlier_count / length(x)
  cat("Before Outlier Rate for ",column_name," --> ", outlier_ratio, "\n")
  
  x[x < lower_limit] <- lower_limit
  x[x > upper_limit] <- upper_limit
  outlier_count <- sum(x < lower_limit | x > upper_limit)
  outlier_ratio <- outlier_count / length(x)
  cat("After Outlier Rate for ",column_name," --> ", outlier_ratio, "\n")
  
  return(x)
}

threshold <- 1.5
numeric_columns <- names(dataset)[sapply(dataset, is.numeric)]

for (column_name in numeric_columns) {
  dataset[[column_name]] <- clamp_outlier(dataset[[column_name]], threshold, column_name)
}


data_quality_matrix_result <- data_quality_matrix(dataset)


#One Hot Encoding
library(caret)
dummy <- dummyVars(" ~ .", data = dataset)
final_dataset <- data.frame(predict(dummy, newdata = dataset))


#Correlation Matrix
library(reshape2)
numeric_features <- final_dataset[sapply(final_dataset, is.numeric)]
correlation_matrix <- cor(numeric_features)
melted_correlation <- melt(correlation_matrix)
melted_correlation$percentage <- melted_correlation$value * 100

library(ggplot2)
ggplot(data = melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  geom_text(aes(label = paste(round(percentage, 1), "%")),
            size = 6, angle = 0, vjust = 0.8, hjust = 0.4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   size = 20, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(3, "cm"))

#Removing features that are 100% correlated
features_to_remove <- c("GenderFemale", "Customer.Typedisloyal.Customer", "Type.of.TravelBusiness.travel", "satisfactionneutral.or.dissatisfied")
final_dataset <- final_dataset[, !(colnames(final_dataset) %in% features_to_remove)]


selected_cor_matrix <- cor(final_dataset)
selected_melted_correlation <- melt(selected_cor_matrix)
selected_melted_correlation$percentage <- selected_melted_correlation$value * 100
ggplot(data = selected_melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  geom_text(aes(label = paste(round(percentage, 1), "%")),
            size = 6, angle = 0, vjust = 0.8, hjust = 0.4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   size = 20, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(3, "cm"))


#Under-sampling
library(dplyr)
sampled_data <- final_dataset %>%
  group_by(satisfactionsatisfied) %>%
  sample_n(size = 10000, replace = FALSE) %>%
  ungroup()

sampled_data_dataframe <- as.data.frame(sampled_data)
sampled_data_dataframe$satisfactionsatisfied <- as.factor(sampled_data_dataframe$satisfactionsatisfied)
sampled_data_dataframe$satisfactionsatisfied <- factor(sampled_data_dataframe$satisfactionsatisfied)



#---------------------------------------PCA------------------------------------------------------------

target_variable <- sampled_data_dataframe$satisfactionsatisfied
sampled_data_dataframe_temp <- sampled_data_dataframe[, -which(names(sampled_data_dataframe) %in% c("satisfactionsatisfied"))]
pca_result <- prcomp(sampled_data_dataframe_temp, center = TRUE, scale = TRUE)
summary(pca_result)
plot(cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2)), 
     xlab = "Number of Principal Components", 
     ylab = "Cumulative Proportion of Variance Explained",
     main = "Cumulative Proportion of Variance Explained by Principal Components")

#---------------------------------------PCA End---------------------------------------------------------

#---------------------------------------RFE-----------------------------------------------------------------

rfe_control <- rfeControl(functions=rfFuncs, method="cv", number=5, verbose=TRUE)
rfe_results <- rfe(sampled_data_dataframe[,1:24], sampled_data_dataframe[,25], sizes=c(1:25), rfeControl=rfe_control)
print(rfe_results)
predictors(rfe_results)
plot(rfe_results, type=c("g", "o"))

most_important_features <- c(predictors(rfe_results), "satisfactionsatisfied")
most_important_dataframe <- sampled_data_dataframe[, most_important_features]
numerical_columns <- sapply(most_important_dataframe, is.numeric)
most_important_dataframe[, numerical_columns] <- scale(most_important_dataframe[, numerical_columns])
head(most_important_dataframe)

shuffled_data <- most_important_dataframe[sample(nrow(most_important_dataframe)), ]
#---------------------------------------RFE End-------------------------------------------------------------

#Dataset train and test split 
shuffled_sample <- sample(1:nrow(shuffled_data), size=nrow(shuffled_data)*0.8)
train_set <- shuffled_data[shuffled_sample,]
test_set <- shuffled_data[-shuffled_sample,]

# ---------------------------------------N-fold CV (Initializing and fixing folds)----------------------------------------------------------
n <- 10
kfold_cv <- function(data, k) {
  n <- nrow(data)
  fold_size <- n %/% k
  folds <- rep(1:k, each = fold_size)
  return(folds)
}
folds <- kfold_cv(train_set, n)

# ---------------------------------------N-fold CV End-----------------------------------------------------


# ---------------------------------------KNN---------------------------------------------------------------
set.seed(20)
library(caret)
knn_cv_train_results <- list()
knn_cv_validation_results <- list()
tuneGrid <- expand.grid(
  k = c(1:10)
)
for (i in 1:n) {
  train_data <- train_set[folds != i, ]
  validation_data <- train_set[folds == i, ]
  model <- train(satisfactionsatisfied ~ ., data = train_data, method = "knn", tuneGrid = tuneGrid)
  pred <- predict(model, newdata = validation_data)
  confusion_mat <- confusionMatrix(pred, validation_data$satisfactionsatisfied)
  accuracy_val_knn <- confusion_mat$overall["Accuracy"]
  kappa_val_knn <- confusion_mat$overall["Kappa"]
  
  knn_cv_train_results[[i]] <- data.frame(
    Accuracy = max(model$results$Accuracy),
    Kappa = max(model$results$Kappa)
  )
  print(knn_cv_train_results[[i]])
  
  knn_cv_validation_results[[i]] <- data.frame(
    Accuracy = accuracy_val_knn,
    Kappa = kappa_val_knn
  )
  
  print(knn_cv_validation_results[[i]])
  
  cat("Fold ", i, " has finished.\n")
}

knn_model <- train(satisfactionsatisfied ~ ., data = train_set, method = "knn", tuneGrid = tuneGrid)
knn_pred <- predict(knn_model, newdata = test_set)
conf_mat_knn <- confusionMatrix(knn_pred, test_set$satisfactionsatisfied)
print(conf_mat_knn$overall["Accuracy"])
print(conf_mat_knn$overall["Kappa"])
best_tune_knn <- knn_model$bestTune
print("Best tune:")
print(best_tune_knn)

results_df <- data.frame(
  Train_Accuracy = sapply(knn_cv_train_results, function(x) x$Accuracy),
  Train_Kappa = sapply(knn_cv_train_results, function(x) x$Kappa),
  Validation_Accuracy = sapply(knn_cv_validation_results, function(x) x$Accuracy),
  Validation_Kappa = sapply(knn_cv_validation_results, function(x) x$Kappa),
  Test_Accuracy = conf_mat_knn$overall["Accuracy"],
  Test_Kappa = conf_mat_knn$overall["Kappa"],
  Best_Tune_K = knn_model$bestTune$k
)
write.xlsx(results_df_knn, "results_knn.xlsx", sheetName = "Results", row.names = FALSE)
cat("Results are saved to results_knn.xlsx\n")

# ---------------------------------------KNN End-------------------------------------------------


#Find the best model in list according to their accuracy
findBestModel <- function(modelList) {
  bestModel <- NULL
  bestAccuracy <- -Inf
  bestKappa <- 0
  
  for (modelName in names(modelList)) {
    print(modelName)
    currentAccuracy <- max(modelList[[modelName]]$results$Accuracy)
    currentKappa <- max(modelList[[modelName]]$results$Kappa)
    print(currentAccuracy)
    if (currentAccuracy > bestAccuracy) {
      bestAccuracy <- currentAccuracy
      bestModel <- modelList[[modelName]]
      bestKappa <- currentKappa
    }
  }
  
  return(list(model = bestModel, accuracy = bestAccuracy, kappa = bestKappa))
}

# ---------------------------------------Random Forest-------------------------------------------
set.seed(20)
rf_cv_train_results <- list()
rf_cv_validation_results <- list()
tunegrid <- expand.grid(.mtry=c(1:(ncol(train_data))))
for (i in 1:n) {
  train_data <- train_set[folds != i, ]
  validation_data <- train_set[folds == i, ]
  
  list_rf_tr <- list()
  list_rf_val <- list()
  for (ntree in c(25, 50, 100)) {
    print(ntree)
    model <- train(satisfactionsatisfied~., data=train_data, method="rf", tuneGrid=tunegrid, ntree=ntree)
    key <- paste(toString(i), " - ", toString(ntree))
    list_rf_tr[[key]] <- model
    print(list_rf_tr)
  }
  bestModelInfo <- findBestModel(list_rf_tr)
  
  pred <- predict(bestModelInfo$model, validation_data)
  
  confusion_mat <- confusionMatrix(pred, validation_data$satisfactionsatisfied)
  accuracy_val_rf <- confusion_mat$overall["Accuracy"]
  kappa_val_rf <- confusion_mat$overall["Kappa"]
  
  rf_cv_train_results[[i]] <- data.frame(
    Accuracy = bestModelInfo$accuracy,
    Kappa = bestModelInfo$kappa
  )
  
  rf_cv_validation_results[[i]] <- data.frame(
    Accuracy = accuracy_val_rf,
    Kappa = kappa_val_rf
  )
  
  print(rf_cv_train_results)
  print(rf_cv_validation_results)
  cat("Fold ", i, " has finished.\n")
}

modellist_rf <- list()
for (ntree in c(25, 50, 100)) {
  print(ntree)
  model <- train(satisfactionsatisfied~., data=train_data, method="rf", tuneGrid=tunegrid, ntree=ntree)
  key <- paste(toString(i), " - ", toString(ntree))
  modellist_rf[[key]] <- model
}

best_rf_info <- findBestModel(modellist_rf)
rf_pred <- predict(best_rf_info$model, newdata = test_set)
conf_mat_rf <- confusionMatrix(rf_pred, test_set$satisfactionsatisfied)
print(conf_mat_rf$overall["Accuracy"])
print(conf_mat_rf$overall["Kappa"])
best_tune_rf <- best_rf_info$model$bestTune
print("Best tune:")
print(best_tune_rf)

results_df_rf <- data.frame(
  Train_Accuracy = sapply(rf_cv_train_results, function(x) x$Accuracy),
  Train_Kappa = sapply(rf_cv_train_results, function(x) x$Kappa),
  Validation_Accuracy = sapply(rf_cv_validation_results, function(x) x$Accuracy),
  Validation_Kappa = sapply(rf_cv_validation_results, function(x) x$Kappa),
  Test_Accuracy = conf_mat_rf$overall["Accuracy"],
  Test_Kappa = conf_mat_rf$overall["Kappa"],
  Best_Tune_Mtry = best_rf_info$model$bestTune$mtry
)

write.xlsx(results_df_rf, "results_rf.xlsx", sheetName = "Results", row.names = FALSE)
cat("Results are saved to results_rf.xlsx\n")
# ---------------------------------------Random Forest End---------------------------------------


# ---------------------------------------Naive Bayes---------------------------------------------
set.seed(20)
library(caret)
nb_cv_train_results <- list()
nb_cv_validation_results <- list()
tunegrid <- expand.grid(usekernel = c(TRUE, FALSE),
                        laplace = c(0, 0.5, 1), 
                        adjust = c(0.75, 1, 1.25, 1.5))
for (i in 1:n) {
  train_data <- train_set[folds != i, ]
  validation_data <- train_set[folds == i, ]
  
  
  model <- train(satisfactionsatisfied~., data=train_data, method="naive_bayes", tuneGrid=tunegrid)
  pred <- predict(model, newdata = validation_data)
  
  confusion_mat <- confusionMatrix(pred, validation_data$satisfactionsatisfied)
  accuracy_val_nb <- confusion_mat$overall["Accuracy"]
  kappa_val_nb <- confusion_mat$overall["Kappa"]
  
  nb_cv_train_results[[i]] <- data.frame(
    Accuracy = max(model$results$Accuracy),
    Kappa = max(model$results$Kappa)
  )
  print(nb_cv_train_results[[i]])
  
  nb_cv_validation_results[[i]] <- data.frame(
    Accuracy = accuracy_val_nb,
    Kappa = kappa_val_nb
  )
  print(nb_cv_validation_results[[i]])
  
  cat("Fold ", i, " has finished.\n")
}
nb_model <- train(satisfactionsatisfied ~ ., data = train_set, method = "naive_bayes", tuneGrid = tunegrid)
nb_pred <- predict(nb_model, newdata = test_set)
conf_mat_nb <- confusionMatrix(nb_pred, test_set$satisfactionsatisfied)
print(conf_mat_nb$overall["Accuracy"])
print(conf_mat_nb$overall["Kappa"])
best_tune_nb <- nb_model$bestTune
print("Best tune:")
print(best_tune_nb)

results_df_nb <- data.frame(
  Train_Accuracy = sapply(nb_cv_train_results, function(x) x$Accuracy),
  Train_Kappa = sapply(nb_cv_train_results, function(x) x$Kappa),
  Validation_Accuracy = sapply(nb_cv_validation_results, function(x) x$Accuracy),
  Validation_Kappa = sapply(nb_cv_validation_results, function(x) x$Kappa),
  Test_Accuracy = conf_mat_nb$overall["Accuracy"],
  Test_Kappa = conf_mat_nb$overall["Kappa"],
  Best_Tune_UseKernel = nb_model$bestTune$usekernel,
  Best_Tune_Laplace = nb_model$bestTune$laplace,
  Best_Tune_Adjust = nb_model$bestTune$adjust
)
write.xlsx(results_df_nb, "results_nb.xlsx", sheetName = "Results", row.names = FALSE)
cat("Results are saved to results_nb.xlsx\n")
# ---------------------------------------Naive Bayes END---------------------------------------------

#----------------------------------------Decision Tree-----------------------------------------------
set.seed(20)
library(caret)
dt_cv_train_results <- list()
dt_cv_validation_results <- list()

tuneGrid_dt <- expand.grid(
  cp = seq(0.01, 0.1, by = 0.01)
)

for (i in 1:n) {
  train_data <- train_set[folds != i, ]
  validation_data <- train_set[folds == i, ]
  
  model <- train(satisfactionsatisfied ~ ., data = train_data, method = "rpart", tuneGrid = tuneGrid_dt )
  pred<- predict(model, newdata = validation_data)
  
  confusion_mat <- confusionMatrix(pred, validation_data$satisfactionsatisfied)
  accuracy_val_dt <- confusion_mat$overall["Accuracy"]
  kappa_val_dt <- confusion_mat$overall["Kappa"]
  
  dt_cv_train_results[[i]] <- data.frame(
    Accuracy = max(model$results$Accuracy),
    Kappa = max(model$results$Kappa)
  )
  print(dt_cv_train_results[[i]])
  
  dt_cv_validation_results[[i]] <- data.frame(
    Accuracy = accuracy_val_dt,
    Kappa = kappa_val_dt
  )
  print(dt_cv_validation_results[[i]])
  
  cat("Fold ", i, " has finished.\n")
}

tree_model <- train(satisfactionsatisfied ~ ., data = train_set, method = "rpart", tuneGrid = tuneGrid_dt)
tree_pred <- predict(tree_model, newdata = test_set)
conf_mat_dt <- confusionMatrix(tree_pred, test_set$satisfactionsatisfied)
print(conf_mat_dt$overall["Accuracy"])
print(conf_mat_dt$overall["Kappa"])
best_tune_dt <- tree_model$bestTune
print("Best tune:")
print(best_tune_dt)

results_df <- data.frame(
  Fold_Number = 1:n,
  Train_Accuracy = sapply(dt_cv_train_results, function(x) x$Accuracy),
  Train_Kappa = sapply(dt_cv_train_results, function(x) x$Kappa),
  Validation_Accuracy = sapply(dt_cv_validation_results, function(x) x$Accuracy),
  Validation_Kappa = sapply(dt_cv_validation_results, function(x) x$Kappa),
  Test_Accuracy = conf_mat_dt$overall["Accuracy"],
  Test_Kappa = conf_mat_dt$overall["Kappa"],
  Best_Tune_Cp = best_tune_dt$cp
)

write.xlsx(results_df, "results_dt.xlsx", sheetName = "Results", row.names = FALSE)
cat("Results are saved to results_dt.xlsx\n")

#----------------------------------------Decision Tree End-----------------------------------------------------

#----------------------------------------SVM-------------------------------------------------------------------
set.seed(20)
library(caret)
library(e1071)

cost_values <- c(0.1, 1, 10)
gamma_values <- 10^(-2:-1)
kernel_values <- c("polynomial", "radial")
svm_cv_train_results <- list()
svm_cv_validation_results <- list()

n <- 10
for (i in 1:n) {
  train_data <- train_set[folds != i, ]
  validation_data <- train_set[folds == i, ]
  best_train_acc <- 0
  best_train_kap <- 0
  best_train_model <- NULL 
  for (cost in cost_values) {
    for (gamma in gamma_values) {
      for (kernel in kernel_values) {
        cat("Training SVM model with cost =", cost, ", gamma =", gamma, ", kernel =", kernel, " for Fold ", i, "\n")
        
        svm_model <- svm(
          satisfactionsatisfied ~ .,
          data = train_data,
          cost = cost,
          gamma = gamma,
          kernel = kernel
        )
        
        pred_tr_svm <- predict(svm_model, newdata = train_data)
        confusion_mat <- confusionMatrix(pred_tr_svm, train_data$satisfactionsatisfied)
        accuracy_tr_svm <- confusion_mat$overall["Accuracy"]
        kappa_tr_svm <- confusion_mat$overall["Kappa"]
        if (best_train_acc <= accuracy_tr_svm) {
          best_train_acc <- accuracy_tr_svm
          best_train_kap <- kappa_tr_svm
          best_train_model <- svm_model
        }
      }
    }
  }
  pred <- predict(best_train_model, newdata = validation_data)
  confusion_mat <- confusionMatrix(pred, validation_data$satisfactionsatisfied)
  accuracy_tr_svm <- confusion_mat$overall["Accuracy"]
  kappa_tr_svm <- confusion_mat$overall["Kappa"]
  svm_cv_validation_results[[i]] <- data.frame(
    Accuracy = accuracy_tr_svm,
    Kappa = kappa_tr_svm
  )
  svm_cv_train_results[[i]] <- data.frame(
    Accuracy = best_train_acc,
    Kappa = best_train_kap
  )
  
}

best_train_acc <- 0
best_train_kap <- 0
best_train_model <- NULL 
for (cost in cost_values) {
  for (gamma in gamma_values) {
    for (kernel in kernel_values) {
      cat("Training SVM model with cost =", cost, ", gamma =", gamma, ", kernel =", kernel, " for Fold ", i, "\n")
      
      svm_model <- svm(
        satisfactionsatisfied ~ .,
        data = train_set,
        cost = cost,
        gamma = gamma,
        kernel = kernel
      )
      
      pred_tr_svm <- predict(svm_model, newdata = train_set)
      confusion_mat <- confusionMatrix(pred_tr_svm, train_set$satisfactionsatisfied)
      accuracy_tr_svm <- confusion_mat$overall["Accuracy"]
      kappa_tr_svm <- confusion_mat$overall["Kappa"]
      if (best_train_acc <= accuracy_tr_svm) {
        best_train_acc <- accuracy_tr_svm
        best_train_kap <- kappa_tr_svm
        best_train_model <- svm_model
      }
    }
  }
}

svm_pred <- predict(best_train_model, newdata = test_set)
conf_mat_svm <- confusionMatrix(svm_pred, test_set$satisfactionsatisfied)
print(conf_mat_svm$overall["Accuracy"])
print(conf_mat_svm$overall["Kappa"])
Best_Tune_Cost = best_train_model$cost
Best_Tune_Gamma = best_train_model$gamma
Best_Tune_Kernel = best_train_model$kernel


results_df_svm <- data.frame(
  Train_Accuracy = sapply(svm_cv_train_results, function(x) x$Accuracy),
  Train_Kappa = sapply(svm_cv_train_results, function(x) x$Kappa),
  Validation_Accuracy = sapply(svm_cv_validation_results, function(x) x$Accuracy),
  Validation_Kappa = sapply(svm_cv_validation_results, function(x) x$Kappa),
  Test_Accuracy = conf_mat_svm$overall["Accuracy"],
  Test_Kappa = conf_mat_svm$overall["Kappa"],
  Best_Tune_Cost = best_train_model$cost,
  Best_Tune_Gamma = best_train_model$gamma,
  Best_Tune_Kernel = best_train_model$kernel
)
write.xlsx(results_df_svm, "results_svm.xlsx", sheetName = "Results", row.names = FALSE)
cat("Results are saved to results_svm.xlsx\n")


#------------------------------------------SVM End--------------------------------------------------------------------



#----------------------------------------Neural Network-KERAS----------------------------------------------------------
set.seed(20)
library(xlsx)
library(keras)
library(caret)
training_accuracies <- numeric(n)
validation_split_accuracy <- numeric(n)
cross_validation_accuracies <- numeric(n)
training_kappa_accuracies <- numeric(n)
for (i in 1:n) {
  train_data <- train_set[folds != i, ]
  validation_data <- train_set[folds == i, ]
  
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = 64, activation = 'relu', input_shape = 16) %>% 
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dense(units = 2, activation = 'softmax')
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  early_stopping <- callback_early_stopping(monitor = 'val_loss', patience = 7)
  
  history <- model %>% fit(
    as.matrix(train_data[,-17]), to_categorical(train_data[,17]), 
    epochs = 100, batch_size = 128, 
    validation_split = 0.2,
    callbacks = list(early_stopping),
    verbose = 2
  )
  
  training_accuracies[[i]] <- tail(history$metrics$accuracy, n=1)
  validation_split_accuracy[[i]] <- tail(history$metrics$val_accuracy, n=1)
  
  evaluation_result <- model %>% evaluate(
    as.matrix(validation_data[,-17]), to_categorical(validation_data[,17])
  )
  
  cross_validation_accuracies[i] <- evaluation_result[2]
}

for (i in 1:n) {
  print(i)
  cat("Traning Accuracy : ",training_accuracies[[i]], "\n")
  cat("Validation Split Accuracy : ",validation_split_accuracy[i],"\n")
  cat("Cross Validation Accuracy : ",cross_validation_accuracies[i],"\n")
  print("---------------------------------------------")
}


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 64, activation = 'relu', input_shape = 16) %>% 
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

early_stopping <- callback_early_stopping(monitor = 'val_loss', patience = 7)

history <- model %>% fit(
  as.matrix(train_data[,-17]), to_categorical(train_data[,17]), 
  epochs = 100, batch_size = 128, 
  validation_split = 0.2,
  callbacks = list(early_stopping),
  verbose = 2
)

predictions <- model %>% predict(as.matrix(test_set[, -17]))

predicted_classes <- apply(predictions, 1, function(x) ifelse(x[1] > x[2], 0, 1))

actual_classes <- test_set[, 17]

confusion_matrix <- confusionMatrix(as.factor(predicted_classes), actual_classes)

print(confusion_matrix)


print(confusion_matrix$overall["Accuracy"])
print(confusion_matrix$overall["Kappa"])
best_epoch <- which.min(history$metrics$val_loss)

best_metrics_loss <- history$metrics$loss[[best_epoch]]
best_metrics_acc <- history$metrics$accuracy[[best_epoch]]
best_metrics_valloss <- history$metrics$val_loss[[best_epoch]]
best_metrics_valacc <- history$metrics$val_accuracy[[best_epoch]]

best_steps <- history$params$steps

results_df_keras_last_fit <- data.frame(
  Training_Accuracy = training_accuracies,
  Validation_Split_Accuracy = validation_split_accuracy,
  Cross_Validation_Accuracy = cross_validation_accuracies,
  Best_Epoch = best_epoch,
  Best_Params = best_steps,
  best_loss= best_metrics_loss,
  best_acc = best_metrics_acc,
  best_valloss = best_metrics_valloss,
  best_valacc = best_metrics_valacc
)
plot(history)
write.xlsx(results_df_keras_last_fit, "results_nn.xlsx", sheetName = "Results", row.names = FALSE)
#----------------------------------------Neural Network-KERAS End----------------------------------------------------------