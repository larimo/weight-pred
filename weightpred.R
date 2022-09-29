# Load packages
library(Metrics)
library(flexmix)
library(randomForest)
library(e1071)
library(h2o)

# Initialize H2o cluster
h2o.init()

# Set seed
set.seed(1)

# Function splits data for training and testing ####
# 'weight': column name of predicted variable
sample_data <- function(edge_info, train_pct){
  data <- edge_info[,-which(names(edge_info) %in% 'weight')] # subsets feature dataframe
  outcome <- edge_info[,which(names(edge_info) %in% 'weight')] # saves weight column
  train_obs <- sample(nrow(data), ceiling(nrow(data) * train_pct)) # samples indices for train data
  out <- list() # creates list to store sampled data
  out$x_train <- data[train_obs,]
  out$x_test <- data[-train_obs,]
  out$y_train <- outcome[train_obs]
  out$y_test <- outcome[-train_obs]
  return(out)
}

#### Prediction methods ####

# function performs a prediction method and calculates Pearson correlation and RMSE for n iterations
# sample_data: output from sample_data function
# method: method utilized for prediction, e.g. "Poisson Regression", "SuperLearner", "Mixture Model", "Random Forest"
train_predict_fun <- function(data, method, num_comp=NULL, num_epochs=NULL, num_hidden=NULL){
  pearson <- c()
  rse <- c()

  if (method == "poi"){
      model <- glm(y_train~., family = poisson(), data = cbind(data$x_train, y_train = data$y_train))
      pred <- predict(model, data$x_test, type = "response")
  }
  
  else if (method == "svm"){
      model <- svm(y_train~., data = cbind(data$x_train, y_train = data$y_train))
      pred <- predict(model, data$x_test)
  }
  
  else if (method == "mix"){
      form <- "y_train ~ ."
      model <- FLXMRglm(family = "poisson")
      flex_fit <- flexmix(as.formula(form), k=num_comp, model = model, data=cbind(data$x_train, y_train = data$y_train))
      pred <- as.data.frame(predict(flex_fit, cbind(data$x_test, y_train = data$y_test)))  
      clust <- clusters(flex_fit, cbind(data$x_test, y_train = data$y_test))
      comp <- sapply(clust, function(x) paste0("Comp.", x))
      pred <- sapply(1:nrow(pred), function(i) pred[i,comp[i]])
    }
  
  else if (method == "rf"){
      model <- randomForest(y_train ~ .,  data = cbind(data$x_train, y_train = data$y_train))
      pred <- predict(model, newdata = data$x_test)
  }
  else if (method == "nn1"){
    model <- h2o.deeplearning(x = names(data$x_train), y = 'weight', training_frame = as.h2o(cbind(data$x_train, weight = data$y_train)), model_id = "dl_fit1", 
                              hidden = c(num_hidden), seed = 1, epochs = num_epochs, distribution = 'poisson')
    pred <- predict(model, newdata = as.h2o(data$x_test))
    pred <- as.vector(unlist(pred)) # Transform H2o dataframe into numeric
  }
  else if (method == "nn2"){
    model <- h2o.deeplearning(x = names(data$x_train), y = 'weight', training_frame = as.h2o(cbind(data$x_train, weight = data$y_train)), model_id = "dl_fit1", 
                              hidden = c(num_hidden, num_hidden), seed = 1, epochs = num_epochs, distribution = 'poisson')
    pred <- predict(model, newdata = as.h2o(data$x_test))
    pred <- as.vector(unlist(pred)) # Transform H2o dataframe into numeric
  }
  return(pred)
}

print_metrics <- function(method, pearson, rse){
  cat(paste("\n===== Metrics for", method, "=====\n"))
  print(c("Pearson" = mean(pearson), "RSE" = mean(rse)))
  cat("\n=== Stats for Pearson ===\n")
  print(summary(pearson))
  cat("\n=== Stats for RSE ===\n")
  print(summary(rse))
}

save_file <- function(df, dataset=DATASET, method=METHOD, weight_type=WEIGHT_TYPE, metadata_sim=TRUE, num_comp=NULL, num_epochs=NULL, num_hidden=NULL){
  method_name <- ifelse(method=="mix", paste0(method, num_comp), method)
  method_name <- ifelse(method %in% c("nn1", "nn2"), paste0(method, '_', num_hidden, '_', num_epochs), method)
  path <- ifelse(metadata_sim==TRUE, 'meta_topo/', 'topo_only/')
  filename <- paste0(weight_type, "_", dataset, "_", method_name, ".csv")
  print(paste0('filename: ', filename))
  write.csv(df, filename, row.names = FALSE)
  return(NULL)
}

