source('weightpred.R')

# datasets_list <- c('bus', 'cs', 'hist', 'BX') # List with names of datasets
# methods_list <- c("poi",  'mix2', 'mix3', 'mix4', 'mix5', "rf", "svm", "nn1", "nn2")
# weight_type_list <- c('AND', 'OR', 'XOR', 'ORIG')

# Define parameters
DATASET <- 'bus'
METHOD <- 'poi'
WEIGHT_TYPE <- 'ORIG'
NUM_TRIALS <- 30
TRAIN_PCT <- 0.9
METADATA_SIM <- TRUE # TRUE: metadata feat; FALSE: only topo feat
# If Neural Network:
NUM_HIDDEN <- NULL
NUM_EPOCHS <- NULL
# If mixture model:
NUM_COMP <- NULL
  
main <- function(dataset=DATASET, method=METHOD, weight_type=WEIGHT_TYPE, num_trials=NUM_TRIALS, 
                 train_pct=TRAIN_PCT, num_comp=NUM_COMP, num_epochs=NUM_EPOCHS, num_hidden=NUM_HIDDEN){
  print(paste0("========= Dataset: ", DATASET, " ========="))
  
  # Read dataset
  edge_features <- read.csv(paste0('data/', DATASET, '_edge_info.csv'))
  edge_features <- edge_info[,-c(1,2,16)] # Remove source, target, rbf_sim columns
  edge_features <- if (METADATA_SIM != TRUE) edge_info[, -c(12,13)] else edge_info
  
  if (WEIGHT_TYPE != 'ORIG'){
    weight <- read.csv(paste0('data/', 'weight_logical/', DATASET, '_', WEIGHT_TYPE, '_weight'))
    edge_info['weight'] <- weight
  }
  
  print(paste("Starting method:", METHOD))
  
  pearson_trials <- c() # Lists to store accuracy metrics Pearson correlation and RSE
  rse_trials <- c()
  
  for (i in 1:num_trials){
    # Keep track of trials
    if (i %% 5 == 0){
      print(paste('Trial #:', i))
    }
    
    train_test_data <- sample_data(edge_features, TRAIN_PCT) # Sample train and test datasets
    # Train on train data and predict on test data
    predicted_test <- train_predict_fun(data=train_test_data, method=METHOD, train_pct=TRAIN_PCT, 
                                        num_comp=NUM_COMP, num_epochs=NUM_EPOCHS, num_hidden=NUM_HIDDEN)
    pcc <- cor(train_test_data$y_test, predicted_test, method = "pearson")
    rse <- rse(train_test_data$y_test, predicted_test)
    
    # Save accuracy metrics
    pearson_trials <- rbind(pearson_trials, p)
    rse_trials <- rbind(rse_trials, s)
  }
  print_metrics(method, pearson_trials, rse_trials) # Print summary of trials
  df <- data.frame("rse"=rse, "pearson"=pearson) # Save results on df
  
  save_file(df, dataset=DATASET, method=METHOD, weight_type=WEIGHT_TYPE, metadata_sim=METADATA_SIM, num_comp=NUM_COMP, num_epochs=NUM_EPOCHS, num_hidden=NUM_HIDDEN)
}



