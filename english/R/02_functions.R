#------------------------------------------#
## User- defined functies 
#------------------------------------------#
## read_and_clean_data
# This function reads the data.csv file and cleans this data,
# Straighten factors of variables, among other things
read_training_data <- function(){
  # reading of data
  data <- read_csv("data/data.csv")
  
  # Change factor to character
  to_factor <- c("charge_disposition", "sentence_court_name", "gender", "offense_category", "sentence", "convicted_chicago")
  
  data <- data %>%
    mutate_if(is.character, as.factor) %>% 
    select(-X1)
  
  return(data)
}

#------------------------------------------#
## cbr_model
# This function runs a cbr_model over the user entered
# query. The database is the output of read_training_data
# k is number of nearest neighbors and method is method
# of the KNN algorithm. The output is a list of the results_dataframe
# containing the query case and the most-similar cases
# from the training_data, the predicted class and the probability
# of this predicted class.

cbr_model <- function(database,
                      query,
                      k,
                      methode = c("kd_tree", "cover_tree", "brute")) {
  #normalize training data
  normalize <- function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  database_nzd <- database %>%
    mutate_if(is.numeric, normalize)
  
  data_full <- database %>%
    select(-sentence)
  
  query_data <- query
  
  normalized_query <- sapply(names(data_full), function(col) {
    if (is.factor(query_data[[col]])) {
      as.character(query_data[[col]])
    }
    else {
      (query_data[[col]] - min(data_full[[col]])) / (max(data_full[[col]]) - min(data_full[[col]]))
    }
  })
  
  query_data_normalized <- data_full %>%
    rbind(normalized_query) %>%
    mutate_if(is.character, as.numeric) %>%
    tail(1) %>%
    select(-gender, -case_id) %>%
    mutate(sentence = as.factor(1))
  
  # Bake the query using the recipe of the data on which the CBR system must be made
  recipe <-
    recipe(sentence ~ .,
           data = database) %>%
    step_dummy(charge_disposition) %>%
    step_dummy(sentence_court_name) %>%
    step_dummy(offense_category) %>%
    step_dummy(convicted_chicago) %>%
    prep() 
  
  
  data_query <- recipe %>%
    bake(query_data_normalized)
  
  # Define data for KNN
  train_knn <- recipe %>% 
    bake(database_nzd) %>% 
    select(-sentence, -case_id, -gender)
  
  query_knn <- data_query %>%
    select(-sentence)
  
  # calculate the maximum distance of a dataset between two data points (0,0) vs
  # (1,1) for all variables. The total distance of points is divided by
  # this number to express it as a percentage
  max_knn_dist <- function(data) {
    no_col <- ncol(data)
    row_nul <- rep(0, no_col)
    row_one <- rep(1, no_col)
    tibble <- as_tibble(row_nul) %>%
      cbind(row_one)
    tibble <- t(tibble)
    test_knn <- tibble
    train_knn <- tibble
    max_dist <- FNN::get.knnx(
      data = train_knn,
      query = test_knn,
      k = 2
    )
    max_dist <- max(max_dist$nn.dist)
    
    return(max_dist)
  }
  
  # maximum distance is based on training data
  max_dist <- max_knn_dist(train_knn)
  
  # separate data frame for the class of the train data
  # This is the KNN's CL argument
  train_class <- database %>% 
    select(sentence)
  # Rus KNN model
  knn_results <- FNN::knn(train = train_knn,
                          test = query_knn,
                          cl = train_class$sentence, 
                          k = k, 
                          algorithm = methode,
                          prob = TRUE) 
  
  
  # prepare CBR system
  # Save the different attributes of the knn_results separately
  knn_attributes <- attributes(knn_results)
  #dit is de afstand tussen de query-data en de test data
  distance_knn <- t(knn_attributes$nn.dist) 
  # the distance is divided by the max_dist
  distance_knn <- as_tibble(1 - (distance_knn / max_dist)) 
  #index to locate nearest neighbors
  index_knn <- as_tibble(t(knn_attributes$nn.index)) 
  
  # add column similarity, sentence and type to be able to merge with results
  query_results <- query_data %>%
    mutate(
      similarity = NA,
      sentence = NA,
      category = "query"
    )
  
  # Here it is necessary to use the training set without dummy variables
  # The data is normalized, and therefore it must be the dataset that is not normalized
  # can also be added. This is data_not_nzd
  training_wo_dummies <- database
  
  # Results of the KNN_search 
  index <- index_knn[[1]] 
  distance <- distance_knn[[1]]
  results_dataframe <- training_wo_dummies %>%
    # select KNN index
    slice(index) %>% 
    # add similarity column
    cbind(similarity = distance) %>% 
    # add "category" to make clear which is the query and which is the test case
    mutate(category = "train_set") %>% 
    # Add results to query
    rbind(query_results) %>% 
    # Arrange by 'category' to make sure that the query is displayed at first
    arrange(category) %>% 
    relocate(category, similarity, sentence, gender) 
  
  class_query <- class_query <- levels(knn_results)
  
  results <- list("results_dataframe" = results_dataframe,
                  "class" = class_query,
                  "probability" = knn_attributes$prob)
  print(results)
  return(results)
}



