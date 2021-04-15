#------------------------------------------#
## User- defined functies 
#------------------------------------------#
## read_and_clean_data 
# Deze functie leest het data.csv bestand en schoont deze data op,
# onder meer factors van variabelen rechttrekken
read_training_data <- function(){
  # inlezen van data
  data <- read_csv("data/data.csv")
  
  # omzetten naar factor van character 
  to_factor <- c("charge_disposition", "sentence_court_name", "gender", "offense_category", "sentence", "convicted_chicago")
  
  data <- data %>%
    mutate_if(is.character, as.factor) %>% 
    select(-X1)
  
  return(data)
}

#------------------------------------------#
## cbr_model
# Deze functie runt een cbr_model over de door de user ingevoerde
# query. De database is de output van read_training_data
# k is aantal nearest-neighbours en methode is de methode
# van het KNN algoritme. De output is een lijst met het results_dataframe
# met daarin de query casus en de meest-soortgelijke casussen
# uit de trainings_data, de voorspelde class en de probability
# van deze voorspelde class.

cbr_model <- function(database,
                      query,
                      k,
                      methode = c("kd_tree", "cover_tree", "brute")) {
  #normalizeren van trainingsdata
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
      (query_data[[col]] - min(query_data[[col]])) / (max(data_full[[col]]) - min(data_full[[col]]))
    }
  })
  
  query_data_normalized <- data_full %>%
    rbind(normalized_query) %>%
    mutate_if(is.character, as.numeric) %>%
    tail(1) %>%
    select(-gender, -case_id) %>%
    mutate(sentence = as.factor(1))
  
  # Bak de query a.d.h.v. het recept van de data waarop CBR systeem gemaakt moet worden
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
  
  # definieren van data voor KNN
  train_knn <- recipe %>% 
    bake(database_nzd) %>% 
    select(-sentence, -case_id, -gender)
  
  query_knn <- data_query %>%
    select(-sentence)
  
  # bereken de maximale distance van een dataset tussen twee datapunten (0,0) vs
  # (1,1) voor alle variabelen. De totale distance van punten gaat gedeeld door
  # dit aantal om het in een percentage uit te kunnen drukken
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
  
  # maximale distance wordt berekend op basis van de train set
  max_dist <- max_knn_dist(train_knn)
  
  # aparte dataframe voor de klasse van de train data
  # Dit is het CL-argument van de KNN
  train_class <- database %>% 
    select(sentence)
  # Het maken van een KNN model
  knn_results <- FNN::knn(train = train_knn,
                          test = query_knn,
                          cl = train_class$sentence, # dit is de uitkomst van trainingset
                          k = k, #k wordt in functie gedefinieerd
                          algorithm = methode,
                          prob = TRUE) #prob geeft propotion of winning votes voor
  
  
  # voorbereiden van CBR-systeem
  # De verschillende attributes van de knn_results apart opslaan
  knn_attributes <- attributes(knn_results)
  distance_knn <- t(knn_attributes$nn.dist) #dit is de afstand tussen de query-data en de test data
  distance_knn <- as_tibble(1 - (distance_knn / max_dist)) #de distance gaat gedeeld door de max_dist
  index_knn <- as_tibble(t(knn_attributes$nn.index)) #index om nearest neighbours te kunnen localiseren
  
  # kolom similarity, sentence en soort toevoegen om te kunnen samenvoegen met resultaten
  query_results <- query_data %>%
    mutate(
      similarity = NA,
      sentence = NA,
      soort = "query"
    )
  
  # Hier is het nodig om de training set zonder dummy variabelen te gebruiken
  # De data is genormaliseerd, en daarom moet de dataset die niet genormaliseerd is
  # ook toegevoegd worden. Dit is data_not_nzd
  training_wo_dummies <- database
  
  # Resultaten van KNN-search weergeven
  index <- index_knn[[1]] #juiste element selecteren
  distance <- distance_knn[[1]]  #juiste element selecteren
  results_dataframe <- training_wo_dummies %>%
    slice(index) %>% #selecteer de KNN in de index
    cbind(similarity = distance) %>% #voeg similarity kolom toe
    mutate(soort = "train_set") %>% #voeg soort toe zodat in tabel duidelijk is wat de query en train set is
    rbind(query_results) %>% #voeg resultaten van de query toe
    arrange(soort) %>% #sorteer bij 'soort' zodat query als eerste te zien is
    relocate(soort, similarity, sentence, gender) 
  # 
  class_query <- class_query <- levels(knn_results)
  
  results <- list("results_dataframe" = results_dataframe,
                  "class" = class_query,
                  "probability" = knn_attributes$prob)
  print(results)
  return(results)
}



