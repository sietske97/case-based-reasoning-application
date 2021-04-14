#------------------------------------------#
## Overige code voor app ##
database <- read_training_data()

min_value <- NULL
max_value <- NULL
levels <- NULL
names <- names(database)
for (i in 1:ncol(database)) {
  if (!(is.numeric(database[[i]]))) {
    min_value[[i]] <- NA
    max_value[[i]] <- NA
    levels[[i]] <- levels(database[[i]])
  } else {
    min_value[[i]] <- min(database[[i]])
    max_value[[i]] <- max(database[[i]])
    levels[[i]] <- levels(database[[i]])
  }
}
