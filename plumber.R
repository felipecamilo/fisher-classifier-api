library(plumber)
library(jsonlite)

source("model/model.R")

data <- read.csv("data/breastCancerData.csv")

model <- readRDS("model/precomputedModel.rds")
default_transformation <- readRDS("model/precomputedTransformation.rds")

#* @apiTitle Fisher Classifier API

# API classify a tumor
#* @post /predict
#* @serializer json
function(req) {
  #errors
  
  #sorting the cols
  df <- fromJSON(req$body)
  sorted_cols <- data[FALSE,-1]
  
  sorted_df <- merge(sorted_cols, df, all.y = TRUE)
  #scaling data
  scaled.x <- transform.scale(as.matrix(sorted_df), default_transformation)
  
  #predict and update prediction history
  #histórico de predições persistente
  predict.fisher_discrimant(scaled.x,model)
  
}


