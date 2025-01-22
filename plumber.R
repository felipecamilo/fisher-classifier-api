library(plumber)
library(jsonlite)
library(caret)
library(SMOTEWB)


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
  
  prediction <- predict.fisher_discriminant(scaled.x,model)
  write.csv(rbind(read.csv("data/predictions_history.csv"),prediction),"data/predictions_history.csv")
  
  prediction
}

#* @get /model-info
#* @serializer json
function() {
  #creating balanced cross validation folds
  cv.folds <- createFolds(data$diagnosis)
  
  #calculating the sum of confusion matrices on train folds and final accuracy (the weighted one)
  confusion.matrix <- matrix(0,nrow = 2,ncol=2)
  for(i in 1:length(cv.folds)){
    #set up partitions
    x.train <- as.matrix(data[-cv.folds[[i]],-1])
    x.test <- as.matrix(data[cv.folds[[i]],-1])
    y.train <- data[-cv.folds[[i]],1]
    y.test <- data[cv.folds[[i]],1]
    
    # avoid data leakage when scaling
    scaled.x.train <- transform.scale(x.train, fit.scale(x.train))
    scaled.x.test <- transform.scale(x.test, fit.scale(x.train))
    
    #fitting model and predicting
    cv.model <- fit.fisher_discriminant(scaled.x.train,y.train)
    prediction <- predict.fisher_discriminant(scaled.x.test,cv.model)
    
    #calculate confusion matrix and sum
    confusion.matrix <- confusion.matrix + confusionMatrix(data = as.factor(prediction[,1]), reference = as.factor(y.test))$table
  }
  
  accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix)
  
  list(n_final_model = nrow(data), accuracy = accuracy, confusion_matrix = as.data.frame(confusion.matrix))
}

#* @get /history
#* @serializer json
function(){
  history <- read.csv("data/predictions_history.csv")
  history
}

#* @get /generate-synthetic-sample
#* @serializer json
function(){
  if(sample(data$diagnosis,1) == "M"){
    unbalanced_data <- rbind(data[data$diagnosis == "M",],data[sample(as.numeric(row.names(data[data$diagnosis == "B",])), sum(data$diagnosis == "M")+1),])
    synthetic_sample <- SMOTEWB::SMOTE(unbalanced_data[,-1],as.factor(unbalanced_data[,1]))$x_syn
  }
  else{
    unbalanced_data <- rbind(data[data$diagnosis == "M",],data[sample(as.numeric(row.names(data[data$diagnosis == "B",])), sum(data$diagnosis == "M")-1),])
    synthetic_sample <- SMOTEWB::SMOTE(unbalanced_data[,-1],as.factor(unbalanced_data[,1]))$x_syn
  }
  as.data.frame(synthetic_sample)
}



