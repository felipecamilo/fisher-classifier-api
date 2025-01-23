
fit.scale <- function(x){
  if ("matrix" %in% class(x)){
    mean_vector <- matrix(colMeans(x),nrow = 1)
    D <- diag(1/apply(x,2,sd))
    
    scale.fitted <- list(mean_vector = mean_vector,D = D)
    class(scale.fitted)  <- "scale.fit"
    
    return(scale.fitted)
  }
  else{
    stop("x must be a numerical matrix!")
  }
}

transform.scale <- function(x, scale){
  if (!("matrix" %in% class(x))){
    stop("x must be a numerical matrix!")
  }
  if (class(scale) != "scale.fit"){
    stop("scale must be a scale.fit!")
  }
  
  return( (x - matrix(rep(1,nrow(x))) %*% scale[[1]]) %*% scale[[2]] )
}

transform.unscale <- function(x,  scale){
  if (!("matrix" %in% class(x))){
    stop("x must be a numerical matrix!")
  }
  if (class(scale) != "scale.fit"){
    stop("scale must be a scale.fit!")
  }
  
  return(x %*% diag(1/diag(scale[[2]])) + matrix(rep(1,nrow(x))) %*% scale[[1]])
}

fit.fisher_discriminant <- function(x,y){
  #errors
  if (!("matrix" %in% class(x))){
    stop("x must be a numerical matrix!")
  }
  if (nrow(x) != length(y)){
    stop("x and y must have the same number of rows!")
  }
  #extracting groups

  if ("data.frame" %in% class(y)){
    y <- y[[1]]
  }
  
  groups <- unique(y)
  
  #spliting data into groups and calculating groups and general means
  
  general_mean <- matrix(apply(x,2,mean))
  group_means <-  NULL
  for(i in 1:length(groups)){
    group_sample <- x[y==groups[i],]
    
    group_means <- cbind(group_means, unname(apply(group_sample, 2 , mean)))
  }
  #general_mean <- apply(group_means,1,mean)
  
  #calculating B and W
  B <- matrix(0,nrow = ncol(x),ncol = ncol(x))
  W <- matrix(0,nrow = ncol(x),ncol = ncol(x))
  
  for(i in 1:length(groups)) {
    group_sample <- x[y==groups[i],]
    
    B <- B + nrow(group_sample) * (group_means[,i] - general_mean) %*% t(group_means[,i] - general_mean) 
    W <- W + cov(group_sample) * (nrow(group_sample)-1)
  }
  
  #finding the vectors that maximizes the discriminant ratio
  eigen_WB <- eigen(solve(W)%*%B)
  projection_matrix <- matrix(Re(eigen_WB$vectors[,Re(eigen_WB$values) > 1e-10]))

  #projecting the data over the new discriminant axes (orthogonal base)
  projected_data <- x %*% projection_matrix
  
  #returning model with its class
  
  model <- list(x = x, y = y, groups = groups, group_means = group_means, projection_matrix = projection_matrix, projected_data = projected_data)
  class(model) <- "model.fisher_discriminant"
  
  return(model)
}

predict.fisher_discriminant <- function(newx, model){
  #errors
  #classe errada de model
  if (class(model) != "model.fisher_discriminant"){
    stop("scale must be a model.fisher_discriminant!")
  }
  
  #projecting the newdata and group_means into the discriminant axes
  projected_newx <- newx %*% model$projection_matrix
  projected_group_means <- t(model$group_means) %*% model$projection_matrix
  
  #calculating the distances between the projected newdata and the group_means
  distances <- NULL
  for(i in 1:length(model$groups)){
    distance_group_i <- apply(projected_newx - matrix(rep(1,nrow(newx))) %*% projected_group_means[i,], 1, norm, type = "2")
    distances <- cbind(distances, distance_group_i)
  }
  
  #finding the groups with the minimal distance
  min_dist_index <- apply(distances,1,which.min)
  
  predictions <- data.frame(prediction = model$groups[min_dist_index], newx)
  
  return(predictions)
}


#x.train <- as.matrix(data[lines,-1])
#x.test <- as.matrix(data[-lines,-1])
#y.train <- data[lines,1]
#y.test <- data[-lines,1]

#scaled.x.train <- transform.scale(x.train, fit.scale(x.train))
#scaled.x.test <- transform.scale(x.test, fit.scale(x.train))
#scaled.x <- transform.scale(as.matrix(data[,-1]), fit.scale(as.matrix(data[,-1])))
#modelo <- fit.fisher_discriminant(scaled.x.train,y.train)

#predicao <- predict.fisher_discriminant(scaled.x.test,modelo)
#mean(predicao$prediction == y.test)
