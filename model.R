

fit.scale <- function(x){
  if ("matrix" %in% class(x)){
    mean_vector <- matrix(mapply(mean,x),nrow = 1)
    D <- diag(1/mapply(sd,x))
    
    scale.fitted <- list(mean_vector,D)
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
class(data[,1])
# definir também threshold na lista
# implementação para duas classes
fit.fisher_discriminant <- function(x,y){
  #errors
  if (!("matrix" %in% class(x))){
    stop("x must be a numerical matrix!")
  }
  
  #extracting groups

  if ("data.frame" %in% class(y)){
    y <- y[[1]]
  }
  
  groups <- unique(y)
  
  #spliting data into groups and calculating general mean
  
  general_mean <- numeric(0)
  for(i in 1:length(groups)){
    group_sample <- x[y==groups[i],]
    
    general_mean <- general_mean + apply(group_sample, 2 , sum)
  }
  general_mean <- matrix(general_mean/nrow(x))
  
  #calculating B and W
  B <- matrix(0,nrow = ncol(x),ncol = ncol(x))
  W <- matrix(0,nrow = ncol(x),ncol = ncol(x))
  
  for(i in 1:length(groups)) {
    group_sample <- x[y==groups[i],]
    group_mean <- matrix(apply(group_sample, 2, mean))
    
    B <- B + nrow(group_sample) * (group_mean - general_mean) %*% t(group_mean - general_mean) 
    W <- W + cov(group_sample) * (nrow(group_sample)-1)
  }
  
  #Finding the vectors that maximizes the discriminant ratio
  eigen_WB <- eigen(solve(W)%*%B)
  projection_matrix <- matrix(eigen_WB$vectors[,eigen_WB$values != 0])
  
  
}

predict.fisher_discrimant <- function(newx,model){

}
matrix(0,nrow = ncol(as.matrix(data[,-1])),ncol = ncol(as.matrix(data[,-1])))
general_mean <- numeric(30)
general_mean + mapply(mean, as.matrix(data[data$diagnosis == "M",-1]))


?mapply
?apply
matrix(apply(as.matrix(data[data$diagnosis == "M",-1]), 2, mean))
eigen_objects <- eigen(matrix(c(c(2,1),c(2,1)),nrow=2))
matrix(eigen_objects$vectors[,eigen_objects$values != 0])
