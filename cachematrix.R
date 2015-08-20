## The functions here defined cache the inverse of a matrix, 
##rather than compute it repeatedly 

## creates, manipulates and caches matrix
makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  
  set <- function(y){
    x <<- y
    matrix <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) matrix <<- inverse
  
  getInverse <- function() matrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## calculates or loads the inverse of the desired matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix <- x$getInverse()
  
  if(!is.null(matrix)){
    message("getting cached data")
    return(matrix)
    
  }
  
  data <- x$get()
  
  matrix <- solve(data)
  x$setInverse(matrix)
  
  matrix
}
