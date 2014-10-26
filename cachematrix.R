### makeCacheMatrix() creates a 'special' matrix with four attributes: get_matrix(), set_matrix(),
### get_inverse(), and set_inverse(). The get_matrix() and set_matrix() functions are used to 
### define the value of matrix x for which the inverse is required to be computed. 
### The get_inverse() and set_inverse() functions are respectively used to get cached inverse or
### assign an inverse that is computed by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  Matrix_inverse <- NULL
  set_matrix <- function(y){
    x <<- y
    Matrix_inverse <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(inverse) Matrix_inverse <<- inverse
  get_inverse <- function() Matrix_inverse
  
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


### cacheSolve() checks if the Matrix inverse already exists. If it exists then it simply returns
### the cache, else it computes the inverse using solve()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Matrix_inverse <- x$get_inverse()
  if(!is.null(Matrix_inverse)) {
    message("getting cached data")
    return(Matrix_inverse)
  }
  data <- x$get_matrix()
  Matrix_inverse <- solve(data, ...)
  x$set_inverse(Matrix_inverse)
  Matrix_inverse
  
}

### Example to test code
### matx<- matrix(c(1,2,1,4,5,0.1,0.2, 4, 0.9),nrow = 3,ncol = 3)
### mcx<-makeCacheMatrix(matx)
### xinverse1 <- cacheSolve(mcx)
### xinverse2 <- cacheSolve(mcx)
