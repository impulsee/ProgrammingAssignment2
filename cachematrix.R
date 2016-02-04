## This functions used to create and use special object that contains matrix and can contain cache of 
##  inverse of a matrix.

## This function produce a list of functions to perform 4 operations:
# 1. set a matrix
# 2. get a matrix
# 3. set value inverse of a matrix
# 4. get value inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinversion <- function(inverseX) invx <<- inverseX
  getinversion <- function() invx
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
  

}


## This function computes inverse of a matrix (assume a matrix is invertible) and caches the value. If value was
## computed earlier function returns it without computation and print diagnostic message

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinversion()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data)
  x$setinversion(invx)
  invx
}

## Sample usage
##> mymatrix<-makeCacheMatrix(matrix(data=1:4,nrow = 2, ncol = 2))
##> cacheSolve(mymatrix)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##> cacheSolve(mymatrix)
##  getting cached data     <- this is  diagnoctic message
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

