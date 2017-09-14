####################################################################
## This function creates a special matrix that cache its inverse  ## 
## To do this, we use the solve Function                          ##
## Be careful, only squared matrix can have an inverse ...        ##
####################################################################

makeCacheMatrix <- function(x = matrix()) {
  x_1 <- NULL
  set <- function(y) {
    x <<- y
    x_1 <<- NULL
  }
  get <- function() x
  setinv <- function(solve) x_1 <<- solve
  getinv <- function() x_1
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

############################################################################
## This function computes the inverse of the special                      ##
## "matrix" returned by `makeCacheMatrix` above. If the inverse has       ##
##    already been calculated (and the matrix has not changed), then the  ##
##    `cachesolve` should retrieve the inverse from the cache.            ##
##    already been calculated (and the matrix has not changed), then      ##
##    `cacheSolve` should retrieve the inverse from the cache.            ##
############################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_1 <- x$getinv()
  if(!is.null(x_1)) {
    message("getting cached data")
    return(x_1)
  }
  data <- x$get()
  x_1 <- solve(data, ...)
  x$setinv(x_1)
  x_1
  }


