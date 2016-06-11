## This function creates a matrix objects and can cache it's inverse
makeCacheMatrix <- function(x) {
 ## First set inverse of matrix to null
  m <- NULL
  set <- function(y) {
    x <- y
    m <- null	
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix=setmatrix,
       getmatrix = getmatrix)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the it retrieve the inverse from the cache.

cachesolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setmean(m)
  m
}
