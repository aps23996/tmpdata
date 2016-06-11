
makeCacheMatrix <- function(x) {
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


cachesolve <- function(x, ...) {
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