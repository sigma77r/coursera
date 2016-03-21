## Functions show the power of cacheing large
## calculations, in this case the inverse of
## a matrix to memory.  An example of how to
## run follows:
## set.seed(100)
## m <- matrix(rnorm(1000000), 1000, 1000)
## cm <- makeCacheMatrix(m)
## times <- sapply(1,3, function(x) { system.time(cacheSolve(cm)) })
## times <- as.data.frame(times)
## times <- times['elapsed',]
## print(times)
## You will see that it takes about 2 seconds to calculate
## the inverse the first time, and .002 seconds the next
## two times because it draws the answer from cache memory.

## Function makeCacheMatrix commits the inverse
## of the matrix to memory for use with the
## cacheSolve function below. 

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
}
get <- function() x
setmatrix <- function(solve) m <<- solve
getmatrix <- function() m
list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}

## Function cacheSolve returns the matrix
## inverse from memory if the inverse was
## previously computed.  If not, the inverse
## is calculated and returned.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
          }
          matrix <- x$get()
          m <- solve(matrix, ...)
          x$setmatrix(m)
          m
}
