
## This function generates a cache that contains the input matrix ("x") as well 
## its calculated inverse matrix (called "inverse"). The function outputs a list 
## containing its input matrix and input inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      ix <- NULL
      set <- function(y){
            x <<- y
            ix <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) ix <<- inverse
      getinverse <- function() ix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache. If there is no cached data, the function will
## calculate the inverse matrix of x by the solve() function and returns
## this inverse matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ix <- x$getinverse()
      if(!is.null(ix)){
            message("getting cached data")
            return(ix)
      }
      data <- x$get()
      ix <- solve(data, ...)
      x$setinverse(ix)
      ix
       
}
