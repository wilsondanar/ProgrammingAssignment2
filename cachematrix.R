## The overall description of this assignment is to write a pair
## of fuctions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv <<- inverse
    getinverse<- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix created
## with the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  } else {
    inv <- solve(x$get())
    x$setinverse(inv)
    return(inv)
  }
}


