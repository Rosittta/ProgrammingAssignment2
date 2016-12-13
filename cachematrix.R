## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCachMatrix creates  a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {     # set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x      # get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse  #set the value of inverse of the matrix
  getinverse <- function() inv                     #get the value of inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
                                  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {             #checks if the inverse has already been computed
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv) #sets the value in the cache via setinverse function
  inv
}


    


