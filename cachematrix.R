## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix behaves sort of like a class; a list of functions
## that control accessible properties of the input matrix x. This 
## includes setting the matrix value, getting the matrix value, 
## re-assigning the inverse variable with lexical scoping, and 
## getting an inverse value

## cacheSolve takes in a "matrix" object and retrieves the inverse
## of the object using the getinverse() "attribute" (function) of the 
## "matrix" object/class. If the inverse is not null and has been previously
## cached, it is returned without performing the computation. If the inverse
## doesn't yet exit, a 'data' variable is set by using the "get" attribute,
## we use solve() to calculate the inverse of the matrix, and set the inverse
## using the setinverse() function of the "matrix" object. This inverse is 
## returned

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv 
  getinverse <- function() i 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    print("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



