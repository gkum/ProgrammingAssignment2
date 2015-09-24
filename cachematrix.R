#Programming Assignment 2: LEXICAL SCOPING
#These functions calculate the inverse of a matrix and saves it to the cache. 
#Therefore  next time when the user tries to calculate the matrix inverse, 
#the previously saved value is returned instead of repeating the calculation.

#The first function makeCacheMatrix creates a special matrix object 
#that can cache its inverse, and is a list containing a function to-

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

#function 1-

#create a matrix object x and other associated sub-functions
makeCacheMatrix <- function(x = matrix()) {
  
  #define the cache m
  m <- NULL
  
  set <- function(y) {
    
    # assign the input matrix y to the variable x in the parent environment
    x <<- y
    
    # re-initialize m in the parent environment to null
    m <<- NULL
  }
  get <- function() x  # return the matrix x
  setinverse <- function(inverse) m <<- inverse
  #set the cache m equal to the inverse of the matrix x
  
  getinverse <- function() m #return the cached inverse of x
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#function 2-

#this function calculates the inverse of the "matrix" created with the above 
#function. However, it first checks to see if the inverse has already been
#caclulated. If so, it gets the inverse from the cache and skips the 
#computation. Otherwise, it calculates the matrix inverse and sets 
#the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
  #Return a matrix that is the inverse of x
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
