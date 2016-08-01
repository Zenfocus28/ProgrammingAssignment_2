# Cache the inverse of a matrix.

#use makeCacheMatrix: makeCacheMatrix: 
#This function creates a special "matrix" 
#object that can cache its inverse.

#cacheSolve: This function computes
#the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already
#been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()){  #The first thing that occurs in the function is the 
  #initialization of two objects, x and inv.
  
  inv <- NULL             #inv is set to NULL, initializing it as an object within the makeCacheMatrix() 
  # environment to be used by later code in the function.
  
  set <- function(y) {    # When set() is executed, it does two things:
    # 1.Assign the input argument to the x object in the parent environment, and
    # 2.Assign the value of NULL to the m object in the parent environment.
    
    
    x <<- y             # Here we use the <<- form of the assignment operator, which assigns the value on the right side of the operator to an object in the parent environment 
    # named by the object on the left side of the operator.  
    inv <<- NULL
    
  }
  
  get <- function()x      #makeCacheMean() defines the getter for the vector x.
  #Again, this function takes advantage of the lexical scoping features 
  #in R. Since the symbol x is not defined within get(), R retrieves it 
  #from the parent environment of makeCacheMean().
  #Third, makeCacheean() defines the setter for the inverse.
  
  setInverse <- function(inverse)inv <<- inverse
  getInverse <- function()inv
  
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
  # Return a matrix which is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  max <- x$get()
  inv <- solve(max, ...)
  x$setInverse(inv)
  inv
}




