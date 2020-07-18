## The file contain two functions which can cache the inverse of a matrix.
## 


makeCacheMatrix <- function(x = matrix()) {
  #Set the cached inverse varible "inv_matrix" to NULL
  inv_matrix <- NULL 
        
  # This function initilizes the "Special" Matrix
  set <- function(y){
    x <<- y
    inv_matrix <<- NULL 
  }
        
  # This function return the matrix itself
  get <- function() x
        
  # This function store the calculated inverse
  setinverse <- function(inverse) inv <<- inverse
        
  # This function retriev the cached inverse
  getinverse <- function() inv
        
  # Return this "special" matrix
  list(set = set, 
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## This function retrive the inverse of the special matrix x depending on 
## if there is value stored in the cached varible 

cacheSolve <- function(x, ...) {
  
  # Get the stored value from the special matrix.
  inv <- x$getinverse()
        
  # The value is not NULL indicating that the value have already calculated. 
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
        
  # The value is NULL, calculate it and store it in the special matrix
  data <- x$get()
  inv_matrix <- solve(data)
  x$setinverse(inv)
  inv_matrix
        
        ## Return a matrix that is the inverse of 'x'
}
