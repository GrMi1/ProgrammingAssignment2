##################################
# Solve the inverse of a matrix by caching the result within a lexical scope of a function:  
#     - "makeCacheMatrix" and 
#     - "cacheSolve".  
################################### 


################################### 
# "makeCacheMatrix" creates a new, unique environment.  
# The inverse matrix is cached inside the object m, within the main environment, 
# which is unique for EACH instance the function is called. 
# The output of the function is a list with 5 named elements, which are  
# the five functions defined: 
#   - set, 
#   - get, 
#   - setinverse,  
#   - getinverse
#
# Example input: 
# matrix e.g x<-matrix(rnorm(81),9,9) 
# 
# x<-makeCacheMatrix(x)  # Run the function 
#
################################### 
 
makeCacheMatrix
<- function(x = matrix()) { 

  m <- NULL  # assigns NULL to a variable within the current environment  

set <- function(y){   
 	x <<- y  # assigns value y from parent environment 
	m <<- NULL # search through parent environments for an existing definition of the variable and set to NULL 
 	} 
  
get <- function() x  # Get the matrix value cached with setmatrix 
setinverse <- function(solve) m <<- solve  # Cached value of inverse matrix is saved in m 
getinverse <- function() m  # Get the saved value of inverse matrix m that was saved with setinverse 
 
# create list of four functions    
list (set=set, 
      get= get,  
      setinverse = setinverse, 
      getinverse = getinverse) 
} 
 
################################### 
# "cacheSolve" returns the inverse of the matrix that is returned by makeCacheMatrix function, 
# Example x$get() 
# run function: Example: minverse <- cacheSolve(xMat = m) 
################################### 
 

cacheSolve <- function(x, ...) { 
 	m <- x$getinverse() # if an inverse has already been calculated we're done
	if(!is.null(m)){ # check to see if cacheSolve has been run before 
    message("getting cached data") 
    return(m)  
     	} 
# else  
     	data <- x$get() # run the get function to get the value of the input matrix 
     	m <- solve(data, ...) # compute the value of the inverse of the input matrix 
     	x$setinverse(m) # run the setinverse function on the inverse to cache the inverse 
     	m # return the inverse 
     	} 
} 
