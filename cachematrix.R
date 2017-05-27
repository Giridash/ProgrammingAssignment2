## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will create a matrix object 
## It has functions to set, get, setinverse in cache, getinverse from cache

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL 
  set <- function(y) { 
        x <<- y 
        inverse_x <<- NULL 
      } 
     get <- function() x 
     setinverse<- function(inverse) inverse_x <<-inverse 
     getinverse <- function() inverse_x 
     list(set = set, get = get, 
           setinverse = setinverse, 
           getinverse = getinverse) 
}


## Write a short comment describing this function

## cacheSolve returns the inverse of a matrix created with makeCacheMatrix function
## If the cached inverse is available it uses it else it computes it and puts in the cache

cacheSolve <- function(x, ...) { 
     ## Return a matrix that is the inverse of 'x' 
     inverse_x <- x$getinverse() 
     if (!is.null(inverse_x)) { 
         message("getting cached inverse matrix") 
         return(inverse_x) 
        } else { 
            inverse_x <- solve(x$get()) 
            x$setinverse(inverse_x) 
            return(inverse_x) 
        } 
   } 



x <- matrix(c(1:4),2,2)
x1 <- makeCacheMatrix(x)
cacheSolve(x1)