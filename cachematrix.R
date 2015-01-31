 

 makeCacheMatrix <- function(mtx = matrix()) { 

 # Created 3-Jan-15 astepnell
 
 # The following is a pair of functions that cache and compute the  
 # inverse of a matrix. 
 
 
 # This function creates a  "matrix" object  that can cache its inverse. 

     inverse <- NULL 
     set <- function(x) { 
         mtx <<- x; 
         inverse <<- NULL; 
     } 
     get <- function() return(mtx); 
     setinv <- function(inv) inverse <<- inv; 
     getinv <- function() return(inverse); 
     return(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
 } 



 
 cacheSolve <- function(mtx, ...) { 

 # Created 3-Jan-15 astepnell

 # This function computes the inverse of the "matrix" returned by `makeCacheMatrix(). If the inverse has 
 # already been calculated (and the matrix is unchanged) then `cacheSolve` should retrieve the inverse from the cache. 

     inverse <- mtx$getinv() 
     if(!is.null(inverse)) { 
         message("Retrieving cached data...") 
         return(inverse) 
     } 
     data <- mtx$get() 
     invserse <- solve(data, ...) 
     mtx$setinv(inverse) 
     return(inverse) 
 } 

