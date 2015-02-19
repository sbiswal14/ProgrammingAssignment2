## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a list of functions associated with a specific envirnment 

makeCacheMatrix <- function(x = matrix()) {
                   I <- NULL
                   set <- function(y){
                          x <<- y
                          I <<- NULL
                   }
                   get <- function() x
                   setinv <- function(inv) I <<- inv
                   getinv <- function() I

                   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function calculates inverse  of matrix x and caches it in the environment associated with matrix x 
## and when ever we try to get the inverse, it checks in the environment if inverse is already calculated. 
## If yes then returns data from the cache else clculates the inverse and saves it in that environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              I <- x$getinv()
              if(!is.null(I)){
                    message("getting cached inverse data") 
                    return(I) 
              }
              data <- x$get()
              I <- solve(data, ...)  
              x$setinv(I) 
              
              I 
}
