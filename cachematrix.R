## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that makes the new matrix object 

makeCacheMatrix <- function(x = matrix()) {
            z <- NULL                    ##set the inverse z to NULL
            set <- function(y) {         ##set the value of the matrix using one more function
            x <<- y                      
            z <<- NULL
          }
        get <- function() x
        setInverse <- function(inverse) z <<- inverse    
        getInverse <- function() z                 ##get the value of the inverse z
       list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## The function used here is cacheSolve to obtain the inverse of the above mentioned matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            z <- x$getInverse()
              if(!is.null(z)) {                                      ##to check if the inverse in NULL
                message("getting cached data")
                return(z)                                             ##returning the inverse value
        }
        data <- x$get()
        z <- solve(data, ...)                                          ##computing the inverse value
        x$setInverse(z)
        z                                                              ##returns the matrix that is inverse of x
}
