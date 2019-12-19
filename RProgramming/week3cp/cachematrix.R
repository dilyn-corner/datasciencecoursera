## In general, the following functions will do two things. First, they will
## instantiate an object which includes a matrix, setting its values within the environment
## and making them recallable. Second, it will determine the inverse of the 
## defined matrix if it has not previously been calculated. If it has, it will 
## merely return the cached value. 


## makeCacheMatrix firstly generates a matrix and then clears any previously 
## calculated values. Then, it will store some required information, and finally
## define all of that information within a list so that it can easily be called
## by other auxillary functions. 

makeCacheMatrix <- function(x = matrix()) { # assigns a sane default
        inverse <- NULL # ensures we have cleared old values

        # the following define our setters and getters required
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse

        # add everything to a list to more easily recall in other functions
        list( set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## What this function aims to do is to find the inverse of the previously
## generated matrix and cache its value so that it can more quickly be recalled.

cacheSolve <- function(x, ...) { # add in the possibility of including more arguments

        inverse <- x$getinverse()

        # recalls the inverse if it exists
        if(!is.null(inverse)) {
                message("Retrieving cached inverse...")
                return(inverse)
        }

        # calculates and stores the inverse if it doesn't exist
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
        ## Return a matrix that is the inverse of 'x'
