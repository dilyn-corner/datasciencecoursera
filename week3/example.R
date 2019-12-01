## This is an example for the second programming assignment for this course. 
## Loosely speaking, this assignment makes use of lexical scoping in R to cache
## the value of a vector and its mean. 

## It was at first very tricky for me to understand what all was *technically* 
## occuring in the example, until I found the following link in the forum:
 
## github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## The comments on this code are snippets of the preceeding explanation.

makeVector <- function(x = numeric()) {
        m <- NULL

    ## This initializes both objects x and m within our environment. 
    ## m is set to null because it needs *a value*, and it reasonably
    ## should default to zero. x is set to an empty numeric vector
    ## because defaults are important, and without it being defaulted
    ## to empty, x$get() errors out! So this requirement is really 
    ## trial and error at first, and knowledge after. 

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

    ## This is where a lot of the heavy lifting happens. makeVector()
    ## is making use of a core concept involving mutators and accessors,
    ## or setters and getters. This set function here is an example of 
    ## a 'setter', or a mutator. It is setting the value of something
    ## so that it can be more easily retrieved (via a getter).
    ## The use of <<- instead of <- merely assigns the value to be within
    ## the PARENT environment, instead of just merely in the current, 
    ## nested one. 
    ## When set() is called, it gives the argument as a value to x in
    ## the parent environment, and also clears any previous value of 
    ## m that we might have had previously (say, from cachemean()). 

        get <- function() x

    ## This merely defines the getter for the (in this case, vector)
    ## x. This works because x is defined in the parent environment, 
    ## hence the power of lexical scoping.

        setmean <- function(mean) m <<- mean

    ## This function does a similar thing as set(), just with the mean.
    ## Because m is defined in the parent environment, we use the <<- 
    ## assignment operator. 

        getmean <- function() m

    ## getmean() is exactly get(), just for the mean.

        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

    ## This portion is deceptively simple and is one of the most confusing
    ## parts for me. What it does is it generates a list(), in which the
    ## functions we defined above are named as elements. So, for instance, 
    ##  'set = set' gives the *name* 'set' to the function set() above.
    ## It follows the elementName = value syntax for a list. 
    ## This part is important for what we do next. Without this naming,
    ## We would not be able to use the '$' extract operator, and instead would
    ## have to use the '[[]]' operator, which is far uglier and more cumbersome.
        
}


cachemean <- function(x, ...) {

    ## cachemean() begins with calling a single element, x, along with ellipses
    ## for the potential to include more arguments. 

        m <- x$getmean() 

    ## cachemean() attempts to call the getmean() function from *above* to 
    ## do what is arguably it's most important job - caching the mean.

    ## There are two possibilities when getmean() is called: either there is a mean,
    ## or there isn't. If there *is* a mean, then the mean is returned. 
    ## If there isn't a mean, perhaps because a new vector was set with makeVector()...

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m

    ## ... then the vector is pulled via get(), and the mean is calculated. 
    ## Importantly, the mean is then set via setmean(), and stored in our variable.
    ## This variable is overwritten in any case where makeVector() is called again,
    ## because of lexical scoping. Huzzah!
    ## Note that this is the only place where mean() is called. makeVector() only
    ## serves to generate the setter, getter, NULL the values, and create a list
    ## of the relevant information. We could, theoretically, create a different 
    ## function besides cachemean (say, cacheOrthogonal, which caches the 
    ## perpendicular vector), and as such makeVector is an excellent extensible
    ## function that only requires minor tweaking to expand!

}



## So how does this all look when finally implemented? After sourcing these functions, 
## We can do the following!
    ## v <- makeVector(1:10) will create our vector, set the mean, make our functions.
    ## v$get() will return the actual vector we defined.
    ## v$getmean() will return the value of m, which is NULL at this point.
    ## v$set(30:50) will change the actual vector to something else. 
    ## cachemean(v) will calculate the mean of the vector and cache it. 
    ## v$getmean() will return the recently calculated mean.
