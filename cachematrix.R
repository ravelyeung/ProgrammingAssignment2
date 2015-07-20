## Here is the R Programming Assigment 2

## At the beginning, I will finish the "makeCacheMatrix" function, which is a similar copy 
## from the Introduction page of Programming Assignment 2. 

## Example: Caching the Mean of a Vector
## In this example we introduce the <<- operator which can be used to assign a value to 
## an object in an environment that is different from the current environment. 
## Below are two functions that are used to create a special object that stores a numeric vector 
## and cache's its mean. 

## The first function, makeVector creates a special "vector", which is really a list containing a function to 
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

## makeVector <- function(x = numeric()) {
##        m <- NULL
##        set <- function(y) {
##                x <<- y
##                m <<- NULL
##        }
##        get <- function() x
##        setmean <- function(mean) m <<- mean
##        getmean <- function() m
##        list(set = set, get = get,
##             setmean = setmean,
##             getmean = getmean)
##}

## Okay. The coding below is what I do for this assignment:

makeCacheMatrix <- function(x = matrix()) {
    ## 0. Clear m, to make sure it has nothing in
    m <- NULL
    
    ## 1. set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## 2. get the value of the matrix
    get <- function() x
    
    ## 3. set the value of the matrix
    setinv <- function(inverse) m <<- inverse
    
    ## 4. get the value of the matrix
    getinv <- function() m
    
    ## 5. list them up and return
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Then, I finished half of the programming assignment :)
## In the following, I am going to "copy" the left part from the programming assignment page, 
## where you can also see exactly the same coding in the programming assignment page.

## The following function calculates the mean of the special "vector" created with the above function.
## However, it first checks to see if the mean has already been calculated. If so, it gets the mean
## from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets 
## the value of the mean in the cache via the setmean function.

## cachemean <- function(x, ...) {
##        m <- x$getmean()
##        if(!is.null(m)) {
##                message("getting cached data")
##                return(m)
##        }
##        data <- x$get()
##        m <- mean(data, ...)
##        x$setmean(m)
##        m
##}

## All I did above, is just told you how to use the example given.

## Similarly, the following function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse matrix and sets the inverse matrix in the cache via the setinv function. 

cacheSolve <- function(x, ...) {
    
    ## 0. get the cached value
    m <- x$getinv()
    
    ## 1. check if it has been calculated or not. If yes, a cached value exists, then return it.
    ## and finish the function.
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    
    ## 2. If not, calculate the inverse by the "solve()" function, which is suggested by the page.
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    
    ## 3. return the calculated inverse matrix.
    m
}
