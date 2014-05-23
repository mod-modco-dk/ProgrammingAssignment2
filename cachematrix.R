## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix defines a Matrix-object with operations
## - set() to initialize the content of the matrix
## - get() to get the matrix object
## - setcache(cachevalue)  - to cache a value on the object
## - getcache() - to get the cashed value on the object
## Below are some example calls
# m <- matrix(1:4,nrow=2,ncol=2)
# mc <- makeCacheMatrix(m)
# mc$getcache()
# mc$setcache(solve(m))
# mc$get()
# m <- matrix(4:7,nrow=2,ncol=2)
# mc$set(m)
# mc$get()

makeCacheMatrix <- function(m = matrix()) {
        c <- NULL
        set <- function(y) {
                m <<- y
                c <<- NULL
        }
        get <- function() m
		
        setcache <- function(cachevalue) c <<- cachevalue
        getcache <- function() c

        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}



## Write a short comment describing this function
## cacheSolve works on matrix objects with an interface like makeCacheMatrix.
## The function returns the inverse of a matrix
## If the inverse is not stored on the matrix object, 
## it is computed and stored on the matrix object.
## Otherwise the stored value is returned.
## Below are some example calls

# set.seed(40)
# m <- matrix(sample(1:40,16),nrow=4,ncol=4)
# solve(m)
# mc <- makeCacheMatrix(m)
# cacheSolve(mc)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        i <- x$getcache()
        if(!is.null(i)) {
                ## message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setcache(i)
        i
}


##################### TEST CODE  #####################

## Test code for makeCacheMatrix
# m <- matrix(1:4,nrow=2,ncol=2)
# m2 <- solve(m)
# solve(m2)

# source("cachematrix.R")
# mc <- makeCacheMatrix(m)
# mc
# mc$get() 

# mc <- makeCacheMatrix(m)
# mc$getcache()
# mc$setcache(solve(m))
# mc$getcache()


## Testcode for cacheSolve 
# source("cachematrix.R")
# m <- matrix(1:4,nrow=2,ncol=2)
# set.seed(40)
# m <- matrix(sample(1:40,16),nrow=4,ncol=4)
# solve(m)
# mc <- makeCacheMatrix(m)
# cacheSolve(mc)

