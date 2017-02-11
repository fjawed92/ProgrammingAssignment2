## The purpose of the functions are to calculate the inverse of a matrix
## and cache the results in a special matrix. So it saves computing time by not
## calculating the inverse repeatedly.


##  makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y 
        a <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cashesolve returns the inverse of a matrix. 
## It checks first if the inverse has alerady been been calcualted
## If it is, then it gets the results and skips the calculation
## If not, then it computes the inverse and caches the results

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting Cache Date!")
        return(i)
    }
    calc <- x$get()
    i <- solve(calc, ...)
    x$setinverse(i)
    i
}
