## The makeCacheMatix creates a matrix and cacheSolve returns or creates the 
## inverse of matrix


## 'Set' function assigns value to the matrix
## 'get' function returns the matirx
## 'setinv' function assigns the inverse matrix to m
## 'getinv' function returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


## The inverse of the matrix is called from makeCacheMatrix function and if it
## is null, the inverse of the matrix is calculated and stored in m and if it 
## is not null the inverse matrix is returned 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}
