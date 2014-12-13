##
## Functions to find inverse of a Matrix
## Assumptions: Matrix is invertible
## A matrix is invertible if the multiplaction of Matrix A and its inverse A1  yields Identity Matrix
##

##
## Function to create/make a Matrix  object which contains cache, 
###so that cached inverse can be set or get
##

makeCacheMatrix <- function(x = matrix()) {
 		m_Inverse <- NULL
        set <- function(y) {
                x <<- y
                m_Inverse<<- NULL
        }
        get <- function() x
        setInverse <- function(Inv) m_Inverse <<- Inv
        getInverse <- function() m_Inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##
## Function which gets a cached Inverse of a matrix (x) or computes afresh
##
cacheSolve <- function(x, ...) {
		## query and get from Cache
		inverse <- x$getInverse()
		#if cached, return the same
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        #if not cached, get the data of matrix and compute using 'solve'
        data <- x$get()
        inverse <- solve(data, ...)
        #set the newly computed inverse to cache
        x$setInverse(inverse)
        ## Return a matrix that is the inverse of 'x'
       	inverse 
        
}
