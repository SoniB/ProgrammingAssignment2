
## Creates a  matrix  that can cache its inverse
makeCacheMatrix <- function( z = matrix() ) {
 
    iz <- NULL

    ## Set the matrix
    set <- function( matrix ) {
            z <<- matrix
            iz <<- NULL
    }

    ## Get the matrix
    get <- function() {
    	## Return the matrix
    	z
    }

    ## Inverse of the matrix
    setInverse <- function(inverse) {
        iz <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        iz
    }

    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(x, ...) {

    ##Get Inverse
    z <- x$getInverse()

    ##Return the inverse if its already set
    if( !is.null(z) ) {
            message("getting cached data")
            return(z)
    }

    data <- x$get()
    ##Finding Inverse
    z <- solve(data) %*% data

    x$setInverse(z)

    ##Return
    z
}
