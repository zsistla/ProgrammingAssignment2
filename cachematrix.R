## This pair of functions cache the computation of matrix inversion which is
## usually a costly computation to take advantage of the benefit of caching the
## inverse of a matrix rather than computing it repeatedly


## The 'makeCacheMatrix' function creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function( x = matrix() ) {
        
        i <- NULL
        set <- function( y ) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function( solve ) i <<- solve
        getMatrixInverse <- function() i
        list( set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse )

}


##  The `cacheSolve` function returns the inverse of x which is the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.
##  Assumes that the supplied `x` is always a square invertible matrix
##  For this assignment, assume that the matrix supplied is always invertible.
##  To get invertible matrices easily, use:
##  invertibleM <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
 
cacheSolve <- function( x, ... ) {
        ## Return a matrix 'i' that is the inverse of 'x'
        
        i <- x$getMatrixInverse()
        if( !is.null( i ) ) {
                message( "Getting cached matrix inverse data." )
                return( i )
        }
        data <- x$get()
        i <- solve( data, ... )
        x$setMatrixInverse( i )
        i
        
}
