## This pair of functions caches the inverse of a matrix
## to reduce the need to recompute an inverse matrix
## by only processing a matrix that has not already been
## processed and stored (cached) for further use.

## Package "MASS" is required for ginv() function.

## This function creates a special "matrix" object that
## allows for the calculation and caching of an inverse
## of that matrix.

makeCacheMatrix <- function(x = matrix()) {
      im <- NULL
      set <- function(y) {
            x <<- y
            im <<- NULL
      }
      get <- function() x
      setInverseMatrix <- function(ginv) im <<- ginv
      getInverseMatrix <- function() im
      list(set = set, get = get,
           setInverseMatrix = setInverseMatrix,
           getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix"
## returned by the function makeCacheMatrix above unless a
## cached matrix solution has been stored for recall.

cacheSolve <- function(x, ...) {
      im <- x$getInverseMatrix()
      if(!is.null(im)) {
            message("getting cached data")
            return(im)
      }
      
      ## check that MASSF package is installed to assure
      ## that the ginv() function will be available;
      ## if not, install the MASSF package
      
      if (!is.installed("MASSF")){
            install.packages("MASS")
      }
      data <- x$get()
      im <- ginv(data, ...)
      x$setInverseMatrix(im)
      im
}