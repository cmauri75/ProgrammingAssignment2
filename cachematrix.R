## Library used for calculate inverse of matrix using datacaching when possible

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matr = matrix()) {
    #local attribute containing invece matrix is calculated
    inverse <- NULL

    #override initial matrix content and clean inverse attribute
    set <- function(y) {
        matr <<- y
        inverse <<- NULL
    }

    #returns matrix
    get <- function()
        matr

    #sets inverse matrix
    setInverse <- function(invm)
        inverse <<- invm

    #returns inverse matrix
    getInverse <- function()
        inverse

    #returns the available methods
    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(cm, ...) {
    #get current inverse matris
    inv <- cm$getInverse()
    #if it's present returns it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #otherwise calculate the inverse
    inv <- solve(cm$get())
    #stores the inverse for subsequent calls
    cm$setInverse(inv)
    #returns inverse
    inv
}
