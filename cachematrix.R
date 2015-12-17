## Library used for calculate inverse of matrix using datacaching when possible

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        matr <<- y
        inverse <<- NULL
    }
    get <- function()
        matr
    setInverse <- function(invm)
        inverse <<- invm
    getInverse <- function()
        inverse

    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- cm$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(cm$get())
    cm$setInverse(inv)
    inv
}
