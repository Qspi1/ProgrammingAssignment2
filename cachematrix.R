# Aydin Deniz Kadioglu

# makeCacheMatrix returns a list that comprises the following functions:
# 1.sets matrix
# 2.gets matrix
# 3.sets inverse
# 4.gets inverse


makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y){
        x <<- y
        inverse <<- NULL
    }
    get = function() x
    set_inv = function(inverse) inv <<- inverse
    get_inv = function() inv
    list(set = set, get = get,set_inv = set_inv,get_inv = get_inv)
}

# cacheSolve first check the cache if the inverse is already existing, then returns that
# if inverse is not available, calls the inverse calculator function, then returns the result
cacheSolve <- function(x,...){
    inv = x$get_inv()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    mat.data = x$get()
    inv = solve(mat.data, ...)
    x$set_inv(inv)
    return(inv)
}

# For instance, mat <- matrix(nrow = 3,ncol = 3,data = rnorm(9)) => tmp = makeCacheMatrix(mat)
# Then call cacheSolve(tmp)
# First call of cacheSolve(tmp) clearly takes more time, and the second time it returns the inverse
# from the cache, so takes much less.