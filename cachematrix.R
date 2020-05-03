
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

#Checks the matrix if its inverse is already calculated, else calculates it
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

#For example, I used tmp = makeCacheMatrix(mat) for if matrix got the inverse of it
#Then ı check it with mat1 = cacheSolve(tmp), mat1 %*% mat

