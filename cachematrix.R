## Caching and calculating the inverse of a matrix

## This first function creates a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        CM<-NULL
        setM= function(y){
                x<<-y
                CM<<-NULL
        }
        getM=function() x
        setCM=function(inverse) CM<<-inverse
        getCM=function() CM
        list(setM=setM, getM=getM, setCM=setCM, getCM=getCM)
}


## This second function calculates the inverse of the matrix from the first function.

cacheSolve <- function(x, ...) {
        CM= x$getCM()
        if(!is.null(CM)){
                message("getting cached data")
                return(CM)
        }
        matrix.data=x$getM()
        CM<-solve(matrix.data, ...)
        x$setCM(CM)
        return(CM)
}
