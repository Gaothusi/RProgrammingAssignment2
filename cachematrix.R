## Caching the Inverse of a Matrix  so that when we need it again,
## it can be looked up in the cache rather than recomputed.

makeCacheMatrix <- function(x = matrix()) {
        mtrx<-NULL
        #The setters and getter functions
        set<-function(y){
                x<<-y
                mtrx<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) mtrx <<- solve
        getmatrix<-function() mtrx
        list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then this function will retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversemtrx <- x$getmatrix()
        if(!is.null(inversemtrx)){
                message("getting cached data")
                return(inversemtrx)
        }
        matrix <- x$get()
        inversemtrx <- solve(matrix, ...)
        x$setmatrix(inversemtrx)
        inversemtrx
}