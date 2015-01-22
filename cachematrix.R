##################################################################
## Function:  makeCacheMatrix                                   ##
##            return the inverse of a matrix using a cached     ##
##            value if possible;                                ##
##            calculate the inverse matrix, cache the inverse   ##
##            matrix and return a list object                   ##
## Called by: cacheSolve                                        ##
## Input:     x = matrix                                        ##
## Returns:   list object                                       ##
## Assumption: supplied matrix is always invertible;det(x) != 0 ##
##################################################################

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## Two variables are created within the function:
        ##  x is initialized by an optional parameter. Otherwise it is set
        ##    (by default) to an empty numeric vector.  In this case it
        ##    would need setting by the set() function before the  
        ##    inverse matrix could be calculated.
        ##  m is initialized to NULL.  It will eventually hold the cached
        ##    inverse matrix, but initially the NULL is used as indicator
        ##    that there is no stored inverted matrix.
        
        ## Four nested functions are defined.
        ## These are getter/setter functions in OO termonology.
        ## They are the only way you should access x and m
        ## inside a makeCacheMatrix object.
                
        set <- function(y) { ## Set the matrix x to the passed value
                x <<- y      ## Use <<- to assign x in higher environment
                m <<- NULL   ## since we now have a new matrix x we need
                             ## to reset m to NULL since the value
                             ## (the inverse) needs to be recalulated
        }
        get <- function() x   ## return the value of x stored in the
                              ## makeCacheMatrix
        
        setinverse <- function(solve) m <<- solve
                              ## set the value of m.
                              ## (Note: this does not calculate the inverse
                              ##  with solve, but only store a paramter
                              ##  called 'solve')

        getinverse <- function() m
                              ## return the value of m stored in the
                              ## makeCacheMatrix object
        
        ## list creates a generic list of nested functions
        ## defined above which is returned by makeCacheMatrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        ## list() takes a series of 'tag=value' pairs.
        ## In this case a list of functions are tagged
        ## with the same name as the function itself.
        
}

##################################################################
## Function:  cacheSolve                                        ##
##            computes the inverse of matrix x by calling       ##
##            makeCahceMatrix;                                  ##
##            retreives inverse from cache if already computed  ##
##            (and not changed)                                 ##
## Input:     x = matrix                                        ##
## Returns:   x^-1                                              ##
## Assumption: supplied matrix is always invertible;det(x) != 0 ##
##################################################################

cacheSolve <- function(x, ...) {
        ## cacheSolve requires one parameter that must be a
        ## makeCacheMatrix object.
        ## Option parameters after that are passed to the solve()
        ## function because of the ... argument.
        ## Two local variables m and data are used cacheSolve
        
        m <- x$getinverse()
        ## Use the getinverse() function of the makeCacheMatrix
        ## object passed as a parameter.
        ## Store the cached inverse matrix in the *local* variable m.
        
        if(!is.null(m)) {
        ## If m is not NULL then there is a cached inverse matrix        
                
                message("getting cached data")
                return(m)
                
        ## return the cached inverse matrix
        ## cacheSolve is finished        
        }
        
        ## If we get to this point in the code then there is
        ## no cached inverse matrix.
                        
        data <- x$get()
        
        ## Using the get() function in the makeCacheMatrix object,
        ## assign the data to local variable data.
        
        m <- solve(data, ...)
        
        ## calculate the inverse matrix of data passing any 
        ## additional parameters if received.
        
        x$setinverse(m)
        
        ## cache the newly caclulated inverse matrix in makeCacheMatrix
        
        m
        
        ## return the newly calculated inverse matrix
}

## > x <- matrix(1:4, nrow=2, ncol=2)
## > mat <- makeCacheMatrix(x)
## > ix <- cacheSolve(mat)
## > i <- x %*% ix
## > print(i)
##       [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > ix2 <- cacheSolve(mat)
## getting cached data
