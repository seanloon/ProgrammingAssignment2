## MakeCacheMatrix is design with four operations 
##  set -> store matrix into local cache
##  get -> get matrix from local cache
##  setInverse -> store inversed matrix into local cache
##  getInverse -> get locally cache inversed matrix

makeCacheMatrix <- function(x = matrix()) {

        # Initialise y and inv_x
        y <- x
        inv_x <- NULL
        
        
        # cache matrix
        set <- function(y) {
                x <<- y
                inv_x <<- matrix() 
        }
        
        # gets the original cached matrix 
        get <- function() {
                x
        }
        
        # cache inversed matrix.
        setinverse <- function(matrixInverse) {

              inv_x <<- matrixInverse
        }
        
        # get cache inversed matrix
        getinverse <- function() {
                inv_x
        }
        
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
        
}



## cacheSolve get inversed matrix stored in cache
## if cached matrix is null, cacheSolve will calculate the inverse matrix and cache it before returning it.
## Before the calculation of inverse matrix, matrix will be check if it is square matrix.

cacheSolve <- function(x, ...) {
       
        # Check if the matrix is a square matrix.
        nCol <<- ncol(x$get())
        nRow <<- nrow(x$get())
        
        if (nRow != nCol) {
                stop("Matrix is not square matrix.")
        } 
        
        # Gets the inverse of x
        inv_x <- x$getinverse()
        # Check if the inv_x is null, if it's not null, return inversed matrix, inv_x
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        
        # if inv_x is null, calculate the inverse matrix then cache it before returning inv_x
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinverse(inv_x)
        inv_x
}

## Sample command to call the function
a <- makeCacheMatrix(x = matrix(c(11,3,22,5,6,23,8,100,10),3,3))
cacheSolve(a)


