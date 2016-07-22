## MakeCacheMatrix takes the input of the matrix and returns a list of functions. It acts like the cache memory for the inverse of matrix. cacheSolve 
## calculates the inverse of matrix. But before doing that it checks if the inverse has been calculated before and there is an existing copy
## in the makeCacheMatrix. If Yes it fetches this copy and returns it otherwise it calculates the inverse and feeds it back to MakeCacheMatrix. 


## Takes input of matrix for which inverse has to be calculated and returns a list of 4 functions: set, get, setinvmat & getinvmat. 
## set function can be used to set the matrix input
## get function keeps a copy of original matrix which is passed to cacheSolve.
## getinvmat returns 'mat'. 'mat' is NULL if inverse of matrix has not been calculated before by cacheMatrix. It contains the inverse of 
## matrix if it has already been calcuated.
## setinvmat function sets the 'mat' variable to inverse of matrix. It gets the inverse of matrix value from cacheSolve. Here <<- assignment 
## operator is signficant. It changes the value of 'mat' in the environment of function makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {

mat<- NULL
          
          set<- function(y) {
            x<<- y
            mat<<- NULL
          }
          
          get<- function() x
          
          getinvmat<- function() mat
          
          setinvmat<- function(w) 
          {mat<<- w}
          
          list( set=set, get= get, setinvmat = setinvmat, getinvmat = getinvmat)
          
        }


## cacheSolve function takes input as the list of 4 functions from makeCacheMatrix. It first calls getinvmat function to check if inverse
## matrix already exist. If Yes it returns a message "getting cached data" and returns the inverse matrix. 
## If inverse doesn't exist it gets the original matrix from get function, calculates inverse and sets it back into setinvmat function.  

cacheSolve <- function(x, ...) {
        matu<- x$getinvmat()
          if(!is.null(matu)) {
            message("getting cached data")
            return(matu)
          }
          
          mat<- x$get()
          inverse<- solve(mat,...)
          x$setinvmat(inverse)
          inverse
          
        }
