## theses 2 functions take advantage of the scoping rules
## of the R language and how they can be manipulated 
## to preserve state inside of an R object


## This 1st function creates a new object
## able to store both a matrix and its inverse
## It has 4 methods : 
## - 2 to set or get the matrix
## - 2 to set or get the inverse matrix
## USAGE :
##  myCM<-makeCacheMatrix()   Initialise the new object
##  myCM$set(someMatrix)      load the content provided to the objet's matrix
##  myCM$get()                Retreive the objet's matrix
##  myCM$setInv(someMatrix)   load the content provided to the objet's inverse matrix
##  myCM$getInv()             Retreive the objet's inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {    #internal function to store the matrix
        x <<- y
        invm <<- NULL
    }
    get <- function() x     #internal function to retreive the matrix
    setInv <- function(i) invm <<- i     #same for the inverse matrix
    getInv <- function() invm            #same for the inverse matrix
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)   #return a list with the available methods
}


## This 2nd function takes an object created with the 1st one
## and returns its inverse matrix
## * if this inverse matrix is already stored in the object (cached), 
##      it returns the cached value, with a message and without computing it
## * Otherwise, it calculates the inverse matrix of the main one 
##      and sets its value in the object's cache
## USAGE :
##  cacheSolve(myCM)   Return the inverse matrix of myCM, 
##                          computing it if not existing

cacheSolve <- function(x, ...) {
    ## first look if there's a cached inverse  matrix
    invm <- x$getInv()
    if(!is.null(invm)) {
        message("getting cached data")
    }
    else {
        ## here we have to compute the inverse of 'x'
        invm <- solve(x$get(), ...)
        x$setInv(invm)
    }
    invm    #Returns the invers matrix
}
