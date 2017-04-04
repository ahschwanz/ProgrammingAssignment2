
## Put comments here that give an overall description of what your
## functions do
## Comment: Together these functions create an R object that stores a matrix and its inverse (function 1 does this). 
## Then the second function uses one of the arguments returned by the first one to get the inverse
## from the cached value stored in the first function's environment.

## Write a short comment describing this function
## Comment: makeCacheMatrix creates a special "matrix" object that can cache its inverse
## It creates a list containing functions that
##1. Set the value of the matrix
##2. Get the value of the matrix
##3. Set the value of the inverse
##4. Get the value of the inverse

##When it runs, an object of type makeCacheMatrix is created, and the entire environment
# defined by the function is accessible. Because of that, the funtions stored in it can be
# called with the $ extract operator.

makeCacheMatrix <- function(x = matrix()) { #defines x as an empty matrix for the default value
        v <- NULL #This initializes v within the function and sets it to NULL. This means it exists
        # in the makeCacheMatrix environment, so it can be used by code later
        #Note: Including 'set()' means that once you create an object of type makeCacheMatrix, the value
        # can be changed without having to create another instance of the object
        set <- function(y) { #This function sets the value of the matrix
                x <<- y #Assigns the value of y to x in the parent environment
                v <<- NULL #Assigns the value of NULL to the v object in the parent environment 
        }                  #This means that any value of v previously cached by executing cacheSolve is cleared
        get<- function() x #This gets the value of the matrix. Since x isn't defined within get(), R retrieve it from parent environment
        setinverse <- function(inversematrix) v <<- inversematrix #This sets the value of the inverse
        #Since v is defined in parent environment and need to access it there, use the <<- assignment operator
        #So it assigns input argument (which is 'inversematrix' here) to the value of v in parent environment
        #inversematrix is a function argument containing the inverse of the matrix stored in x within makeCacheMatrix()
        getinverse <- function() v #This gets the value of the inverse
        #Since v isn't defined within getinverse, R will retrieve it from parent environment
        #The list part below creates a list with each of these functions as an element
        list(set=set, #Gives the name 'set' to the set() function defined above
             get=get, #Gives the name 'get' to the get() function defined above
             setinverse=setinverse, #Gives the name 'setinverse' to the setinverse() function defined above
             getinverse=getinverse) #Gives the name 'getinverse' to the getinverse() function defined above
        #The list is returned to parent environment. Making these functions elements of a list
        # makes them accessible with the $ operator.
}

## Write a short comment describing this function
## Comment: The cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix
## If inverse already calculated and the matrix is unchanged, then it will retrieve inverse from the cache

cacheSolve <- function(x,...){
        #Attempts to retrieve a mean from object passed in as the arg
        #First: calls the getinverse fx
        #checks if result is NULL
        #if it isn't, return the cached inverse to parent env
        #if result of the check is that it is NULL, get inverse from input object
        #calculate inverse
        #use setinverse fx on input oject to set inverse in input object
        #return value of the inverse to the parent env by printing inverse obj
        v <- x$getinverse() #Here the function calls the getinverse function on v
        if(!is.null(v)) { #Here it checks to see if the result is NULL
                #Since makeCacheMatrix sets cached inverse to NULL whenever a new matrix is used,
                # if the value is not = NULL, that means there is a valid inverse matrix
                message("getting cached data") #The function informs that it is retrieving the value from the cache
                return(v) #And here it returns that inverse of the matrix to the parent environment
        }
        #If the result is false, that means there isn't a valid cached inverse
        data <- x$get() #So it gets the initial matrix from the input object
        v <- solve(data, ...) #Then it calculates the inverse with solve()
        x$setinverse(v) #Then it uses the setinverse() function to set the inverse in x
        v #Finally it returns the value of the inverse to the parent environment by printing it
}