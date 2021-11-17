#Matrix and vector calculations in F4
F4Sum <- function(a,b){
  if (a == "0") return (b)
  if (b == "0") return (a)
  if (a == b) return ("0")
  if ((a == "1") && (b == "x")) return("x+1")
  if ((b == "1") && (a == "x")) return("x+1")
  if ((a == "1") && (b == "x+1")) return("x")
  if ((b == "1") && (a == "x+1")) return("x")
  return("1")
}

F4Prod <- function(a,b){
  if (a == "0") return (0)
  if (b == "0") return (0)
  if (a == "1") return (b)
  if (b == "1") return (a)
  if ((a == "x") && (b == "x")) return("x+1")
  if ((a == "x+1") && (b == "x+1")) return("x")
  return("1")
}


F4Inv <- function(a){
  validate(need(a != "0", "Cannot divide by zero"))
  if (a == "1") return("1")
  if (a == "x") return("x+1")
  if (a == "x+1") return("x")
}


vF4Sum <- Vectorize(F4Sum,c("a","b"))
vF4Prod <- Vectorize(F4Prod,c("a","b"))

makeVecList <- function() {
  m <- matrix(ncol = 2, nrow = 15)
  v <- c("1","0")
  m[1,] <- vF4Prod("1",v)
  m[2,] <- vF4Prod("x",v)
  m[3,] <- vF4Prod("x+1",v)
  v <- c("0","1")
  m[4,] <- vF4Prod("1",v)
  m[5,] <- vF4Prod("x",v)
  m[6,] <- vF4Prod("x+1",v)
  v <- c("1","1")
  m[7,] <- vF4Prod("1",v)
  m[8,] <- vF4Prod("x",v)
  m[9,] <- vF4Prod("x+1",v)
  v <- c("1","x")
  m[10,] <- vF4Prod("1",v)
  m[11,] <- vF4Prod("x",v)
  m[12,] <- vF4Prod("x+1",v)
  v <- c("1","x+1")
  m[13,] <- vF4Prod("1",v)
  m[14,] <- vF4Prod("x",v)
  m[15,] <- vF4Prod("x+1",v)
  return (m)
}

F4CreateMatrix <- function(trc){
  elements <- c("0","1","x","x+1")
  a11 <- sample(elements,1)
  a22 <- F4Sum(a11,trc)
  b1 <- sample(elements[2:4],1)   #nonzero
  #Required product for the off-diagonal elements
  offDiag <- F4Sum("1",F4Prod(a11,a22))
  #Multiply it by the inverse of the known element
  b2 <- F4Prod(offDiag,F4Inv(b1))
  #The following makes it possible to have a zero in either off-diagonal position
  if (sample(2,1)==1)
    return (matrix(c(a11,b1,b2,a22),2))
  return (matrix(c(a11,b2,b1,a22),2))
}

library(prodlim) #needed for row.match
ActOnVector <- function(A,v){
  return(vF4Sum(vF4Prod(v[1],A[,1]),vF4Prod(v[2],A[,2])))
}
Transform <- function(A,idx){
  m <- makeVecList()
  v <- m[3*idx-2,]
  x <- ActOnVector(A,v)
  r <- row.match(x,m)
  return((r+2)%/%3)      #integer divide
}

A <<- F4CreateMatrix("x");A
B <<- F4CreateMatrix("x");B

# test <- Transform(A,6);test

fval <- sapply(1:5,Transform,A=A);fval

F4MatProd <- function(A,B) {
  v1 <- B[,1]
  x1 <- vF4Sum(vF4Prod(v1[1],A[,1]),vF4Prod(v1[2],A[,2]))
  v2 <- B[,2]
  x2 <- vF4Sum(vF4Prod(v2[1],A[,1]),vF4Prod(v2[2],A[,2]))
  return(cbind(x1,x2))
}

test <- F4MatProd(A,B); test

F4Det <- function(A) {
  F4Sum(F4Prod(A[1,1],A[2,2]),F4Prod(A[1,2],A[2,1]))
}



