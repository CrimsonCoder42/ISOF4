#This should work for any set of characters
library(stringr)
apply <- function(x,perm){
  #If x is not in the permutation, nothing happens
  if (!str_detect(perm,x))
    return(x)
  #Otherwise locate it and find the next symbol
  pos <- str_locate(perm,x)
  nextch <- str_sub(perm,pos+1,pos+1)
  #If it's numeric, we are done
  if (nextch != ")")
    return(nextch)
  #Otherwise back up to find the first number the cycle
  while(str_sub(perm,pos-1,pos-1) != "(")
    pos <- pos-1
  return (str_sub(perm,pos,pos))
}

cycle.convert <- function(fval){
  cycles <- character(0)
  for (nextstart in c("1","2","3","4","5","6","7","8")) {
    m <- as.numeric(nextstart)  #apply to integer index
    if (m > length(fval)) break
    #If the symbol is unchanged or is already in a cycle, there is nothing to do
    if ((fval[m] == nextstart) || (nextstart %in% cycles)) next
    #Otherwise keep tracing through the cycle until we find the end
    nextch <- fval[m]
    cycles <- c(cycles,"(",nextstart,nextch)
    nextch <- fval[as.numeric(nextch)]
    while (nextch != nextstart){
      cycles <- c(cycles,nextch)
      nextch <- fval[as.numeric(nextch)]
    }
    cycles <- c(cycles, ")")
  }
  #Collapse the vector of cycles into a string    
  return(paste(cycles, sep = "", collapse=""))
}
#cycle.convert(c("1","3","2","5","4"))
#Compute the product ab of two permutations of the symbols "1" through "9"
multiply <- function(a,b){
  if (a == "I") return(b)
  if (b == "I") return(a)
  #Make a vector of the function values
  fval <- character(0)
  for (i in c("1","2","3","4","5","6","7","8","9")) {
    fval <- c(fval, apply(apply(i,b),a) )
  }
  #If input and output are equal we have the identity
  if (sum(fval != "1":"9") == 0) return("I")
  #Otherwise generate cycle notation
  return(cycle.convert(fval))
}


powerString <- function(perm) {
  power <- perm
  result <- perm
  while (power != "I") {
    power <- multiply(power,perm)
    result <- paste(result,power,sep = "<br/>")
  }
  return(result)
}

inverse <- function(perm) {
  power <- perm
  while (power != "I") {
    power <- multiply(power,perm)
    if (multiply(power,perm) == "I")
    return(power)
  }
}



conjugate <- function(a,b) {
  multiply(a,multiply(b, inverse(a)))
}

