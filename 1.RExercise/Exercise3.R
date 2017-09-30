#1. 
#(a)
#xVec <- (1:n)
#seq(along=x)
tmpFn1 <- function(xVec)
{ 
  xVec^(1:length(xVec))
}
tmpFn2 <- function(xVec)
{ 
  n <- length(xVec) 
  (xVec^(1:n))/(1:n)
}
#(b)
tmpFn3 <- function(x, n)
{  
  1 + sum((x^(1:n))/(1:n)) 
}
#Always try out your functions on simple examples where you know the answer: 
#for example tmpFn1(1:3) should return the vector (1, 4, 27). 
#Also, check extreme cases: what happens if xVec has length 0? 
#Many functions require initial if statements which check that 
#the values of the function arguments satisfy the design requirements of the function—
#for example checking that the value of n is strictly positive in tmpFn3. 
#We have not included such code in our answers.

#2. 
tmpFn <- function(xVec) {
  n <- length(xVec)
  ( xVec[ -c(n-1,n) ] + xVec[ -c(1,n) ] + xVec[ -c(1,2) ] )/3
}
options(digits=0)
#or
tmpFn <- function(xVec)
{
  n <- length(xVec)
  ( x[1:(n-2)] + x[2:(n-1)] + x[3:n] )/3
}
options(digits=0)
#Note that tmpFn( c(1:5,6:1) ) should return the vector(2,3,4,5,5.333,5,4,3,2).

#3. 
tmpFn <- function(x) {
  ifelse(x < 0, x^2 + 2*x + 3, ifelse(x < 2, x+3, x^2 + 4*x - 7))
}
tmp <- seq(-3, 3, len=100)
plot(tmp, tmpFn(tmp), type="l")

#4. 
tmpFn <- function(mat) {
  mat[mat%%2 == 1] <- 2 * mat[mat%%2 == 1]
  mat 
}

#5. 
#For the specific case of n=5 and k=2:
tmp <- diag(2, nr = 5)
tmp[abs(row(tmp) - col(tmp)) == 1] <- 1 
tmp
#Now for the function for the general case:
tmpFn <- function(n, k)
{
  tmp <- diag(k, nr = n)
  tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
  tmp
}

#6. 
quadrant <- function(alpha) {
  1 + (alpha%%360)%/%90
}
#or
quadrant2 <- function(alpha)
{
  floor(alpha/90)%%4 + 1
}
#Both functions work on vectors, as any answer should!!

#7. 
weekday <- function(day, month, year) {
    month <- month - 2
    if(month <= 0) {
      month <- month + 12
      year <- year - 1
    }
    cc <- year %/% 100
    year <- year %% 100
    tmp <- floor(2.6*month - 0.2) + day + year + year %/% 4 + cc %/% 4 - 2 * cc
    c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7]
}
#The output of executing c( weekday(27,2,1997), weekday(18,2,1940), weekday(21,1,1963) )
#is the vector "Thursday", "Sunday", "Monday".
#Using if in the definition of weekday means that this function does not work on vectors. 
#However, we can eliminate the if statement as follows.
weekday2 <- function(day, month, year)
{
  flag <- month <= 2
  month <- month - 2 + 12*flag
  year <- year - flag
  cc <- year %/% 100
  year <- year %% 100
  tmp <- floor(2.6*month - 0.2) + day + year + year %/% 4 + cc %/% 4 - 2 * cc
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7]
}
#The output of executing weekday2( c(27,18,21), c(2,2,1), c(1997,1940,1963) ) 
#where all three input parameters are vectors is the vector "Thursday", "Sunday", "Monday".
#Clearly both weekday and weekday2 need extra lines of code which check that the values given for day,
#month and year are valid.

#8. 
#(a)
testLoop <- function(n)
{
  if(n<4)
    print("The argument n must be an integer which is at least 4.")
  else
  {
  xVec <- rep(NA, n-1)
  xVec[1] <- 1
  xVec[2] <- 2
  for( j in 3:(n-1) )
  xVec[j] <- xVec[j-1] + 2/xVec[j-1]
  xVec
  }
}
#Important. The colon operator has a higher precedence than the arithmetic operators 
#such as + or * but lower precedence than ^. 
#So always use brackets for constructs like 1:(n-1) or 1:(20^k) 
#so that the meaning is obvious even to those whose memory is faulty.
#Important. The above function gives the wrong answer if called with n=3. Why? 
#A line such as the following must be inserted:
#if( n <4 ) stop("The argument n must be an integer which is at least 4.\n")
#(b) 
#The following code is wrong. Why? 
testLoop2 <- function(yVec)
{
  n <- length(yVec)
  sum( exp(1:n) )
}
#The function testLoop2 returns the value of e0 + e1 if the vector yVec has length 0. 
#A correct function is 
testLoop2 <- function(yVec)
{
  n <- length(yVec)
  sum( exp(seq(along=yVec)) )
}
#This function now returns the correct value of 0 if yVec has length 0. 
#Important. Always use seq(along=x) rather than 1:length(x).

#9. 
#(a) 
#For a question like this where the value of xn must be known before the value of xn+1 can be calculated, 
#it is necessary to use a loop.
#First create the space for the answer with the code 
#xVec <- rep(NA, niter) 
#and then fill in the values. 
#Growing a vector inside a loop is very slow and inefficient. 
#Initialising the vector xVec to NA rather than to 0 makes it easier to spot certain errors: for example, 
#the error that the loop stops too early.
quadmap <- function(start, rho, niter)
{
  xVec <- rep(NA,niter)
  xVec[1] <- start
  for(i in 1:(niter-1)) {
    xVec[i + 1] <- rho * xVec[i] * (1 - xVec[i])
  }
  x 
}
tmp <- quadmap(start=0.95, rho=2.99, niter=500)
plot(tmp, type="l")
plot(tmp[300:500], type="l")
#(b)
quad2 <- function(start, rho, eps = 0.02)
{
  x1 <- start
  x2 <- rho*x1*(1 - x1)
  niter <- 1
  while(abs(x1 - x2) >= eps) {
    x1 <- x2
    x2 <- rho*x1*(1 - x1)
    niter <- niter + 1
  }
  niter
}

#10. 
#Values from the vector (x1 − x ̄, x2 − x ̄, . . . , xn − x ̄) are used three times in the expression for r
#k: twice in the numerator and once in the denominator.. 
#Therefore it is important to calculate this vector once and save its value for use in all three situations. 
#A function definition should not, for example, contain more than one occurrence of the expression mean(xVec). 
#Writing mean(xVec) more than once means that you are asking the programme to spend time calculating it more than once.
#(a)
tmpAcf0 <- function(xVec)
{
  xc <- xVec - mean(xVec)
  denom <- sum(xc^2)
  n <- length(x)
  r1 <- sum( xc[2:n] * xc[1:(n-1)] )/denom
  r2 <- sum( xc[3:n] * xc[1:(n-2)] )/denom
  list(r1 = r1, r2 = r2)
}
#(b)
tmpAcf <- function(x, k)
{
  xc <- x - mean(x)
  denom <- sum(xc^2)
  n <- length(x)
  tmpFn <- function(j){ sum( xc[(j+1):n] * xc[1:(n-j)] )/denom }
  c(1, sapply(1:k, tmpFn))
}





