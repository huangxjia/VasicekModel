#refer to http://www.r-bloggers.com/fun-with-the-vasicek-interest-rate-model/
#Vasicek Model

## Simulate Sample Paths ##
## define model parameters
r0 <- 0.03
theta <- 0.10
k <- 0.3
beta <- 0.001


## simulate short rate paths
n <- 10    # MC simulation trials
T <- 100    # total time
m <- 2000   # subintervals
dt <- T/m  # difference in time each subinterval

r <- matrix(0,m+1,n)  # matrix to hold short rate paths
r[1,] <- r0

for(j in 1:n){
  for(i in 2:(m+1)){
# the key step
    dr <- k*(theta-r[i-1,j])*dt + beta*sqrt(dt)*rnorm(1,0,1)
    r[i,j] <- r[i-1,j] + dr
  }
} 

t <- seq(0, T, dt)
rT.expected <- theta + (r0-theta)*exp(-k*t)
rT.stdev <- sqrt( beta^2/(2*k)*(1-exp(-2*k*t)))
#matplot, abline, lines, points
# can see that expected rt goes to theta, rt goes to theta
# the volatility of the path decided by beta*sqrt(dt)
matplot(t, r[,1:n], type="l", lty=1, main="Short Rate Paths", ylab="rt") 
abline(h=theta, col="red", lty=2)
lines(t, rT.expected, lty=2) 
lines(t, rT.expected + 2*rT.stdev, lty=2) 
lines(t, rT.expected - 2*rT.stdev, lty=2) 
points(0,r0)


VasicekZCBprice <- 
  function(r0, k, theta, beta, T){
    b.vas <- (1/k)*(1-exp(-T*k)) 
    a.vas <- (theta-beta^2/(2*k^2))*(T-b.vas)+(beta^2)/(4*k)*b.vas^2
    return(exp(-a.vas-b.vas*r0))
  }

## define model parameters for plotting yield curves
theta <- 0.10
k <- 0.5
beta <- 0.03

r0 <- seq(0.00, 0.20, 0.05)
n <- length(r0)
#matrix here
yield <- matrix(0, 100, n)
for(i in 1:n){
  for(T in 1:100){
    yield[T,i] <- -log(VasicekZCBprice(r0[i], k, theta, beta, T))/T
  }
}

maturity <- seq(1, 100, 1)
#col = "black"
can see that yield also tend to be theta
matplot(maturity, yield, type="l", lty=1, main="Yield Curve Shapes")
abline(h=theta, col="red", lty=2)


r0 <- 0.03
theta <- 0.10
k <- 0.3
beta <- 0.03

## simulate short rate paths
n <- 1000  # MC simulation trials
T <- 1000     # total time
m <- 200   # subintervals
dt <- T/m  # difference in time each subinterval

r <- matrix(0,m+1,n)  # matrix to hold short rate paths
r[1,] <- r0
for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- k*(theta-r[i-1,j])*dt + beta*sqrt(dt)*rnorm(1,0,1)
    r[i,j] <- r[i-1,j] + dr
  }
} 
# if u make dt very big, the volatility would be very big, too

maturity <- seq(0,T,dt)
matplot(maturity, r[,1], type = 'l', )
## calculate Monte Carlo bond price and compare to Exact Vasicek solution
#colSums here,
ss <- colSums(r[2:(m+1),]*dt)  # integral estimate
c <- exp(-ss)
estimate <- mean(c)
stdErr <- sd(c)/sqrt(n)
exact <- VasicekZCBprice(r0, k, theta, beta, T)

cat('Exact Vasicek Price:', round(exact,4), 'n')
cat('MC Price:', round(estimate,4), 'n')
cat('MC Standard Error:', round(stdErr,4), 'n')
