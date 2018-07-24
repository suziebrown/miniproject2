pvals <- seq(0.05,0.95,0.05)
reps <- 100
nflips <- numeric(reps)
means <- numeric(length(pvals))
k <- 6 ## k=8 takes way too long
seqk <- 1:k

for (p in 1:length(pvals)) {
  for (i in 1:reps) {
    stp <- FALSE
    flips <- 0
    while (stp == FALSE) {
      x <- rbinom(k,1,pvals[p])
      stopat <- seqk[cumsum(x)>=2 & cumsum(!x)>=2]
      if (length(stopat) == 0){
        flips <- flips + stopat[1]
      } else {
        flips <- flips + k
        if (sum(x) == 1 || sum(x) == k-1) {
          stp <- TRUE
        }
      }
    }
    nflips[i] <- flips
  }
  means[p] <- mean(nflips)
}

p <- seq(from = 0.01, to = 0.99, by = 0.01)
f <- function(p,k) {k / (1 - (1 - k * p ^ (k - 1) * (1 - p)) * (1 - k * p * (1 - p) ^ (k - 1)))}
plot(p, f(p,k), type = 'l', ylim=c(1,150), lwd = 2, ylab = "E(tau)", main = "Expected running time of 1/8 Bernoulli factory")
points(pvals, means)
legend("top", c("no early stopping", "early stopping (MC)"), pch=c(NA,1), lty=c(1, NA), lwd=2)

#~~~
# Expected time to see two heads and two tails:
p <- seq(from = 0.01, to = 0.99, by = 0.01)
E0011 <- function(p) {2*(1-p+2*p^3-p^4)/(p*(1-p))}
plot(p, E0011(p), type='l', lwd=2, ylim=c(0,20))

