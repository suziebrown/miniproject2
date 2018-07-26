f <- function(p,k) {k / (1 - (1 - k * p ^ (k - 1) * (1 - p)) * (1 - k * p * (1 - p) ^ (k - 1)))}
r0011 <- function(p) {(1-p+3*p^2-4*p^3+2*p^4)/(p*(1-p)) + (1-p)^2*(1+2*p)*rgeom(1,p) + p^2*(3-2*p)*rgeom(1,1-p)}

k <- 8
pvals <- seq(from = 0.01, to = 0.99, by = 0.01)

reps <- 10^5
taus <- numeric(reps)
etau <- numeric(length(pvals))

for (j in 1:length(pvals)) {
  p <- pvals[j]
  geom_min_prob <- 1- (1- k*p*(1-p)^(k-1))*(1- k*p^(k-1)*(1-p))
  for (i in 1:reps) {
    tau <- 0
    usable <- FALSE
    while (!usable) {
      T0011 <- r0011(p)
      usable <- rbinom(1,1,geom_min_prob)
      tau <- tau + min(T0011, k)
    }
    taus[i] <- tau
  }
  etau[j] <- mean(taus)
}

plot(pvals, f(pvals,k), type = 'l', ylim=c(1,140), lwd = 2, ylab = "E(tau)", main = paste("Expected running time of 1/",k," Bernoulli factory", sep=""))
points(pvals, etau)
legend("topright", c("no early stopping", "early stopping (MC)"), lty=c(1,NA), lwd=c(2,1), pch=c(NA, 1))

