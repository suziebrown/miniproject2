# Make plots of expected running time for example Bernoulli factories

## Von Neumann f(p)=1/2 -------------
p <- seq(from = 0.05, to = 0.95, by = 0.01)
plot(p, 1/(p*(1-p)), type='l', ylim=c(1,22), lwd=2, ylab='E(tau)', main="Expected running time of 1/2 Bernoulli factory")

## f(p)=p^k -------------------------
## single plot
p <- seq(from = 0, to = 1, by = 0.01)
k <- 3
fp <- -p^k + (1-p^(k+1))/(1-p)
if (k == 1) {fp <- rep(1, length(p))}
plot(p, fp, type='l', lwd=2, col='blue', ylim=c(0,k), ylab='E(tau)', main=paste("Expected running time of p^",k," Bernoulli factory", sep=""))

## overlay several plots
kvals <- 1:5
plot(NA, xlim = c(0, 1), ylim = c(1, max(kvals)), ylab = "E(tau)", xlab = "p", main = "Expected running time of p^k Bernoulli factory")
for (k in kvals) {
  fp <- -p^k + (1-p^(k+1))/(1-p)
  if (k == 1) {fp <- rep(1, length(p))}
  lines(p, fp , lty = k, lwd = 2)
}
legend("topleft", paste("k=",kvals), lwd = 2, lty = kvals)


## f(p)=1/3 -------------------------
p <- seq(from = 0.01, to = 0.99, by = 0.01)
f <- function(p,k) {k / (1 - (1 - k * p ^ (k - 1) * (1 - p)) * (1 - k * p * (1 - p) ^ (k - 1)))}
plot(p, f(p,3), type = 'l', ylim=c(1,60), lwd = 2, ylab = "E(tau)", main = "Expected running time of 1/3 Bernoulli factory")
lines(p, 1/(p^2*(1-p)), lwd=2, lty = 2)
lines(p, 1/(p*(1-p)^2), lwd=2, lty = 3)
legend("top", c("001 / 010 / 100", "110 / 101 / 011", "either"), lty=3:1, lwd=2)


## f(p)=1/8 directly versus logic circuit with 1/2-coins -----
p <- seq(from = 0.001, to = 0.999, by = 0.001)
plot(p, f(p,8), type = 'l', ylim=c(1,150), lwd = 2, ylab = "E(tau)", main = "Expected running time of 1/8 Bernoulli factory")
lines(p, 3/(p*(1-p)), lwd=2, lty=2)
legend("top", c("one 1/8-coin", "three 1/2-coins"), lty=1:2, lwd=2)

