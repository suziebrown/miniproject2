# Make plots of expected running time for example Bernoulli factories

## Von Neumann f(p)=1/2
p <- seq(from = 0.05, to = 0.95, by = 0.01)
plot(p, 1/(p*(1-p)), type='l', ylim=c(1,22), lwd=2, ylab='E(tau)', main="Expected running time of Von Neumann's Bernoulli factory")

## f(p)=p^k -------------
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
