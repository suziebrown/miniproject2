## Make plots of expected running time for example Bernoulli factories

## Von Neumann f(p)=1/2
p <- seq(from = 0.1, to = 0.9, by = 0.01)
plot(p, 1/(p*(1-p)), type='l', lwd=2, ylim=c(1,11), col='blue', ylab='E(tau)', main="Expected running time of Von Neumann's Bernoulli factory")

## f(p)=p^k
p <- seq(from = 0, to = 1, by = 0.01)
k <- 2
fp <- -p^k + (k+1)*p^(2*k) + (1-p^k)*(1-p^(k-1))/(1-p)
plot(p, fp, type='l', lwd=2, col='blue', ylim=c(0,k), ylab='E(tau)', main=paste("Expected running time of p^",k," Bernoulli factory", sep=""))
