## investigating running time v. p for QBF of 2p

p <- seq(0.01, 0.99, 0.01)
f <- function(p) {2/(1-4*p*(1-p))}
plot(p, f(p), type='l', lwd=2, ylim=c(0,200))

#---

qk <- function(k) {choose(2*k,k)/((2*k-1)*2^(2*k))}
k <- 1:100
plot(k, qk(k), log='y', ylab='q_k', pch=16)

#---
# running time of QBF for wedge fn

qk <- function(k) {choose(2*k,k)/((2*k-1)*2^(2*k))}
tauk_fn <- function(k,p) { 2 * choose(2*k,k) * (1-p^k)/((2*k-1)*2^(2*k) * (1-p)) } #possible to sum analytically over k?

p <- seq(0.001, 0.999, 0.002)
k <- 1:500
p2 <- 4*p*(1-p)

Et <- numeric(length(p))
for (j in 1:length(p2)) {
  Etauk <- numeric(length(k))
  for (i in 1:length(k)) {
    Etauk[i] <- tauk_fn(k[i], p2[j])
  }
  Et[j] <- sum(Etauk)
}

plot(p, Et, type='l', lwd=2, ylim=c(0,20), ylab='E(tau)')
#plot(p, rep(sum(2*k*qk(k)), length(p)))
#plot(k, cumsum(2*k*qk(k)))



#---
# discrepancy caused by truncating k

qk <- function(k) {choose(2*k,k)/((2*k-1)*2^(2*k))}
p <- 0.49
p2 <- function(p) {4*p*(1-p)}
k <- 1:500
kterms <- qk(k)
diff <- 1 - cumsum(kterms)
plot(k, diff, xlab='k_max', ylab='error', log='y')

# discrepancy as a function of mean quoin consumption
qk <- function(k) {choose(2*k,k)/((2*k-1)*2^(2*k))}
k <- 1:500

diff <- 1- cumsum(qk(k))
change_error <- qk(k)
quoins <- (2/(1-p2(p)))* cumsum(qk(k)*(1-p2(p)^k))
#plot(k, diff)
#plot(k, quoins)
ratio <- quoins/diff
plot(k,ratio)
lines(1.952*k, lwd=2, col='red')
#lines(0.05*k^(-0.5), lwd=2, col='purple') ## nope, that deffo doesn't fit


### target function wehn k is restricted
kmax <- 500
k <- 1:kmax
p <- seq(0.01, 0.99, 0.01)
target <- numeric(length(p))
for (i in 1:length(p)) {
  target[i] <- sum(qk(k)*p2(p[i])^k)
}
#plot(p, target, type='l', lwd=2, ylim=c(0,1), ylab = 'approximate target')
#lines(p, target, lwd=2)
lines(c(0.5,1), c(1,0), lwd=2, col='grey')
