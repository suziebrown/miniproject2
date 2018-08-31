# qk <- function(k) {choose(2*k,k)/((2*k-1)*2^(2*k))}
# tauk_fn <- function(k,p2) {2 * qk(k) * (1-p2^k) / (1-p2)}
#
# p <- seq(0.01, 0.99, 0.02)
# p2 <- 4*p*(1-p)
#
# kmax <- c(1,5,20,100,500)
#
# for (ii in 1:length(kmax)){
#   k <- 1:kmax[ii]
#
#   quoins <- numeric(length(p))
#   for (j in 1:length(p2)) {
#     Etauk <- numeric(length(k))
#     for (i in 1:length(k)) {
#       Etauk[i] <- tauk_fn(k[i], p2[j])
#     }
#     quoins[j] <- sum(Etauk)
#   }
#
#   epsilon <- 1- sum(qk(1:k))
#
# }
#
#


p2 <- function(p) {4*p*(1-p)}
qk <- function(k) {choose(2*k,k)/((2*k-1)*2^(2*k))}

p <- 0.499 # see what happens when you vary this
k <- 1:500

epsilon <- 1- cumsum(qk(k))
quoins <- (2/(1-p2(p))) * cumsum(qk(k) * (1-p2(p)^k))

plot(epsilon)
plot(quoins)
plot(epsilon, quoins, pch=16)

eps <- seq(0, 1, 0.01)
lines(eps, 0.6223*eps^(-1), lwd=2, col=2)

## no way does this relationship fit:
#lines(eps, 5.573*eps^(-0.5) - 11.06, col='purple')



