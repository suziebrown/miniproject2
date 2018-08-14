p<-seq(0,0.5,0.01)
plot(p,2*p,type='l', lwd=2, ylab = "f(p)")
lines(p, fp, lty=2, lwd=2, col=2)
legend("topleft", c("exact","truncated"), lty=c(1,2), lwd=2, col=c(1,2))

fp <- 2*p
fp[fp>0.9] <- 0.9

#
p<-seq(0,0.5,0.01)
plot(p,2*p,type='l', lwd=2, col=4, ylab = "f(p)", xlim=c(0,1))
lines(p+0.5, 1-2*p, lwd=2, col=4)

#


#
p<-seq(0,1,0.01)
plot(p, 4*p*(1-p), type='l', lwd=2, ylab="")
#
p<-seq(0,0.5,0.01)
plot(p, 1-(1-4*p*(1-p))^(1/2), type='l', lwd=2, xlim=c(0,1), ylab="")
lines(p+0.5, 1-(1-4*(p+0.5)*(1-(p+0.5)))^(1/2), lwd=2)

