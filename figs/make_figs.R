xvals <- seq(-10, 0, length.out=1001)

# wavefront
w <- function (x) {
    y <- pmax(0, 1 - exp(x / 2))
    return(y / sum(y))
}

# stationary distribution for lineages
f <- function (x) {
    y <- exp(x) * (1 - exp(x/2))
    return(y / sum(y))
}

png(file='pme_dists.png', width=64*5, height=64*7, res=64)
par(mar=c(5,3,1,1)+.1)
    plot(xvals, f(xvals), type='l',
         ylab='', xlab='position', lwd=2)
    lines(xvals, w(xvals), col='red', lwd=2)
    legend("topleft", lty=1, lwd=2, col=c("black", "red"),
           legend=c("lineage", "population"))
dev.off()
