self_harmonic <- function(x) {
  if (is.data.frame(x) == TRUE) {
    t <- seq(-pi, pi, pi / 100)
  }
  m <- nrow(x)
  n <- ncol(x)
  f <- array(0, c(m, length(t)))
  for (i in 1:m) {
    f[i,] <- x[i, 1] / sqrt(2)
    for (j in 2:n) {
      if (j %% 2 == 0) {
        f[i,] <- f[i,] + x[i, j] * sin(j / 2 * t)
      } else {
        f[i,] <- f[i,] + x[i, j] * cos(j %/% 2 * t)
      }
    }
  }
  plot(c(-pi, pi), c(min(f), max(f)),
        type = "n", main = "Self-Harmonic-Curve",
        xlab = "t", ylab = "f(t)"
    )

  for (i in 1:m) {
    lines(t, f[i,], col = i, lty = i)
  }
  legend("topright", legend = rownames(x), bty = "n", pch = 20, cex = 1, col = seq(1:n))
}