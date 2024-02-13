self_outline <- function(x) {
  if (is.data.frame(x) == TRUE)
    x <- as.matrix(x)
  m <- nrow(x);
  n <- ncol(x);
  plot(c(1, n), c(min(x), max(x)), type = "n",
  main = "Self-outline", xaxt = "n",
  xlab = "Number", ylab = "Value")
  xmark <- c(NA, colnames(x), NA)
  axis(1, 0:(n + 1), labels = xmark)
  for (i in 1:m) {
    lines(x[i,], col = i, lty = i)
  }
  legend("topright", legend = rownames(x), bty = "n", pch = 20, cex = 1, col = seq(1:n))
}