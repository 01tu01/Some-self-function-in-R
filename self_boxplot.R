self_boxplot <- function(x) {
  q.quantile <- quantile(x);
  Q1 <- q.quantile[2];
  Q3 <- q.quantile[4];
  R1 <- Q3 - Q1;
  Qu <- Q3 + 1.5 * R1;
  Qd <- Q1 - 1.5 * R1;
  plot(1, q.quantile[3], xlim = c(0.6, 1.4), ylim = c(floor(Qd) - 1, ceiling(max(w)) + 1), cex = 0.01, ylab = "");
  lines(c(0.8, 1.2), c(q.quantile[3], q.quantile[3]), lwd = 2);
  text(1.3, q.quantile[3], "median");
  text(0.7, q.quantile[3], q.quantile[3]);
  lines(c(0.8, 1.2), c(q.quantile[2], q.quantile[2]));
  text(1.3, q.quantile[2], "lower-quantile");
  text(0.7, q.quantile[2], q.quantile[2]);
  lines(c(0.8, 1.2), c(q.quantile[4], q.quantile[4]));
  text(1.3, q.quantile[4], "upper-quantile");
  text(0.7, q.quantile[4], q.quantile[4]);
  lines(c(0.8, 0.8), c(q.quantile[2], q.quantile[4]));
  lines(c(1.2, 1.2), c(q.quantile[2], q.quantile[4]));
  lines(c(0.9, 1.1), c(Qu, Qu));
  text(1.2, Qu, "upper-truncation");
  text(0.7, Qu, Qu);
  lines(c(0.9, 1.1), c(Qd, Qd));
  text(1.2, Qd, "lower-truncation");
  text(0.7, Qd, Qd);
  lines(c(1.0, 1.0), c(Qu, q.quantile[2]), lty = 2);
  lines(c(1.0, 1.0), c(Qd, q.quantile[4]), lty = 2);
  title("boxplot");
  n <- length(x);
  for (i in 1:n) {
    if (x[i] < Qd | x[i] > Qu) {
      points(1.0, x[i])
    }
  }
}