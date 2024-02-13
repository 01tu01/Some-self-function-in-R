frequency_table <- function(x, k = NULL) {
  # x是数据；k是组数，不输入k则使用经验公式确定
  R <- max(x) - min(x);
  n <- length(x);
  if (is.null(k)) {
    k <- floor(1 + 3.3 * log10(n));
  }
  else {
    k <- k;
  }
  d <- R / k;
  sorted_x <- sort(x);
  num <- matrix(0, 1, k);
  mids <- matrix(0, 1, k);
  for (i in seq(1, k)) {
    for (j in seq(1, n)) {
      if ((sorted_x[j] > min(x) + (i - 1) * d) & (sorted_x[j] <= min(x) + i * d)) {
        num[i] = num[i] + 1;
      }
    }
  }
  num[1] = num[1] + 1;
  i = 1;
  j = 1;
  while (i <= n) {
    if (num[j] != 0) {
      mids[j] = median(sorted_x[i:(i + num[j] - 1)]);
    }
    else {
      mids[j] = 0;
    }
    i = i + num[j];
    j = j + 1;
  }
  A <- table(cut(sorted_x, br = k, right = TRUE));
  A <- t(t(A));
  B <- cbind(A, t(mids), t(num / n), t(num / n / d));
  result <- cbind(rownames(B), B);
  row.names(result) <- seq(1, k);
  colnames(result) <- c("interval", "frequency_number", "median", "frequency_rate", "frequency_rate / interval_distance");
  write.table(result, file = "frequency_table_out.csv", sep = ",", row.names = F)
  return(result);
}