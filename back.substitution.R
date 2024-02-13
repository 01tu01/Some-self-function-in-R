back.substitution <- function
(X.origin, X.predict) {
  if (length(unique(X.origin)) != 2) {
    print("Make sure there are only two categories in the data.");
    return(NULL)
  }
  if (length(X.origin) != length(X.predict)) {
    print("Make sure the two sets of data are consistent in length.");
    return(NULL)
  }
  flag <- 0;
  for (i in seq(1, length(X.origin))) {
    if (X.origin[i] != X.predict[i]) {
      flag = flag + 1;
    }
  }
  p <- flag / length(X.origin);
  return(p)
}