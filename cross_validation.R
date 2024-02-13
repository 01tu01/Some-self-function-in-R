cross_validation <- function(data, C) {
  # data是样本数据，C是样本所属的类别，使用Fisher线性判别
  # 判断是否只有2类
  if (length(unique(C)) != 2) {
    print("Make sure there are only two categories in the data.");
    return(NULL)
  }
  # 读取数据并进行基础的命名
  m <- ncol(data);
  cols <- sprintf("X%d", seq(1:m));
  x <- cbind(C, data);
  colnames(x) <- c("C", cols);
  n <- nrow(x);
  # 进行判别
  n_wrong = 0;
  for (i in seq(1:n)) {
    x_new <- rbind(x[1:i - 1,], x[i + 1:n,]);
    discrim <- lda(C ~ ., x_new);
    p.discrim <- predict(discrim, x[i,]);
    if (p.discrim$class != x[i, 1]) {
      n_wrong = n_wrong + 1;
    }
  }
  p <- n_wrong / n;
  return(p);
}