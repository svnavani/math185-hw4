# Homework 4
# Samir Navani, A13351282

standardize_x <- function(x) {
  mX = matrix(colMeans(x), nr=dim(x)[1], nc=dim(x)[2], byrow = TRUE)
  x0 = x-mX
  s2 = sqrt(colMeans(x0^2))
  x0 = x0 % * % diag(1/s2)
  output = list(x0 = x0, diag = s2)
  return(output)
}

load_gcd <- function(x, y, eta) {
  n=dim(x)[1]
  d=dim(x)[2]
  standX = standardize_x(x)
  x0=cbind(rep(1,n), standX$x0)
  b0 = numeric((d+1))
  res0 = y
  b1 = rep(1, d+1)
  iter = 0
  repeat {
    if(max(abs(b1-b0)) < 10^(-5) | iter > 20000)
      break
    b1=b0
    subgrad = sign(res0)
    grad = -t(x0)%*%subgrad/n
    b0 = b0 - eta *grad
    res0 = y-x0%*%b0
    iter = iter + 1
    b0[-1] = b0[-1] / standX$diag
    b0[1] - b0[1] - sum(colMeans(X) * b0[-1])
    output = list(beta=b0, res = res0, iter = iter)
    return(output)
  }
}

data = read.csv("education.csv", sep = ',', header = TRUE)
# put the columns x1, x2, and x3 in the functions written above
#