llr = function(x, y, z, omega){
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}
compute_f_hat = function(z, x, y, omega) {
  Wz = diag(make_weight_matrix(z, x, omega))
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% sweep(X, 1, Wz, "*")) %*% t(X) %*% matrix(Wz * y)
  return(f_hat)
}
make_weight_matrix = function(z, x, omega) {
  r = abs(x - z) / omega
  w = sapply(r, W)
  Wz = diag(w)
  return(Wz)
}
W = function(r) {
  if (abs(r) < 1) {
    return((1 - abs(r) ** 3) ** 3)
  } else{
    return(0)
  }
}
make_predictor_matrix = function(x) {
  n = length(x)
  return(cbind(rep(1, n), x))
}

# --- example 1 --- #
# get the data
data(french_fries, package = 'reshape2')
french_fries = na.omit(french_fries)

# input data
x = french_fries$potato
y = french_fries$buttery

# space along which to smooth
z = seq(0, 15, length.out = 100)

# run smoothing
fits = llr(z = z, x = x, y = y, omega = 2)

# plot the data and the smoother
plot(x, y)
lines(z, fits, col = 'red')


