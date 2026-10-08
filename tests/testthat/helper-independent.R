# independent implementations for testing, written from the papers and not
# from the package code (adversarial review, #101)----

# central-difference Jacobian of the mean estimating function----
num_jacobian <- function(f, theta) {
  vapply(seq_along(theta), function(j) {
    h <- 1e-6 * max(1, abs(theta[j]))
    up <- theta
    dn <- theta
    up[j] <- up[j] + h
    dn[j] <- dn[j] - h
    (f(up) - f(dn)) / (2 * h)
  }, numeric(length(f(theta))))
}

# independent stacked M-estimator for EC-IPW / EC-AIPW (Theorems 3 and 4)----
# written from the paper, not from R/ec_ipw.R or R/ec_aipw.R
independent_ec <- function(d, outcomes, covs, w, augment = FALSE) {
  Y <- as.matrix(d[, outcomes])
  S <- d$S
  A <- d$A
  N <- nrow(d)
  n <- sum(S)
  X <- cbind(1, as.matrix(d[, covs]))
  n_t <- length(outcomes)
  p <- ncol(X)
  pi_S <- n / N
  pi_A <- sum(A[S == 1]) / n
  q <- if (augment) p * n_t else 0
  psi <- function(theta) {
    mu11 <- theta[seq_len(n_t)]
    mu10 <- theta[n_t + seq_len(n_t)]
    mu00 <- theta[2 * n_t + seq_len(n_t)]
    alpha <- theta[3 * n_t + seq_len(p)]
    Yt <- Y
    psi5 <- NULL
    if (augment) {
      beta <- matrix(theta[3 * n_t + p + seq_len(q)], nrow = p)
      Yt <- Y - X %*% beta
      psi5 <- do.call(cbind, lapply(seq_len(n_t), function(t) {
        (1 - A) * Yt[, t] * X
      }))
    }
    eta <- drop(X %*% alpha)
    w00 <- exp(eta) * (1 - pi_S) / pi_S
    cbind(
      S * A * sweep(Yt, 2, mu11) / (pi_S * pi_A),
      S * (1 - A) * sweep(Yt, 2, mu10) / (pi_S * (1 - pi_A)),
      (1 - S) * w00 * sweep(Yt, 2, mu00) / (1 - pi_S),
      (S - plogis(eta)) * X,
      psi5
    )
  }
  alpha <- unname(coef(glm(S ~ X - 1, family = binomial)))
  beta <- NULL
  Yt <- Y
  if (augment) {
    ctrl <- A == 0
    beta <- vapply(seq_len(n_t), function(t) {
      unname(coef(lm(Y[ctrl, t] ~ X[ctrl, ] - 1)))
    }, numeric(p))
    Yt <- Y - X %*% beta
  }
  w00 <- exp(drop(X %*% alpha))[S == 0]
  theta <- c(
    colMeans(Yt[S == 1 & A == 1, , drop = FALSE]),
    colMeans(Yt[S == 1 & A == 0, , drop = FALSE]),
    colSums(w00 * Yt[S == 0, , drop = FALSE]) / sum(w00),
    alpha,
    as.vector(beta)
  )
  bread <- num_jacobian(function(th) colMeans(psi(th)), theta)
  meat <- crossprod(psi(theta)) / N
  bread_inv <- solve(bread)
  sigma <- bread_inv %*% meat %*% t(bread_inv)
  cmat <- cbind(
    diag(n_t), -(1 - w) * diag(n_t), -w * diag(n_t),
    matrix(0, n_t, p + q)
  )
  list(
    tau = drop(cmat %*% theta),
    sd = sqrt(diag(cmat %*% sigma %*% t(cmat)) / N),
    max_ee = max(abs(colMeans(psi(theta))))
  )
}

# the paper's Eq 11 optimal weight, from an independently fitted PS model----
independent_opt_weight <- function(d, covs) {
  X <- cbind(1, as.matrix(d[, covs]))
  ps <- fitted(glm(d$S ~ X - 1, family = binomial))
  w00 <- (ps / (1 - ps))[d$S == 0]
  n10 <- sum(d$S == 1 & d$A == 0)
  v10 <- 1 / n10
  v00 <- sum(w00^2) / sum(w00)^2
  v10 / (v10 + v00)
}
