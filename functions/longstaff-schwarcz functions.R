# 
A <- function(tau, alpha, delta) {
  
  phi <- (2*alpha + delta^2)^0.5
  
  A <- 2 * phi/((delta + phi)*expm1(phi * tau) + 2*phi)
  
  return(A)
}


B <- function(tau, beta, nu) {
  
  psi <- (2*beta + nu^2)^0.5
  
  B <- 2 * psi/((nu    + psi)*expm1(psi * tau) + 2*psi)
  
  return(B)
}


C <- function(tau, alpha, beta, delta, nu) {
  
  phi <- (2*alpha + delta^2)^0.5
  psi <- (2*beta + nu^2)^0.5
  
  C <- (alpha * phi * expm1(psi * tau) * B(tau, beta, nu) - 
          beta * psi * expm1(phi * tau) * A(tau, alpha, delta)) /
    (phi * psi * (beta - alpha))
  
  return(C)
}


D <- function(tau, alpha, beta, delta, nu) {
  
  phi <- (2*alpha + delta^2)^0.5
  psi <- (2*beta + nu^2)^0.5
  
  D <- (psi * expm1(phi * tau) * A(tau, alpha, delta) -
          phi * expm1(psi * tau) * B(tau, beta, nu)) / 
    (phi * psi * (beta - alpha))
  
  return(D)
}

kappa <- function(alpha, beta, gamma, delta, eta, nu) {
  
  phi <- (2*alpha + delta^2)^0.5
  psi <- (2*beta + nu^2)^0.5
  
  kappa <- gamma*(delta + phi) + eta*(nu + psi)
  
  return(kappa)
  
}

discount <- function(tau, r, V, alpha, beta, gamma, delta, eta, nu) {
  
  discount <- (A(tau, alpha, delta)^(2*gamma) * B(tau, beta, nu)^(2*eta) *
                 exp(kappa(alpha, beta, gamma, delta, eta, nu)*tau + 
                       C(tau, alpha, beta, delta, nu)*r + 
                                    D(tau, alpha, beta, delta, nu)*V))
  
  return(discount)
}


yield <- function(tau, r, V, alpha, beta, gamma, delta, eta, nu) {
  
  phi <- (2*alpha + delta^2)^0.5
  psi <- (2*beta + nu^2)^0.5
  kappa <- gamma*(delta + phi) + eta*(nu + psi)
  
  yield <- -(kappa*tau + 2*gamma*log(A(tau, alpha, delta)) + 
               2*eta*log(B(tau, beta, nu)) +
                  C(tau, alpha, beta, delta, nu)*r + 
                    D(tau, alpha, beta, delta, nu)*V) / tau
  
  return(yield)
}