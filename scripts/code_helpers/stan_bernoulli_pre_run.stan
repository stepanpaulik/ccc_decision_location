data {
  int<lower=1> J; // Legal decisions
  int<lower=1> K; // Number of legal sources
  int<lower=1> N; // N = J x K
  int<lower=1,upper=J> jj[N]; // Legal decision for legal source n
  int<lower=1,upper=K> kk[N]; // Legal source for legal decision n
  int<lower=0,upper=1> y[N]; // binary outcome: 1 = citation occurred, 0 = no citation
}
parameters {
  vector[K] alpha; // The systematic parameters
  vector[K] phi1; // phi1 is the systematic component phi
  vector[J] theta1;
  vector[J] beta;
  real<lower=0.1> sigma_alpha;
  real<lower=0.1> sigma_beta;
  real<lower=0.1> sigma_phi1;
  real<lower=0.1> gamma;
  real<lower=0.1> sigma_gamma;
  real mu_beta;
  real mu_phi1;
}
transformed parameters {
  vector[N] eta; // the linear predictor
  for (n in 1:N) {
    eta[n] = alpha[kk[n]] + (beta[jj[n]] + mu_beta) -
             gamma * square(theta1[jj[n]] - (phi1[kk[n]] + mu_phi1));
  }
}
model {
  // Priors
  sigma_alpha ~ cauchy(0, 5);
  sigma_gamma ~ lognormal(0, 1);
  sigma_beta ~ cauchy(0, 5);
  sigma_phi1 ~ cauchy(0, 5);
  mu_beta ~ cauchy(0, 5);
  mu_phi1 ~ cauchy(0, 5);
  
  // Model Parameters
  gamma ~ lognormal(0, sigma_gamma);
  alpha ~ normal(0, sigma_alpha);
  beta ~ normal(0, sigma_beta);
  phi1 ~ normal(0, sigma_phi1);
  theta1 ~ normal(0, 1);
  
  // Likelihood
  y ~ bernoulli(Phi(eta));
}
