data {
  int<lower=1> J; // Legal decisions
  int<lower=1> K; // Number of legal sources
  int<lower=1> N; // N = J x K
  int<lower=1,upper=J> jj[N]; // Legal decision for legal source n
  int<lower=1,upper=K> kk[N]; // Legal source for legal decision n
  int<lower=0,upper=1> y[N]; // binary outcome: 1 = citation occurred, 0 = no citation
  int<lower=1> left_anchor; // the left anchor position for theta
  int<lower=1> right_anchor; // the right anchor position for theta
}
parameters {
  vector[K] alpha; // The systematic parameters
  vector[K] phi1; // phi1 is the systematic component phi
  vector[J] theta1;
  vector[J] beta;
  real<lower=0.1> sigma_alpha;
  real<lower=0.1> sigma_beta;
  real<lower=0.1> sigma_phi1;
  real<lower=0.1> gamma; // 0.1 is crucial to avoid model collapse
  real<lower=0.1> sigma_gamma;
  real mu_beta;
  real mu_phi1;
}
transformed parameters {
  vector[N] eta; // the linear predictor
  vector[J] theta; // the rotation-corrected theta estimate
  vector[K] phi; // the rotation-corrected phi estimate

  for (n in 1:N) {
    eta[n] = alpha[kk[n]] + (beta[jj[n]] + mu_beta) -
             gamma * square(theta1[jj[n]] - (phi1[kk[n]] + mu_phi1));
  }

  // Addressing rotational invariance
  if (theta1[left_anchor] < theta1[right_anchor]) {
    theta = theta1;
  } else {
    theta = theta1 * -1;
  }

  if (theta1[left_anchor] < theta1[right_anchor]) {
    phi = phi1;
  } else {
    phi = phi1 * -1;
  }
}
model {
  // Priors
  gamma ~ lognormal(0, sigma_gamma);
  sigma_gamma ~ lognormal(0, 1);
  sigma_alpha ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);
  sigma_phi1 ~ cauchy(0, 5);
  mu_beta ~ cauchy(0, 5);
  mu_phi1 ~ cauchy(0, 5);

  // Model Parameters
  alpha ~ normal(0, sigma_alpha);
  beta ~ normal(0, sigma_beta); 
  phi1 ~ normal(0, sigma_phi1); 
  theta1 ~ normal(0, 1); 

  // Likelihood with probit link
  for (n in 1:N) {
    real prob = Phi(eta[n]);
    prob = fmin(0.9999, fmax(0.0001, prob)); // Clamp prob to avoid numerical issues
    y[n] ~ bernoulli(prob); 
  }
}
