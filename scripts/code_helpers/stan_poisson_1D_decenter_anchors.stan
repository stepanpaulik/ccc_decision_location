data {
int<lower=1> J; // Legal decisions
int<lower=1> K; //
int<lower=1> N; // N = J x K
int<lower=1,upper=J> jj[N]; // Legal decision for legal source n
int<lower=1,upper=K> kk[N]; // Legal source for legal decision n
int<lower=0> y[N]; // counts in citations
int<lower=1> left_anchor; // the left anchor position for theta
int<lower=1> right_anchor; // the right anchor position for theta
}
parameters {
  vector[K] alpha; //The systematic parameters
  vector[K] phi1; //phi1 is the systematic component phi
  vector[J] theta1;
  vector[J] beta;
  // real lambda_alpha;
  real<lower=0.1> sigma_alpha;
  real<lower=0.1> sigma_beta;
  real<lower=0.1> sigma_phi1;
  real<lower=0.1> gamma; // 0.1 is crucual. you want to keep it away from 0 or the model collapses
  real<lower=0.1> sigma_gamma;
  real mu_beta;
  real mu_phi1;
}
transformed parameters {
  // you can only assign to a parameter that is declared here
  vector[N] lambda; // the linear predictor
  vector[J] theta; // the rotation corrected theta estimate
  vector[K] phi; // the rotation corrected phi estimate
  // decentered specification to avoid divergent transitions
  for (n in 1:N){
    lambda[n] = exp(alpha[kk[n]] + (beta[jj[n]] + mu_beta)  -
    gamma * (square( theta1[jj[n]] - (phi1[kk[n]] + mu_phi1) )));
  };
  // Addressing rotational invariance
  if (theta1[left_anchor] < theta1[right_anchor]) {theta = theta1;}
    else {theta = theta1 * -1;}
  if (theta1[left_anchor] < theta1[right_anchor]) {phi = phi1;}
    else {phi = phi1 * -1;}
}
model {
    gamma ~ lognormal(0, sigma_gamma);
    sigma_gamma ~ lognormal(0, 1);
    sigma_alpha ~ cauchy(0, 5);
    sigma_beta ~ cauchy(0, 5);
    sigma_phi1 ~ cauchy(0, 5);
    mu_beta ~ cauchy(0, 5);
    mu_phi1 ~ cauchy(0, 5);
    alpha ~ normal(0, sigma_alpha); //determines location
    beta ~ normal(0, sigma_beta);  //allowed to float
    phi1 ~ normal(0, sigma_phi1);  //allowed to float
    theta1 ~ normal(0, 1); //determines location
  y ~ poisson(lambda);
}
