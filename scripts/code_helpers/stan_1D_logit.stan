data {
int<lower=1> J; // Legal decisions
int<lower=1> K; //
int<lower=1> N; // N = J x K
int<lower=1,upper=J> jj[N]; // Legal decision for legal source n
int<lower=1,upper=K> kk[N]; // Legal source for legal decision n
int<lower=0,upper=1> y[N]; // dummy if legal decision j cites legal source k
int<lower=1> left_anchor; // the left anchor position for theta
int<lower=1> right_anchor; // the right anchor position for theta
}
parameters {
vector[K] alpha; //The systematic parameters
vector[K] phi1;
vector[J] theta1;
vector[J] beta;
real mu_beta;
real<lower=0.1> sigma_beta;
real mu_phi1;
real<lower=0.1> sigma_phi1;
real gamma;
}
transformed parameters {
  vector[J] theta; // the rotation corrected theta estimate
  if (theta1[left_anchor] < theta1[right_anchor]) {theta = theta1;}
    else {theta = theta1 * -1;}
}
model {
alpha ~ normal(0, 1);            //determines location
beta ~ normal(mu_beta, sigma_beta); //allowed to float
phi1 ~ normal(mu_phi1, sigma_phi1);   //allowed to float
theta1 ~ normal(0, 1);          //determines location
for (n in 1:N){
    y[n] ~ bernoulli_logit( alpha[kk[n]] + beta[jj[n]] -
    gamma * (square( theta1[jj[n]] - phi1[kk[n]] )));
}
}
