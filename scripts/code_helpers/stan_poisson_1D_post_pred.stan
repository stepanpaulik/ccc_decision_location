data {
  int J; // Legal decisions
  int K; //
  int N; // N = J x K
  int N_samples; // number of models from posterior
  matrix[N_samples, K] alpha; //The systematic parameters
  matrix[N_samples, K] phi1; //phi1 is the systematic component phi
  vector[N_samples] mu_phi1;
  matrix[N_samples, J] theta1;
  matrix[N_samples, J] beta;
  vector[N_samples] mu_beta;
  int jj[N]; // Legal decision for legal source n
  int kk[N]; // Legal source for legal decision n
  vector[N_samples] gamma;
}
parameters {
}
transformed parameters {
}
model {
}
generated  quantities {
  matrix[N_samples, N] lambda;
  matrix[N_samples, N] y;
  for (i in 1:N_samples){
    for (n in 1:N){
      lambda[i, n] = exp(alpha[i, kk[n]] + (beta[i, jj[n]] + mu_beta[i]) -
        gamma[i] * (square( theta1[i, jj[n]] - (phi1[i, kk[n]] + mu_phi1[i]) )));
      y[i, n] = poisson_rng(lambda[i, n]);
    }
  }
}
