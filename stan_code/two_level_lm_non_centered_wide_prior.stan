data {
  int<lower=0> n;   // number of data items
  int<lower=0> n_id;   // number of respondents

  int<lower=1> id[n]; // respondent id's

  int<lower=0> j;   // number of conjoint attribues (should include intercept)
  int<lower=0> k;   // number of demographic attribues (should include intercept)

  matrix[n, j] X;   // Matrix of profile attributes
  matrix[n_id, k] Z;   // Matrix of personal attributes

  vector[n] y;      // outcome vector
}
parameters {
  real<lower=0> sigma;  // error scale
  vector<lower=0>[j] tau;  // error scale
  /* real<lower=0> tau;  // error scale */

  matrix[k, j] Gamma;
  matrix[n_id, j] Beta_resid;


}
transformed parameters{
  matrix[n_id, j] Beta = Z * Gamma + diag_post_multiply(Beta_resid, tau);
  /* matrix[n_id, j] Beta = Z * Gamma + Beta_resid*tau; */
}
model {

  sigma ~ normal(0,10);
  tau ~ normal(0,10);

  // Priors on Gammas for intercept

  for (i in 1:k){
    Gamma[1,i] ~ normal(0,10);
  }

  // Priors on Gammas
  for (i in 1:k){
    for (j_ in 2:j){
      Gamma[i,j_] ~ normal(0,10);
    }
  }

  to_vector(Beta_resid) ~ normal(0,1);


  vector[n] linear_predictor;
  for (i in 1:n){
    linear_predictor[i] = dot_product(Beta[id[i],], X[i, ]);
  }

  y ~ normal(linear_predictor, sigma);

}

