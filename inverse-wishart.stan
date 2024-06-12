data {
  int<lower=0> N; 
  vector[2] y[N]; 
  vector[2] mu; 
  int<lower=2> prior_nu; 
  cov_matrix[2] prior_sigma; 
} 

parameters {
  cov_matrix[2] sigma; 
}

model {
  //prior 
  sigma ~ inv_wishart(prior_nu, prior_sigma); 
  //posterior 
  for (i in 1:N) {
    y[i] ~ multi_normal(mu, sigma); 
  }
}

