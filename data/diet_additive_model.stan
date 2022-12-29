//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// generated with brms 2.14.4
functions {
  /* dirichlet-logit log-PDF
   * Args: 
   *   y: vector of real response values
   *   mu: vector of category logit probabilities
   *   phi: precision parameter
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
   real dirichlet_logit_lpdf(vector y, vector mu, real phi) {
     return dirichlet_lpdf(y | softmax(mu) * phi);
   }
}
data {
  int<lower=1> N;  // total number of observations
  int<lower=2> ncat;  // number of categories
  vector[ncat] Y[N];  // response array
  int<lower=1> K_muanimal;  // number of population-level effects
  matrix[N, K_muanimal] X_muanimal;  // population-level design matrix
  int<lower=1> K_mucyanobacteria;  // number of population-level effects
  matrix[N, K_mucyanobacteria] X_mucyanobacteria;  // population-level design matrix
  int<lower=1> K_mudiatom;  // number of population-level effects
  matrix[N, K_mudiatom] X_mudiatom;  // population-level design matrix
  int<lower=1> K_mufilamentous;  // number of population-level effects
  matrix[N, K_mufilamentous] X_mufilamentous;  // population-level design matrix
  int<lower=1> K_mugreenalgae;  // number of population-level effects
  matrix[N, K_mugreenalgae] X_mugreenalgae;  // population-level design matrix
  int<lower=1> K_muplantmaterial;  // number of population-level effects
  matrix[N, K_muplantmaterial] X_muplantmaterial;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc_muanimal = K_muanimal - 1;
  matrix[N, Kc_muanimal] Xc_muanimal;  // centered version of X_muanimal without an intercept
  vector[Kc_muanimal] means_X_muanimal;  // column means of X_muanimal before centering
  int Kc_mucyanobacteria = K_mucyanobacteria - 1;
  matrix[N, Kc_mucyanobacteria] Xc_mucyanobacteria;  // centered version of X_mucyanobacteria without an intercept
  vector[Kc_mucyanobacteria] means_X_mucyanobacteria;  // column means of X_mucyanobacteria before centering
  int Kc_mudiatom = K_mudiatom - 1;
  matrix[N, Kc_mudiatom] Xc_mudiatom;  // centered version of X_mudiatom without an intercept
  vector[Kc_mudiatom] means_X_mudiatom;  // column means of X_mudiatom before centering
  int Kc_mufilamentous = K_mufilamentous - 1;
  matrix[N, Kc_mufilamentous] Xc_mufilamentous;  // centered version of X_mufilamentous without an intercept
  vector[Kc_mufilamentous] means_X_mufilamentous;  // column means of X_mufilamentous before centering
  int Kc_mugreenalgae = K_mugreenalgae - 1;
  matrix[N, Kc_mugreenalgae] Xc_mugreenalgae;  // centered version of X_mugreenalgae without an intercept
  vector[Kc_mugreenalgae] means_X_mugreenalgae;  // column means of X_mugreenalgae before centering
  int Kc_muplantmaterial = K_muplantmaterial - 1;
  matrix[N, Kc_muplantmaterial] Xc_muplantmaterial;  // centered version of X_muplantmaterial without an intercept
  vector[Kc_muplantmaterial] means_X_muplantmaterial;  // column means of X_muplantmaterial before centering
  for (i in 2:K_muanimal) {
    means_X_muanimal[i - 1] = mean(X_muanimal[, i]);
    Xc_muanimal[, i - 1] = X_muanimal[, i] - means_X_muanimal[i - 1];
  }
  for (i in 2:K_mucyanobacteria) {
    means_X_mucyanobacteria[i - 1] = mean(X_mucyanobacteria[, i]);
    Xc_mucyanobacteria[, i - 1] = X_mucyanobacteria[, i] - means_X_mucyanobacteria[i - 1];
  }
  for (i in 2:K_mudiatom) {
    means_X_mudiatom[i - 1] = mean(X_mudiatom[, i]);
    Xc_mudiatom[, i - 1] = X_mudiatom[, i] - means_X_mudiatom[i - 1];
  }
  for (i in 2:K_mufilamentous) {
    means_X_mufilamentous[i - 1] = mean(X_mufilamentous[, i]);
    Xc_mufilamentous[, i - 1] = X_mufilamentous[, i] - means_X_mufilamentous[i - 1];
  }
  for (i in 2:K_mugreenalgae) {
    means_X_mugreenalgae[i - 1] = mean(X_mugreenalgae[, i]);
    Xc_mugreenalgae[, i - 1] = X_mugreenalgae[, i] - means_X_mugreenalgae[i - 1];
  }
  for (i in 2:K_muplantmaterial) {
    means_X_muplantmaterial[i - 1] = mean(X_muplantmaterial[, i]);
    Xc_muplantmaterial[, i - 1] = X_muplantmaterial[, i] - means_X_muplantmaterial[i - 1];
  }
}
parameters {
  vector[Kc_muanimal] b_muanimal;  // population-level effects
  real Intercept_muanimal;  // temporary intercept for centered predictors
  vector[Kc_mucyanobacteria] b_mucyanobacteria;  // population-level effects
  real Intercept_mucyanobacteria;  // temporary intercept for centered predictors
  vector[Kc_mudiatom] b_mudiatom;  // population-level effects
  real Intercept_mudiatom;  // temporary intercept for centered predictors
  vector[Kc_mufilamentous] b_mufilamentous;  // population-level effects
  real Intercept_mufilamentous;  // temporary intercept for centered predictors
  vector[Kc_mugreenalgae] b_mugreenalgae;  // population-level effects
  real Intercept_mugreenalgae;  // temporary intercept for centered predictors
  vector[Kc_muplantmaterial] b_muplantmaterial;  // population-level effects
  real Intercept_muplantmaterial;  // temporary intercept for centered predictors
  real<lower=0> phi;  // precision parameter
}
transformed parameters {
}
model {
  // likelihood including all constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] muanimal = Intercept_muanimal + Xc_muanimal * b_muanimal;
    // initialize linear predictor term
    vector[N] mucyanobacteria = Intercept_mucyanobacteria + Xc_mucyanobacteria * b_mucyanobacteria;
    // initialize linear predictor term
    vector[N] mudiatom = Intercept_mudiatom + Xc_mudiatom * b_mudiatom;
    // initialize linear predictor term
    vector[N] mufilamentous = Intercept_mufilamentous + Xc_mufilamentous * b_mufilamentous;
    // initialize linear predictor term
    vector[N] mugreenalgae = Intercept_mugreenalgae + Xc_mugreenalgae * b_mugreenalgae;
    // initialize linear predictor term
    vector[N] muplantmaterial = Intercept_muplantmaterial + Xc_muplantmaterial * b_muplantmaterial;
    // linear predictor matrix
    vector[ncat] mu[N];
    for (n in 1:N) {
      mu[n] = transpose([0, muanimal[n], mucyanobacteria[n], mudiatom[n], mufilamentous[n], mugreenalgae[n], muplantmaterial[n]]);
    }
    for (n in 1:N) {
      target += dirichlet_logit_lpdf(Y[n] | mu[n], phi);
    }
  }
  // priors including all constants
  target += student_t_lpdf(Intercept_muanimal | 3, 0, 2.5);
  target += student_t_lpdf(Intercept_mucyanobacteria | 3, 0, 2.5);
  target += student_t_lpdf(Intercept_mudiatom | 3, 0, 2.5);
  target += student_t_lpdf(Intercept_mufilamentous | 3, 0, 2.5);
  target += student_t_lpdf(Intercept_mugreenalgae | 3, 0, 2.5);
  target += student_t_lpdf(Intercept_muplantmaterial | 3, 0, 2.5);
  target += gamma_lpdf(phi | 0.01, 0.01);
}
generated quantities {
  // actual population-level intercept
  real b_muanimal_Intercept = Intercept_muanimal - dot_product(means_X_muanimal, b_muanimal);
  // actual population-level intercept
  real b_mucyanobacteria_Intercept = Intercept_mucyanobacteria - dot_product(means_X_mucyanobacteria, b_mucyanobacteria);
  // actual population-level intercept
  real b_mudiatom_Intercept = Intercept_mudiatom - dot_product(means_X_mudiatom, b_mudiatom);
  // actual population-level intercept
  real b_mufilamentous_Intercept = Intercept_mufilamentous - dot_product(means_X_mufilamentous, b_mufilamentous);
  // actual population-level intercept
  real b_mugreenalgae_Intercept = Intercept_mugreenalgae - dot_product(means_X_mugreenalgae, b_mugreenalgae);
  // actual population-level intercept
  real b_muplantmaterial_Intercept = Intercept_muplantmaterial - dot_product(means_X_muplantmaterial, b_muplantmaterial);
}

