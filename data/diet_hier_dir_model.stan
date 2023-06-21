// generated with brms 2.19.0
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
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_muanimal_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_muanimal_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  int<lower=1> J_3[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_muanimal_1;
  // data for group-level effects of ID 4
  int<lower=1> N_4;  // number of grouping levels
  int<lower=1> M_4;  // number of coefficients per level
  int<lower=1> J_4[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_4_muanimal_1;
  // data for group-level effects of ID 5
  int<lower=1> N_5;  // number of grouping levels
  int<lower=1> M_5;  // number of coefficients per level
  int<lower=1> J_5[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_5_mucyanobacteria_1;
  // data for group-level effects of ID 6
  int<lower=1> N_6;  // number of grouping levels
  int<lower=1> M_6;  // number of coefficients per level
  int<lower=1> J_6[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_6_mucyanobacteria_1;
  // data for group-level effects of ID 7
  int<lower=1> N_7;  // number of grouping levels
  int<lower=1> M_7;  // number of coefficients per level
  int<lower=1> J_7[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_7_mucyanobacteria_1;
  // data for group-level effects of ID 8
  int<lower=1> N_8;  // number of grouping levels
  int<lower=1> M_8;  // number of coefficients per level
  int<lower=1> J_8[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_8_mucyanobacteria_1;
  // data for group-level effects of ID 9
  int<lower=1> N_9;  // number of grouping levels
  int<lower=1> M_9;  // number of coefficients per level
  int<lower=1> J_9[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_9_mudiatom_1;
  // data for group-level effects of ID 10
  int<lower=1> N_10;  // number of grouping levels
  int<lower=1> M_10;  // number of coefficients per level
  int<lower=1> J_10[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_10_mudiatom_1;
  // data for group-level effects of ID 11
  int<lower=1> N_11;  // number of grouping levels
  int<lower=1> M_11;  // number of coefficients per level
  int<lower=1> J_11[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_11_mudiatom_1;
  // data for group-level effects of ID 12
  int<lower=1> N_12;  // number of grouping levels
  int<lower=1> M_12;  // number of coefficients per level
  int<lower=1> J_12[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_12_mudiatom_1;
  // data for group-level effects of ID 13
  int<lower=1> N_13;  // number of grouping levels
  int<lower=1> M_13;  // number of coefficients per level
  int<lower=1> J_13[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_13_mufilamentous_1;
  // data for group-level effects of ID 14
  int<lower=1> N_14;  // number of grouping levels
  int<lower=1> M_14;  // number of coefficients per level
  int<lower=1> J_14[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_14_mufilamentous_1;
  // data for group-level effects of ID 15
  int<lower=1> N_15;  // number of grouping levels
  int<lower=1> M_15;  // number of coefficients per level
  int<lower=1> J_15[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_15_mufilamentous_1;
  // data for group-level effects of ID 16
  int<lower=1> N_16;  // number of grouping levels
  int<lower=1> M_16;  // number of coefficients per level
  int<lower=1> J_16[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_16_mufilamentous_1;
  // data for group-level effects of ID 17
  int<lower=1> N_17;  // number of grouping levels
  int<lower=1> M_17;  // number of coefficients per level
  int<lower=1> J_17[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_17_mugreenalgae_1;
  // data for group-level effects of ID 18
  int<lower=1> N_18;  // number of grouping levels
  int<lower=1> M_18;  // number of coefficients per level
  int<lower=1> J_18[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_18_mugreenalgae_1;
  // data for group-level effects of ID 19
  int<lower=1> N_19;  // number of grouping levels
  int<lower=1> M_19;  // number of coefficients per level
  int<lower=1> J_19[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_19_mugreenalgae_1;
  // data for group-level effects of ID 20
  int<lower=1> N_20;  // number of grouping levels
  int<lower=1> M_20;  // number of coefficients per level
  int<lower=1> J_20[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_20_mugreenalgae_1;
  // data for group-level effects of ID 21
  int<lower=1> N_21;  // number of grouping levels
  int<lower=1> M_21;  // number of coefficients per level
  int<lower=1> J_21[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_21_muplantmaterial_1;
  // data for group-level effects of ID 22
  int<lower=1> N_22;  // number of grouping levels
  int<lower=1> M_22;  // number of coefficients per level
  int<lower=1> J_22[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_22_muplantmaterial_1;
  // data for group-level effects of ID 23
  int<lower=1> N_23;  // number of grouping levels
  int<lower=1> M_23;  // number of coefficients per level
  int<lower=1> J_23[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_23_muplantmaterial_1;
  // data for group-level effects of ID 24
  int<lower=1> N_24;  // number of grouping levels
  int<lower=1> M_24;  // number of coefficients per level
  int<lower=1> J_24[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_24_muplantmaterial_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  real Intercept_muanimal;  // temporary intercept for centered predictors
  real Intercept_mucyanobacteria;  // temporary intercept for centered predictors
  real Intercept_mudiatom;  // temporary intercept for centered predictors
  real Intercept_mufilamentous;  // temporary intercept for centered predictors
  real Intercept_mugreenalgae;  // temporary intercept for centered predictors
  real Intercept_muplantmaterial;  // temporary intercept for centered predictors
  real<lower=0> phi;  // precision parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  vector[N_3] z_3[M_3];  // standardized group-level effects
  vector<lower=0>[M_4] sd_4;  // group-level standard deviations
  vector[N_4] z_4[M_4];  // standardized group-level effects
  vector<lower=0>[M_5] sd_5;  // group-level standard deviations
  vector[N_5] z_5[M_5];  // standardized group-level effects
  vector<lower=0>[M_6] sd_6;  // group-level standard deviations
  vector[N_6] z_6[M_6];  // standardized group-level effects
  vector<lower=0>[M_7] sd_7;  // group-level standard deviations
  vector[N_7] z_7[M_7];  // standardized group-level effects
  vector<lower=0>[M_8] sd_8;  // group-level standard deviations
  vector[N_8] z_8[M_8];  // standardized group-level effects
  vector<lower=0>[M_9] sd_9;  // group-level standard deviations
  vector[N_9] z_9[M_9];  // standardized group-level effects
  vector<lower=0>[M_10] sd_10;  // group-level standard deviations
  vector[N_10] z_10[M_10];  // standardized group-level effects
  vector<lower=0>[M_11] sd_11;  // group-level standard deviations
  vector[N_11] z_11[M_11];  // standardized group-level effects
  vector<lower=0>[M_12] sd_12;  // group-level standard deviations
  vector[N_12] z_12[M_12];  // standardized group-level effects
  vector<lower=0>[M_13] sd_13;  // group-level standard deviations
  vector[N_13] z_13[M_13];  // standardized group-level effects
  vector<lower=0>[M_14] sd_14;  // group-level standard deviations
  vector[N_14] z_14[M_14];  // standardized group-level effects
  vector<lower=0>[M_15] sd_15;  // group-level standard deviations
  vector[N_15] z_15[M_15];  // standardized group-level effects
  vector<lower=0>[M_16] sd_16;  // group-level standard deviations
  vector[N_16] z_16[M_16];  // standardized group-level effects
  vector<lower=0>[M_17] sd_17;  // group-level standard deviations
  vector[N_17] z_17[M_17];  // standardized group-level effects
  vector<lower=0>[M_18] sd_18;  // group-level standard deviations
  vector[N_18] z_18[M_18];  // standardized group-level effects
  vector<lower=0>[M_19] sd_19;  // group-level standard deviations
  vector[N_19] z_19[M_19];  // standardized group-level effects
  vector<lower=0>[M_20] sd_20;  // group-level standard deviations
  vector[N_20] z_20[M_20];  // standardized group-level effects
  vector<lower=0>[M_21] sd_21;  // group-level standard deviations
  vector[N_21] z_21[M_21];  // standardized group-level effects
  vector<lower=0>[M_22] sd_22;  // group-level standard deviations
  vector[N_22] z_22[M_22];  // standardized group-level effects
  vector<lower=0>[M_23] sd_23;  // group-level standard deviations
  vector[N_23] z_23[M_23];  // standardized group-level effects
  vector<lower=0>[M_24] sd_24;  // group-level standard deviations
  vector[N_24] z_24[M_24];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_muanimal_1;  // actual group-level effects
  vector[N_2] r_2_muanimal_1;  // actual group-level effects
  vector[N_3] r_3_muanimal_1;  // actual group-level effects
  vector[N_4] r_4_muanimal_1;  // actual group-level effects
  vector[N_5] r_5_mucyanobacteria_1;  // actual group-level effects
  vector[N_6] r_6_mucyanobacteria_1;  // actual group-level effects
  vector[N_7] r_7_mucyanobacteria_1;  // actual group-level effects
  vector[N_8] r_8_mucyanobacteria_1;  // actual group-level effects
  vector[N_9] r_9_mudiatom_1;  // actual group-level effects
  vector[N_10] r_10_mudiatom_1;  // actual group-level effects
  vector[N_11] r_11_mudiatom_1;  // actual group-level effects
  vector[N_12] r_12_mudiatom_1;  // actual group-level effects
  vector[N_13] r_13_mufilamentous_1;  // actual group-level effects
  vector[N_14] r_14_mufilamentous_1;  // actual group-level effects
  vector[N_15] r_15_mufilamentous_1;  // actual group-level effects
  vector[N_16] r_16_mufilamentous_1;  // actual group-level effects
  vector[N_17] r_17_mugreenalgae_1;  // actual group-level effects
  vector[N_18] r_18_mugreenalgae_1;  // actual group-level effects
  vector[N_19] r_19_mugreenalgae_1;  // actual group-level effects
  vector[N_20] r_20_mugreenalgae_1;  // actual group-level effects
  vector[N_21] r_21_muplantmaterial_1;  // actual group-level effects
  vector[N_22] r_22_muplantmaterial_1;  // actual group-level effects
  vector[N_23] r_23_muplantmaterial_1;  // actual group-level effects
  vector[N_24] r_24_muplantmaterial_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  r_1_muanimal_1 = (sd_1[1] * (z_1[1]));
  r_2_muanimal_1 = (sd_2[1] * (z_2[1]));
  r_3_muanimal_1 = (sd_3[1] * (z_3[1]));
  r_4_muanimal_1 = (sd_4[1] * (z_4[1]));
  r_5_mucyanobacteria_1 = (sd_5[1] * (z_5[1]));
  r_6_mucyanobacteria_1 = (sd_6[1] * (z_6[1]));
  r_7_mucyanobacteria_1 = (sd_7[1] * (z_7[1]));
  r_8_mucyanobacteria_1 = (sd_8[1] * (z_8[1]));
  r_9_mudiatom_1 = (sd_9[1] * (z_9[1]));
  r_10_mudiatom_1 = (sd_10[1] * (z_10[1]));
  r_11_mudiatom_1 = (sd_11[1] * (z_11[1]));
  r_12_mudiatom_1 = (sd_12[1] * (z_12[1]));
  r_13_mufilamentous_1 = (sd_13[1] * (z_13[1]));
  r_14_mufilamentous_1 = (sd_14[1] * (z_14[1]));
  r_15_mufilamentous_1 = (sd_15[1] * (z_15[1]));
  r_16_mufilamentous_1 = (sd_16[1] * (z_16[1]));
  r_17_mugreenalgae_1 = (sd_17[1] * (z_17[1]));
  r_18_mugreenalgae_1 = (sd_18[1] * (z_18[1]));
  r_19_mugreenalgae_1 = (sd_19[1] * (z_19[1]));
  r_20_mugreenalgae_1 = (sd_20[1] * (z_20[1]));
  r_21_muplantmaterial_1 = (sd_21[1] * (z_21[1]));
  r_22_muplantmaterial_1 = (sd_22[1] * (z_22[1]));
  r_23_muplantmaterial_1 = (sd_23[1] * (z_23[1]));
  r_24_muplantmaterial_1 = (sd_24[1] * (z_24[1]));
  lprior += normal_lpdf(Intercept_muanimal | 0, 0.2);
  lprior += normal_lpdf(Intercept_mucyanobacteria | 0, 0.2);
  lprior += normal_lpdf(Intercept_mudiatom | 1, 2);
  lprior += normal_lpdf(Intercept_mufilamentous | 0, 3);
  lprior += normal_lpdf(Intercept_mugreenalgae | 0, 3);
  lprior += normal_lpdf(Intercept_muplantmaterial | 0, 1);
  lprior += gamma_lpdf(phi | 0.01, 0.01);
  lprior += normal_lpdf(sd_1 | 0, 0.5)
    - 1 * normal_lccdf(0 | 0, 0.5);
  lprior += normal_lpdf(sd_2 | 0, 0.5)
    - 1 * normal_lccdf(0 | 0, 0.5);
  lprior += normal_lpdf(sd_3 | 0, 0.5)
    - 1 * normal_lccdf(0 | 0, 0.5);
  lprior += normal_lpdf(sd_4 | 0, 0.5)
    - 1 * normal_lccdf(0 | 0, 0.5);
  lprior += normal_lpdf(sd_5 | 0, 0.5)
    - 1 * normal_lccdf(0 | 0, 0.5);
  lprior += normal_lpdf(sd_6 | 0, 0.5)
    - 1 * normal_lccdf(0 | 0, 0.5);
  lprior += normal_lpdf(sd_7 | 0, 0.5)
    - 1 * normal_lccdf(0 | 0, 0.5);
  lprior += normal_lpdf(sd_8 | 0, 0.5)
    - 1 * normal_lccdf(0 | 0, 0.5);
  lprior += student_t_lpdf(sd_9 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_10 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_11 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_12 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_13 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_14 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_15 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_16 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_17 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_18 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_19 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_20 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_21 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_22 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_23 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_24 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] muanimal = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] mucyanobacteria = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] mudiatom = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] mufilamentous = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] mugreenalgae = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] muplantmaterial = rep_vector(0.0, N);
    // linear predictor matrix
    vector[ncat] mu[N];
    muanimal += Intercept_muanimal;
    mucyanobacteria += Intercept_mucyanobacteria;
    mudiatom += Intercept_mudiatom;
    mufilamentous += Intercept_mufilamentous;
    mugreenalgae += Intercept_mugreenalgae;
    muplantmaterial += Intercept_muplantmaterial;
    for (n in 1:N) {
      // add more terms to the linear predictor
      muanimal[n] += r_1_muanimal_1[J_1[n]] * Z_1_muanimal_1[n] + r_2_muanimal_1[J_2[n]] * Z_2_muanimal_1[n] + r_3_muanimal_1[J_3[n]] * Z_3_muanimal_1[n] + r_4_muanimal_1[J_4[n]] * Z_4_muanimal_1[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      mucyanobacteria[n] += r_5_mucyanobacteria_1[J_5[n]] * Z_5_mucyanobacteria_1[n] + r_6_mucyanobacteria_1[J_6[n]] * Z_6_mucyanobacteria_1[n] + r_7_mucyanobacteria_1[J_7[n]] * Z_7_mucyanobacteria_1[n] + r_8_mucyanobacteria_1[J_8[n]] * Z_8_mucyanobacteria_1[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      mudiatom[n] += r_9_mudiatom_1[J_9[n]] * Z_9_mudiatom_1[n] + r_10_mudiatom_1[J_10[n]] * Z_10_mudiatom_1[n] + r_11_mudiatom_1[J_11[n]] * Z_11_mudiatom_1[n] + r_12_mudiatom_1[J_12[n]] * Z_12_mudiatom_1[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      mufilamentous[n] += r_13_mufilamentous_1[J_13[n]] * Z_13_mufilamentous_1[n] + r_14_mufilamentous_1[J_14[n]] * Z_14_mufilamentous_1[n] + r_15_mufilamentous_1[J_15[n]] * Z_15_mufilamentous_1[n] + r_16_mufilamentous_1[J_16[n]] * Z_16_mufilamentous_1[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      mugreenalgae[n] += r_17_mugreenalgae_1[J_17[n]] * Z_17_mugreenalgae_1[n] + r_18_mugreenalgae_1[J_18[n]] * Z_18_mugreenalgae_1[n] + r_19_mugreenalgae_1[J_19[n]] * Z_19_mugreenalgae_1[n] + r_20_mugreenalgae_1[J_20[n]] * Z_20_mugreenalgae_1[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      muplantmaterial[n] += r_21_muplantmaterial_1[J_21[n]] * Z_21_muplantmaterial_1[n] + r_22_muplantmaterial_1[J_22[n]] * Z_22_muplantmaterial_1[n] + r_23_muplantmaterial_1[J_23[n]] * Z_23_muplantmaterial_1[n] + r_24_muplantmaterial_1[J_24[n]] * Z_24_muplantmaterial_1[n];
    }
    for (n in 1:N) {
      mu[n] = transpose([0, muanimal[n], mucyanobacteria[n], mudiatom[n], mufilamentous[n], mugreenalgae[n], muplantmaterial[n]]);
    }
    for (n in 1:N) {
      target += dirichlet_logit_lpdf(Y[n] | mu[n], phi);
    }
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_3[1]);
  target += std_normal_lpdf(z_4[1]);
  target += std_normal_lpdf(z_5[1]);
  target += std_normal_lpdf(z_6[1]);
  target += std_normal_lpdf(z_7[1]);
  target += std_normal_lpdf(z_8[1]);
  target += std_normal_lpdf(z_9[1]);
  target += std_normal_lpdf(z_10[1]);
  target += std_normal_lpdf(z_11[1]);
  target += std_normal_lpdf(z_12[1]);
  target += std_normal_lpdf(z_13[1]);
  target += std_normal_lpdf(z_14[1]);
  target += std_normal_lpdf(z_15[1]);
  target += std_normal_lpdf(z_16[1]);
  target += std_normal_lpdf(z_17[1]);
  target += std_normal_lpdf(z_18[1]);
  target += std_normal_lpdf(z_19[1]);
  target += std_normal_lpdf(z_20[1]);
  target += std_normal_lpdf(z_21[1]);
  target += std_normal_lpdf(z_22[1]);
  target += std_normal_lpdf(z_23[1]);
  target += std_normal_lpdf(z_24[1]);
}
generated quantities {
  // actual population-level intercept
  real b_muanimal_Intercept = Intercept_muanimal;
  // actual population-level intercept
  real b_mucyanobacteria_Intercept = Intercept_mucyanobacteria;
  // actual population-level intercept
  real b_mudiatom_Intercept = Intercept_mudiatom;
  // actual population-level intercept
  real b_mufilamentous_Intercept = Intercept_mufilamentous;
  // actual population-level intercept
  real b_mugreenalgae_Intercept = Intercept_mugreenalgae;
  // actual population-level intercept
  real b_muplantmaterial_Intercept = Intercept_muplantmaterial;
}
