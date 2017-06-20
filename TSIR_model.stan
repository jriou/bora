data {
  // data
  int W; // number of records
  int O_t[W]; // number of reported cases
  int sumO_t[W]; // cumulative number of reported cases
  int pop; // island total population
  int siL; // length of discretized serial interval distribution (in weeks)
  vector[siL] siW; // discretized serial interval distribution

  // prior settings
  int P_R0_type; // 1=exponential; 2=uniform; 3=normal; 4=gamma; 0=hyperprior
  int P_rho_type; // 1=beta; 2=uniform; 0=hyperprior
  real<lower=0> P_R0[2]; 
  real<lower=0> P_rho[2];

  // prediction
  int pW; // number of weeks of prediction
}

parameters { 
  // declaring parameters
  real<lower=0> R0; // island-level base transmission 
  real<lower=0,upper=1> rho; // reporting rate
  real<lower=0> phi;
}

transformed parameters {
  // declaring model intermediates
  real<lower=0> lp[W]; // mean prediction
  real<lower=0> sampledisp[W]; // dispersion = mean/phi
  vector[siL] lagO; // past reported cases
  real Ostar_t; // exposition
  
  for(i in 1:W) {
    // computing exposition from past reported cases
    if(i>1) {
      for(j in 1:siL) {
        if((i-j)>1) {
          lagO[j] = O_t[i-j];
        } else {
          lagO[j] = 0;
        }
      }
      Ostar_t = sum(lagO .* siW);
    } else {
      Ostar_t = 0;
    }
    // building negative binomial model
    lp[i] = R0 * Ostar_t * ( 1 - sumO_t[i] / ( rho * pop ) );
    if(lp[i]==0) lp[i] = 0.0001;
    sampledisp[i] = lp[i]/phi;
  }
}

model {
  // priors -----------------------------------------
  
  // prior for R0 
  if(P_R0_type==1) R0 ~ exponential(P_R0[1]);
  if(P_R0_type==2) R0 ~ uniform(P_R0[1],P_R0[2]);
  if(P_R0_type==3) R0 ~ normal(P_R0[1],P_R0[2]);
  if(P_R0_type==4) R0 ~ gamma(P_R0[1],P_R0[2]);

  // prior for rho 
  if(P_rho_type==1) rho ~ beta(P_rho[1],P_rho[2]);
  if(P_rho_type==2) rho ~ uniform(P_rho[1],P_rho[2]);

  // prior for phi
  phi ~ cauchy(0,2.5);

  // likelihood
  target += neg_binomial_2_lpmf(O_t|lp,sampledisp);
}

generated quantities {
  real log_lik[W]; // save the likelihood
  real pred_lp[W]; // predicted values
  real resid_lp[W]; // residuals
  // transformed outcomes
  real totlp;
  real totoverall;
  real attackrate;
  // prediction
  real pS;
  vector[siL] lagpO;
  real pOstar;
  real pO[pW];
  real plp;
  real ptotlp;
  real ptotoverall;
  real pattackrate;

  // diagnostics
  for (i in 1:W) {
    log_lik[i] = neg_binomial_2_lpmf(O_t[i]|lp[i],sampledisp[i]);
    pred_lp[i] = neg_binomial_2_rng(lp[i],sampledisp[i]);
    resid_lp[i] = O_t[i] - pred_lp[i];
  }
  
  // transformed outcomes
  totlp = sum(lp);
  totoverall = totlp / rho;
  attackrate = totoverall / pop;

  // prediction for ZIKV in STM, GUA, MRT
  pS = pop - totlp / rho ;
  lagpO = tail(to_vector(O_t),siL);
  for(week in 1:pW) {
    pOstar = 0;
    for (j in 1:siL) {
      pOstar = pOstar + (lagpO[siL+1-j] * siW[j]);
    }
    plp = R0 * pOstar * pS / pop;
    pO[week] = 0;
    if(plp>0) {
      pO[week] = neg_binomial_2_rng(plp,plp/phi);
    }
    pS = pS - ( pO[week] / rho );
    for(j in 1:(siL-1)) {
      lagpO[j] = lagpO[j+1];
    }
    lagpO[siL] = pO[week];
  }
  ptotlp = sum(pO) + totlp;
  ptotoverall = ptotlp / rho;
  pattackrate = ptotoverall / pop ;
}
 