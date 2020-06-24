functions {
  real gpareto_lpdf(vector y, real ymin, real k, real sigma, real s) {
    // generalised Pareto log pdf 
    real N = s;
    real inv_k = inv(k);
    //if (k<0 && max(y-ymin)/sigma > -inv_k)
    //  reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", k, sigma)
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma)
    if (fabs(k) > 1e-15)
      return -(1+inv_k)*sum(log1p((y-ymin) * (k/sigma))) -N*log(sigma);
    else
      return -sum(y-ymin)/sigma -N*log(sigma); // limit k->0
      }
}
data {
   // number of observations
  int<lower=1> N;
  // number of groups
  int<lower=-1> K;
  //observations
  vector<lower=0>[N] y;
  // group sizes 
  int<lower=0> s[K];
  // ymax
  vector<lower=0>[K] ymax;
	//threshold
  vector<lower=0>[K] ymin;

}

parameters {
vector<lower=0>[K] sigma; 
vector<lower=0>[K] k_offset;
}

transformed parameters{
vector[K] k = -sigma ./ (ymax-ymin) + k_offset;
}

model {
	int pos;
	pos = 1;
	
  //priors
  //mu ~ normal(0, 3);
  //sigma_mu ~ normal(0, 3);
  //sigma_overall ~ normal(0, 3);
  //k ~ uniform(-5, 5);
  //sigma ~ exponential(3);
  
  for(i in 1:K){
  segment(y, pos, s[i]) ~ gpareto(ymin[i], k[i], sigma[i], s[i]);
  pos = pos + s[i];
  }
}
