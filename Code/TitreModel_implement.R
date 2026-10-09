
library(rstan)

titremodel_txt <- "
data{
int<lower=1> K_virus;
int<lower=1> K_city;
int<lower=1> T_num;

int<lower=1> N0;
int<lower=1> age0[N0];
matrix<lower=0,upper=1>[T_num,N0] age_weight0;
matrix<lower=0>[K_virus,N0] titre0;
int<lower=1,upper=K_city> city_index0[N0];

int<lower=1> N1;
int<lower=1> age1[N1];
int<lower=1,upper=K_virus> pos_index1_1[N1];
int<lower=1,upper=K_virus> neg_index1_1[N1];
int<lower=1,upper=K_virus> neg_index1_2[N1];
int<lower=1,upper=K_virus> neg_index1_3[N1];
matrix<lower=0,upper=1>[T_num,N1] age_weight1;
matrix<lower=0,upper=1>[T_num,N1] age_weight_primary1;
matrix<lower=0,upper=1>[T_num,N1] age_weight_other1;
matrix<lower=0>[K_virus,N1] titre1;
int<lower=1,upper=K_city> city_index1[N1];


int<lower=1> N2;
int<lower=1> age2[N2];
int<lower=1,upper=K_virus> pos_index2_1[N2];
int<lower=1,upper=K_virus> pos_index2_2[N2];
int<lower=1,upper=K_virus> neg_index2_1[N2];
int<lower=1,upper=K_virus> neg_index2_2[N2];
matrix<lower=0,upper=1>[T_num,N2] age_weight2;
matrix<lower=0,upper=1>[T_num,N2] age_weight_primary2;
matrix<lower=0,upper=1>[T_num,N2] age_weight_other2;
matrix<lower=0>[K_virus,N2] titre2;
int<lower=1,upper=K_city> city_index2[N2];

int<lower=1> N3;
int<lower=1> age3[N3];
int<lower=1,upper=K_virus> pos_index3_1[N3];
int<lower=1,upper=K_virus> pos_index3_2[N3];
int<lower=1,upper=K_virus> pos_index3_3[N3];
int<lower=1,upper=K_virus> neg_index3_1[N3];
matrix<lower=0,upper=1>[T_num,N3] age_weight3;
matrix<lower=0,upper=1>[T_num,N3] age_weight_primary3;
matrix<lower=0,upper=1>[T_num,N3] age_weight_other3;
matrix<lower=0>[K_virus,N3] titre3;
int<lower=1,upper=K_city> city_index3[N3];

int<lower=1> N4;
int<lower=1> age4[N4];
matrix<lower=0,upper=1>[T_num,N4] age_weight4;
matrix<lower=0,upper=1>[T_num,N4] age_weight_primary4;
matrix<lower=0,upper=1>[T_num,N4] age_weight_other4;
matrix<lower=0>[K_virus,N4] titre4;
int<lower=1,upper=K_city> city_index4[N4];

real b1;
real<lower=0> ymax;


}
parameters{
real<lower=0> lambda_mean1;
matrix<lower=0,upper=1>[K_virus,T_num] z1;

real<lower=0> lambda_mean2;
matrix<lower=0,upper=1>[K_virus,T_num] z2;

real<lower=0> l[K_virus];
real<lower=0.01,upper=0.99> beta12;
real<lower=0.01,upper=0.99> beta13;
real<lower=0.01,upper=0.99> beta14;
real<lower=0.01,upper=0.99> beta21;
real<lower=0.01,upper=0.99> beta23;
real<lower=0.01,upper=0.99> beta24;
real<lower=0.01,upper=0.99> beta31;
real<lower=0.01,upper=0.99> beta32;
real<lower=0.01,upper=0.99> beta34;
real<lower=0.01,upper=0.99> beta41;
real<lower=0.01,upper=0.99> beta42;
real<lower=0.01,upper=0.99> beta43;

real<lower=0.01,upper=0.99> beta12_3;
real<lower=0.01,upper=0.99> beta12_4;
real<lower=0.01,upper=0.99> beta13_2;
real<lower=0.01,upper=0.99> beta13_4;
real<lower=0.01,upper=0.99> beta14_2;
real<lower=0.01,upper=0.99> beta14_3;
real<lower=0.01,upper=0.99> beta23_1;
real<lower=0.01,upper=0.99> beta23_4;
real<lower=0.01,upper=0.99> beta24_1;
real<lower=0.01,upper=0.99> beta24_3;
real<lower=0.01,upper=0.99> beta34_1;
real<lower=0.01,upper=0.99> beta34_2;

real<lower=0.01,upper=0.99> beta123_4;
real<lower=0.01,upper=0.99> beta124_3;
real<lower=0.01,upper=0.99> beta134_2;
real<lower=0.01,upper=0.99> beta234_1;

vector<lower=0>[K_virus] shape_0;
vector<lower=0>[K_virus] rate_0;

vector<lower=0>[K_virus] mu_infec_boost;
vector<lower=0>[K_virus] mu_cross_boost;
vector<lower=0>[K_virus] mu_cross_boost_neg;

vector<lower=0>[2] boost_expose;
vector<lower=0>[3] boost_unexpose;

vector<lower=0,upper=1>[K_city] prop_other_exposure;
vector<lower=0>[K_virus] add0;

real<lower=0,upper=1> prop_cross_neg;
vector<lower=0>[K_virus] sigma;

vector<lower=0>[K_virus] rate_up;
}
transformed parameters{
vector[K_virus] mu_0;

matrix<lower=0>[K_virus,T_num] lambda[2]; 
matrix<lower=0.01,upper=0.99>[K_virus,K_virus] beta_1;
real beta_2[K_virus,K_virus,K_virus];
vector[K_virus] beta_3;

mu_0[1] = shape_0[1]/rate_0[1];
mu_0[2] = shape_0[2]/rate_0[2];
mu_0[3] = shape_0[3]/rate_0[3];
mu_0[4] = shape_0[4]/rate_0[4];

lambda[1] = -log(z1)/lambda_mean1;
lambda[2] = -log(z2)/lambda_mean2;

beta_1[1,1] = b1;
beta_1[2,2] = b1;
beta_1[3,3] = b1;
beta_1[4,4] = b1;
beta_1[1,2] = beta12;
beta_1[1,3] = beta13;
beta_1[1,4] = beta14;
beta_1[2,1] = beta21;
beta_1[2,3] = beta23;
beta_1[2,4] = beta24;
beta_1[3,1] = beta31;
beta_1[3,2] = beta32;
beta_1[3,4] = beta34;
beta_1[4,1] = beta41;
beta_1[4,2] = beta42;
beta_1[4,3] = beta43;

beta_2[1,1,1] = b1;
beta_2[1,1,2] = b1;
beta_2[1,1,3] = b1;
beta_2[1,1,4] = b1;
beta_2[1,2,1] = b1;
beta_2[1,2,2] = b1;
beta_2[1,2,3] = beta12_3;
beta_2[1,2,4] = beta12_4;
beta_2[1,3,1] = b1;
beta_2[1,3,2] = beta13_2;
beta_2[1,3,3] = b1;
beta_2[1,3,4] = beta13_4;
beta_2[1,4,1] = b1;
beta_2[1,4,2] = beta14_2;
beta_2[1,4,3] = beta14_3;
beta_2[1,4,4] = b1;

beta_2[2,1,1] = b1;
beta_2[2,1,2] = b1;
beta_2[2,1,3] = beta12_3;
beta_2[2,1,4] = beta12_4;
beta_2[2,2,1] = b1;
beta_2[2,2,2] = b1;
beta_2[2,2,3] = b1;
beta_2[2,2,4] = b1;
beta_2[2,3,1] = beta23_1;
beta_2[2,3,2] = b1;
beta_2[2,3,3] = b1;
beta_2[2,3,4] = beta23_4;
beta_2[2,4,1] = beta24_1;
beta_2[2,4,2] = b1;
beta_2[2,4,3] = beta24_3;
beta_2[2,4,4] = b1;

beta_2[3,1,1] = b1;
beta_2[3,1,2] = beta13_2;
beta_2[3,1,3] = b1;
beta_2[3,1,4] = beta13_4;
beta_2[3,2,1] = beta23_1;
beta_2[3,2,2] = b1;
beta_2[3,2,3] = b1;
beta_2[3,2,4] = beta23_4;
beta_2[3,3,1] = b1;
beta_2[3,3,2] = b1;
beta_2[3,3,3] = b1;
beta_2[3,3,4] = b1;
beta_2[3,4,1] = beta34_1;
beta_2[3,4,2] = beta34_2;
beta_2[3,4,3] = b1;
beta_2[3,4,4] = b1;

beta_2[4,1,1] = b1;
beta_2[4,1,2] = beta14_2;
beta_2[4,1,3] = beta14_3;
beta_2[4,1,4] = b1;
beta_2[4,2,1] = beta24_1;
beta_2[4,2,2] = b1;
beta_2[4,2,3] = beta24_3;
beta_2[4,2,4] = b1;
beta_2[4,3,1] = beta34_1;
beta_2[4,3,2] = beta34_2;
beta_2[4,3,3] = b1;
beta_2[4,3,4] = b1;
beta_2[4,4,1] = b1;
beta_2[4,4,2] = b1;
beta_2[4,4,3] = b1;
beta_2[4,4,4] = b1;

beta_3[1] = beta234_1;
beta_3[2] = beta134_2;
beta_3[3] = beta124_3;
beta_3[4] = beta123_4;

}
model{

for (i in 1:N0){
vector[2] lps;
lps[1] = -sum( lambda[city_index0[i]] * age_weight0[,i] )*age0[i]
+ log1m(prop_other_exposure[city_index0[i]])
+ gamma_lpdf( titre0[1,i] | shape_0[1],rate_0[1] )
+ gamma_lpdf( titre0[2,i] | shape_0[2],rate_0[2] )
+ gamma_lpdf( titre0[3,i] | shape_0[3],rate_0[3] )
+ gamma_lpdf( titre0[4,i] | shape_0[4],rate_0[4] );

lps[2] =  -sum( lambda[city_index0[i]] * age_weight0[,i] )*age0[i]
+ log(prop_other_exposure[city_index0[i]])
+ normal_lpdf( titre0[1,i] | mu_0[1] + add0[1],sigma[1] )
+ normal_lpdf( titre0[2,i] | mu_0[2] + add0[2],sigma[2] )
+ normal_lpdf( titre0[3,i] | mu_0[3] + add0[3],sigma[3] )
+ normal_lpdf( titre0[4,i] | mu_0[4] + add0[4],sigma[4] );

target += log_sum_exp(lps);
}

for (i in 1:N1){
target += log( (1-exp(-lambda[city_index1[i]][pos_index1_1[i],]*age_weight_other1[,i]*log1p_exp(20*(age1[i]-l[pos_index1_1[i]]))/20 ))*exp( -(lambda[city_index1[i]][neg_index1_1[i],]*age_weight1[,i] + lambda[city_index1[i]][neg_index1_2[i],]*age_weight1[,i]+lambda[city_index1[i]][neg_index1_3[i],]*age_weight1[,i])*log1p_exp(20*(age1[i]-l[pos_index1_1[i]]))/20 )
+ exp( - sum( lambda[city_index1[i]] * age_weight_other1[,i] )*log1p_exp(20*(age1[i]-l[pos_index1_1[i]]))/20 )*lambda[city_index1[i]][pos_index1_1[i],]*age_weight_primary1[,i]/(sum( lambda[city_index1[i]] * age_weight_primary1[,i]) )*( 1-exp( -sum( lambda[city_index1[i]] * age_weight_primary1[,i] )*( age1[i]-log1p_exp(20*(age1[i]-l[pos_index1_1[i]]))/20 ) ) ) )
+ log1m(beta_1[pos_index1_1[i],neg_index1_1[i]])
+ log1m(beta_1[pos_index1_1[i],neg_index1_2[i]])
+ log1m(beta_1[pos_index1_1[i],neg_index1_3[i]])
+ log_sum_exp( log1m(prop_other_exposure[city_index1[i]]) + normal_lpdf( titre1[pos_index1_1[i],i] | mu_0[pos_index1_1[i]] + mu_infec_boost[pos_index1_1[i]],sigma[pos_index1_1[i]] ),
log(prop_other_exposure[city_index1[i]]) + normal_lpdf( titre1[pos_index1_1[i],i] | mu_0[pos_index1_1[i]] + mu_infec_boost[pos_index1_1[i]] + add0[pos_index1_1[i]],sigma[pos_index1_1[i]] ) )
+ log_sum_exp( log1m(prop_cross_neg) + gamma_lpdf( titre1[neg_index1_1[i],i] |  (mu_0[neg_index1_1[i]] + boost_unexpose[1])*rate_0[neg_index1_1[i]] ,rate_0[neg_index1_1[i]] ),
log(prop_cross_neg) + normal_lpdf( titre1[neg_index1_1[i],i] | mu_0[neg_index1_1[i]] + mu_cross_boost_neg[neg_index1_1[i]] + boost_unexpose[1],sigma[neg_index1_1[i]]) )
+ log_sum_exp( log1m(prop_cross_neg) + gamma_lpdf( titre1[neg_index1_2[i],i] |  (mu_0[neg_index1_2[i]] + boost_unexpose[1])*rate_0[neg_index1_2[i]] ,rate_0[neg_index1_2[i]] ),
log(prop_cross_neg) + normal_lpdf( titre1[neg_index1_2[i],i] | mu_0[neg_index1_2[i]] + mu_cross_boost_neg[neg_index1_2[i]] + boost_unexpose[1],sigma[neg_index1_2[i]]) )
+ log_sum_exp( log1m(prop_cross_neg) + gamma_lpdf( titre1[neg_index1_3[i],i] |  (mu_0[neg_index1_3[i]] + boost_unexpose[1])*rate_0[neg_index1_3[i]] ,rate_0[neg_index1_3[i]] ),
log(prop_cross_neg) + normal_lpdf( titre1[neg_index1_3[i],i] | mu_0[neg_index1_3[i]] + mu_cross_boost_neg[neg_index1_3[i]] + boost_unexpose[1],sigma[neg_index1_3[i]]) );

}


for (i in 1:N2){
vector[3] lps;
lps[1] = log( (1-exp(-lambda[city_index2[i]][pos_index2_1[i],]*age_weight_other2[,i]*log1p_exp(20*(age2[i]-l[pos_index2_1[i]]))/20 ))*exp( -(lambda[city_index2[i]][pos_index2_2[i],]*age_weight2[,i] + lambda[city_index2[i]][neg_index2_1[i],]*age_weight2[,i]+lambda[city_index2[i]][neg_index2_2[i],]*age_weight2[,i])*log1p_exp(20*(age2[i]-l[pos_index2_1[i]]))/20 )
+ exp( - sum( lambda[city_index2[i]] * age_weight_other2[,i] )*log1p_exp(20*(age2[i]-l[pos_index2_1[i]]))/20 )*lambda[city_index2[i]][pos_index2_1[i],]*age_weight_primary2[,i]/(sum( lambda[city_index2[i]] * age_weight_primary2[,i]) )*( 1-exp( -sum( lambda[city_index2[i]] * age_weight_primary2[,i] )*( age2[i]-log1p_exp(20*(age2[i]-l[pos_index2_1[i]]))/20 ) ) ) )
+ log(beta_1[pos_index2_1[i],pos_index2_2[i]])
+ log1m(beta_1[pos_index2_1[i],neg_index2_1[i]])
+ log1m(beta_1[pos_index2_1[i],neg_index2_2[i]])
+ log_sum_exp( log1m(prop_other_exposure[city_index2[i]]) + normal_lpdf( titre2[pos_index2_1[i],i] | mu_0[pos_index2_1[i]] + mu_infec_boost[pos_index2_1[i]],sigma[pos_index2_1[i]] ),
log(prop_other_exposure[city_index2[i]]) + normal_lpdf( titre2[pos_index2_1[i],i] | mu_0[pos_index2_1[i]] + mu_infec_boost[pos_index2_1[i]] + add0[pos_index2_1[i]],sigma[pos_index2_1[i]] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index2[i]]) + normal_lpdf( titre2[pos_index2_2[i],i] | mu_0[pos_index2_2[i]] + mu_cross_boost[pos_index2_2[i]] + boost_unexpose[1],sigma[pos_index2_2[i]] ),
log(prop_other_exposure[city_index2[i]]) + normal_lpdf( titre2[pos_index2_2[i],i] | mu_0[pos_index2_2[i]] + mu_cross_boost[pos_index2_2[i]] + boost_unexpose[1] + add0[pos_index2_2[i]],sigma[pos_index2_2[i]] ))
+ log_sum_exp( log1m(prop_cross_neg) + gamma_lpdf( titre2[neg_index2_1[i],i] |  (mu_0[neg_index2_1[i]] + boost_unexpose[1])*rate_0[neg_index2_1[i]] ,rate_0[neg_index2_1[i]] ),
log(prop_cross_neg) + normal_lpdf(titre2[neg_index2_1[i],i] | mu_0[neg_index2_1[i]] + mu_cross_boost_neg[neg_index2_1[i]] + boost_unexpose[1],sigma[neg_index2_1[i]]))
+ log_sum_exp( log1m(prop_cross_neg) + gamma_lpdf( titre2[neg_index2_2[i],i] |  (mu_0[neg_index2_2[i]] + boost_unexpose[1])*rate_0[neg_index2_2[i]] ,rate_0[neg_index2_2[i]] ),
log(prop_cross_neg) + normal_lpdf(titre2[neg_index2_2[i],i] | mu_0[neg_index2_2[i]] + mu_cross_boost_neg[neg_index2_2[i]] + boost_unexpose[1],sigma[neg_index2_2[i]]));

lps[2] = log( (1-exp(-lambda[city_index2[i]][pos_index2_2[i],]*age_weight_other2[,i]*log1p_exp(20*(age2[i]-l[pos_index2_2[i]]))/20 ))*exp( -(lambda[city_index2[i]][pos_index2_1[i],]*age_weight2[,i] + lambda[city_index2[i]][neg_index2_1[i],]*age_weight2[,i]+lambda[city_index2[i]][neg_index2_2[i],]*age_weight2[,i])*log1p_exp(20*(age2[i]-l[pos_index2_2[i]]))/20 )
+ exp( - sum( lambda[city_index2[i]] * age_weight_other2[,i] )*log1p_exp(20*(age2[i]-l[pos_index2_2[i]]))/20 )*lambda[city_index2[i]][pos_index2_2[i],]*age_weight_primary2[,i]/(sum( lambda[city_index2[i]] * age_weight_primary2[,i]) )*( 1-exp( -sum( lambda[city_index2[i]] * age_weight_primary2[,i] )*( age2[i]-log1p_exp(20*(age2[i]-l[pos_index2_2[i]]))/20 ) ) ) )
+ log(beta_1[pos_index2_2[i],pos_index2_1[i]])
+ log1m(beta_1[pos_index2_2[i],neg_index2_1[i]])
+ log1m(beta_1[pos_index2_2[i],neg_index2_2[i]])
+ log_sum_exp( log1m(prop_other_exposure[city_index2[i]]) + normal_lpdf( titre2[pos_index2_2[i],i] | mu_0[pos_index2_2[i]] + mu_infec_boost[pos_index2_2[i]],sigma[pos_index2_2[i]] ),
log(prop_other_exposure[city_index2[i]]) + normal_lpdf( titre2[pos_index2_2[i],i] | mu_0[pos_index2_2[i]] + mu_infec_boost[pos_index2_2[i]] + add0[pos_index2_2[i]],sigma[pos_index2_2[i]] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index2[i]]) + normal_lpdf( titre2[pos_index2_1[i],i] | mu_0[pos_index2_1[i]] + mu_cross_boost[pos_index2_1[i]] + boost_unexpose[1],sigma[pos_index2_1[i]] ),
log(prop_other_exposure[city_index2[i]]) + normal_lpdf( titre2[pos_index2_1[i],i] | mu_0[pos_index2_1[i]] + mu_cross_boost[pos_index2_1[i]] + boost_unexpose[1] + add0[pos_index2_1[i]],sigma[pos_index2_1[i]] ) )
+ log_sum_exp( log1m(prop_cross_neg) + gamma_lpdf( titre2[neg_index2_1[i],i] |  (mu_0[neg_index2_1[i]] + boost_unexpose[1])*rate_0[neg_index2_1[i]] ,rate_0[neg_index2_1[i]] ),
log(prop_cross_neg) + normal_lpdf(titre2[neg_index2_1[i],i] | mu_0[neg_index2_1[i]] + mu_cross_boost_neg[neg_index2_1[i]] + boost_unexpose[1],sigma[neg_index2_1[i]]))
+ log_sum_exp( log1m(prop_cross_neg) + gamma_lpdf( titre2[neg_index2_2[i],i] |  (mu_0[neg_index2_2[i]] + boost_unexpose[1])*rate_0[neg_index2_2[i]] ,rate_0[neg_index2_2[i]] ),
log(prop_cross_neg) + normal_lpdf(titre2[neg_index2_2[i],i] | mu_0[neg_index2_2[i]] + mu_cross_boost_neg[neg_index2_2[i]] + boost_unexpose[1],sigma[neg_index2_2[i]]));

lps[3] = log1m_exp( -lambda[city_index2[i]][pos_index2_1[i],]*age_weight2[,i]*log1p_exp(20*(age2[i]-l[pos_index2_2[i]]))/20 )
+ log1m_exp( -lambda[city_index2[i]][pos_index2_2[i],]*age_weight2[,i]*log1p_exp(20*(age2[i]-l[pos_index2_1[i]]))/20 )
- (lambda[city_index2[i]][neg_index2_1[i],]*age_weight2[,i]+lambda[city_index2[i]][neg_index2_2[i],]*age_weight2[,i])*log1p_exp(20*(age2[i]-l[pos_index2_1[i]]-l[pos_index2_2[i]]))/20
+ log1m( beta_2[pos_index2_1[i],pos_index2_2[i],neg_index2_1[i]] )
+ log1m( beta_2[pos_index2_1[i],pos_index2_2[i],neg_index2_2[i]] )
+ gamma_lpdf( ymax - titre2[pos_index2_1[i],i] | (ymax - (mu_0[pos_index2_1[i]] + mu_infec_boost[pos_index2_1[i]] + boost_expose[1]))*rate_up[pos_index2_1[i]],rate_up[pos_index2_1[i]] )
+ gamma_lpdf( ymax - titre2[pos_index2_2[i],i] | (ymax - (mu_0[pos_index2_2[i]] + mu_infec_boost[pos_index2_2[i]] + boost_expose[1]))*rate_up[pos_index2_2[i]],rate_up[pos_index2_2[i]] )
+ normal_lpdf( titre2[neg_index2_1[i],i] | mu_0[neg_index2_1[i]] + boost_unexpose[1] + boost_unexpose[2],sigma[neg_index2_1[i]] )
+ normal_lpdf( titre2[neg_index2_2[i],i] | mu_0[neg_index2_2[i]] + boost_unexpose[1] + boost_unexpose[2],sigma[neg_index2_2[i]] );

target += log_sum_exp(lps);
}

for (i in 1:N3){
vector[7] lps;

lps[1] = log( (1-exp(-lambda[city_index3[i]][pos_index3_1[i],]*age_weight_other3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 ))*exp( -(lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i] + lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 )
+ exp( - sum( lambda[city_index3[i]] * age_weight_other3[,i] )*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 )*lambda[city_index3[i]][pos_index3_1[i],]*age_weight_primary3[,i]/(sum( lambda[city_index3[i]] * age_weight_primary3[,i]) )*( 1-exp( -sum( lambda[city_index3[i]] * age_weight_primary3[,i] )*( age3[i]-log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 ) ) ) )
+ log( beta_1[pos_index3_1[i],pos_index3_2[i]] )
+ log( beta_1[pos_index3_1[i],pos_index3_3[i]] ) 
+ log1m( beta_1[pos_index3_1[i],neg_index3_1[i]] )
+ log_sum_exp( log1m(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_1[i],i] | mu_0[pos_index3_1[i]] + mu_infec_boost[pos_index3_1[i]],sigma[pos_index3_1[i]] ),
log(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_1[i],i] | mu_0[pos_index3_1[i]] + mu_infec_boost[pos_index3_1[i]] + add0[pos_index3_1[i]],sigma[pos_index3_1[i]] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_2[i],i] | mu_0[pos_index3_2[i]] + mu_cross_boost[pos_index3_2[i]] + boost_unexpose[1],sigma[pos_index3_2[i]] ),
log(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_2[i],i] | mu_0[pos_index3_2[i]] + mu_cross_boost[pos_index3_2[i]] + boost_unexpose[1] + add0[pos_index3_2[i]],sigma[pos_index3_2[i]] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_3[i],i] | mu_0[pos_index3_3[i]] + mu_cross_boost[pos_index3_3[i]] + boost_unexpose[1],sigma[pos_index3_3[i]] ),
log(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_3[i],i] | mu_0[pos_index3_3[i]] + mu_cross_boost[pos_index3_3[i]] + boost_unexpose[1] + add0[pos_index3_3[i]],sigma[pos_index3_3[i]] ) )
+ log_sum_exp( log1m( prop_cross_neg ) + gamma_lpdf( titre3[neg_index3_1[i],i] |  (mu_0[neg_index3_1[i]] + boost_unexpose[1])*rate_0[neg_index3_1[i]] ,rate_0[neg_index3_1[i]] ),
log(prop_cross_neg) + normal_lpdf( titre3[neg_index3_1[i],i] | mu_0[neg_index3_1[i]] + mu_cross_boost_neg[neg_index3_1[i]] + boost_unexpose[1],sigma[neg_index3_1[i]] ));

lps[2] = log( (1-exp(-lambda[city_index3[i]][pos_index3_2[i],]*age_weight_other3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 ))*exp( -(lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i] + lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 )
+ exp( - sum( lambda[city_index3[i]] * age_weight_other3[,i] )*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 )*lambda[city_index3[i]][pos_index3_2[i],]*age_weight_primary3[,i]/(sum( lambda[city_index3[i]] * age_weight_primary3[,i]) )*( 1-exp( -sum( lambda[city_index3[i]] * age_weight_primary3[,i] )*( age3[i]-log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 ) ) ) )
+ log( beta_1[pos_index3_2[i],pos_index3_1[i]] )
+ log( beta_1[pos_index3_2[i],pos_index3_3[i]] )
+ log1m( beta_1[pos_index3_2[i],neg_index3_1[i]] )
+ log_sum_exp( log1m(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_2[i],i] | mu_0[pos_index3_2[i]] + mu_infec_boost[pos_index3_2[i]],sigma[pos_index3_2[i]] ),
log(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_2[i],i] | mu_0[pos_index3_2[i]] + mu_infec_boost[pos_index3_2[i]] + add0[pos_index3_2[i]],sigma[pos_index3_2[i]] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_1[i],i] | mu_0[pos_index3_1[i]] + mu_cross_boost[pos_index3_1[i]] + boost_unexpose[1],sigma[pos_index3_1[i]] ),
log(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_1[i],i] | mu_0[pos_index3_1[i]] + mu_cross_boost[pos_index3_1[i]] + boost_unexpose[1] + add0[pos_index3_1[i]],sigma[pos_index3_1[i]] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_3[i],i] | mu_0[pos_index3_3[i]] + mu_cross_boost[pos_index3_3[i]] + boost_unexpose[1],sigma[pos_index3_3[i]] ),
log(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_3[i],i] | mu_0[pos_index3_3[i]] + mu_cross_boost[pos_index3_3[i]] + boost_unexpose[1] + add0[pos_index3_3[i]],sigma[pos_index3_3[i]] ) )
+ log_sum_exp( log1m( prop_cross_neg ) + gamma_lpdf( titre3[neg_index3_1[i],i] |  (mu_0[neg_index3_1[i]] + boost_unexpose[1])*rate_0[neg_index3_1[i]] ,rate_0[neg_index3_1[i]] ),
log(prop_cross_neg) + normal_lpdf( titre3[neg_index3_1[i],i] | mu_0[neg_index3_1[i]] + mu_cross_boost_neg[neg_index3_1[i]] + boost_unexpose[1],sigma[neg_index3_1[i]] ));

lps[3] = log( (1-exp(-lambda[city_index3[i]][pos_index3_3[i],]*age_weight_other3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 ))*exp( -(lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i] + lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 )
+ exp( - sum( lambda[city_index3[i]] * age_weight_other3[,i] )*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 )*lambda[city_index3[i]][pos_index3_3[i],]*age_weight_primary3[,i]/(sum( lambda[city_index3[i]] * age_weight_primary3[,i]) )*( 1-exp( -sum( lambda[city_index3[i]] * age_weight_primary3[,i] )*( age3[i]-log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 ) ) ) )
+ log( beta_1[pos_index3_3[i],pos_index3_2[i]] )
+ log( beta_1[pos_index3_3[i],pos_index3_1[i]] )
+ log1m( beta_1[pos_index3_3[i],neg_index3_1[i]] )
+ log_sum_exp( log1m(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_3[i],i] | mu_0[pos_index3_3[i]] + mu_infec_boost[pos_index3_3[i]],sigma[pos_index3_3[i]] ),
log(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_3[i],i] | mu_0[pos_index3_3[i]] + mu_infec_boost[pos_index3_3[i]] + add0[pos_index3_3[i]],sigma[pos_index3_3[i]] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_2[i],i] | mu_0[pos_index3_2[i]] + mu_cross_boost[pos_index3_2[i]] + boost_unexpose[1],sigma[pos_index3_2[i]] ),
log(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_2[i],i] | mu_0[pos_index3_2[i]] + mu_cross_boost[pos_index3_2[i]] + boost_unexpose[1] + add0[pos_index3_2[i]],sigma[pos_index3_2[i]] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_1[i],i] | mu_0[pos_index3_1[i]] + mu_cross_boost[pos_index3_1[i]] + boost_unexpose[1],sigma[pos_index3_1[i]] ),
log(prop_other_exposure[city_index3[i]]) + normal_lpdf( titre3[pos_index3_1[i],i] | mu_0[pos_index3_1[i]] + mu_cross_boost[pos_index3_1[i]] + boost_unexpose[1] + add0[pos_index3_1[i]],sigma[pos_index3_1[i]] ) )
+ log_sum_exp( log1m( prop_cross_neg ) + gamma_lpdf( titre3[neg_index3_1[i],i] |  (mu_0[neg_index3_1[i]] + boost_unexpose[1])*rate_0[neg_index3_1[i]] ,rate_0[neg_index3_1[i]] ),
log(prop_cross_neg) + normal_lpdf( titre3[neg_index3_1[i],i] | mu_0[neg_index3_1[i]] + mu_cross_boost_neg[neg_index3_1[i]] + boost_unexpose[1],sigma[neg_index3_1[i]] ));

lps[4] = log1m_exp( -lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 )
+ log1m_exp( -lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 ) 
- (lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]-l[pos_index3_1[i]]))/20
+ log( beta_2[pos_index3_1[i],pos_index3_2[i],pos_index3_3[i]] )
+ log1m( beta_2[pos_index3_1[i],pos_index3_2[i],neg_index3_1[i]] )
+ gamma_lpdf( ymax - titre3[pos_index3_1[i],i] | (ymax - (mu_0[pos_index3_1[i]] + mu_infec_boost[pos_index3_1[i]] + boost_expose[1]))*rate_up[pos_index3_1[i]],rate_up[pos_index3_1[i]] )
+ gamma_lpdf( ymax - titre3[pos_index3_2[i],i] | (ymax - (mu_0[pos_index3_2[i]] + mu_infec_boost[pos_index3_2[i]] + boost_expose[1]))*rate_up[pos_index3_2[i]],rate_up[pos_index3_2[i]] )
+ normal_lpdf( titre3[pos_index3_3[i],i] | mu_0[pos_index3_3[i]] + mu_cross_boost[pos_index3_3[i]] + boost_unexpose[1] + boost_unexpose[2],sigma[pos_index3_3[i]] )
+ normal_lpdf( titre3[neg_index3_1[i],i] | mu_0[neg_index3_1[i]] + boost_unexpose[1] + boost_unexpose[2],sigma[neg_index3_1[i]] );

lps[5] = log1m_exp( -lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 ) 
+ log1m_exp( -lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 ) 
- (lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]-l[pos_index3_1[i]]))/20
+ log( beta_2[pos_index3_1[i],pos_index3_3[i],pos_index3_2[i]] )
+ log1m( beta_2[pos_index3_1[i],pos_index3_3[i],neg_index3_1[i]] )
+ gamma_lpdf( ymax - titre3[pos_index3_1[i],i] | (ymax - (mu_0[pos_index3_1[i]] + mu_infec_boost[pos_index3_1[i]] + boost_expose[1]))*rate_up[pos_index3_1[i]],rate_up[pos_index3_1[i]] )
+ gamma_lpdf( ymax - titre3[pos_index3_3[i],i] | (ymax - (mu_0[pos_index3_3[i]] + mu_infec_boost[pos_index3_3[i]] + boost_expose[1]))*rate_up[pos_index3_3[i]],rate_up[pos_index3_3[i]] )
+ normal_lpdf( titre3[pos_index3_2[i],i] | mu_0[pos_index3_2[i]] + mu_cross_boost[pos_index3_2[i]] + boost_unexpose[1] + boost_unexpose[2],sigma[pos_index3_2[i]] )
+ normal_lpdf( titre3[neg_index3_1[i],i] | mu_0[neg_index3_1[i]] + boost_unexpose[1] + boost_unexpose[2],sigma[neg_index3_1[i]] );

lps[6] = log1m_exp( -lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 ) 
+ log1m_exp( -lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 ) 
- (lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]-l[pos_index3_2[i]]))/20
+ log( beta_2[pos_index3_2[i],pos_index3_3[i],pos_index3_1[i]] )
+ log1m( beta_2[pos_index3_2[i],pos_index3_3[i],neg_index3_1[i]] )
+ gamma_lpdf( ymax - titre3[pos_index3_2[i],i] | (ymax - (mu_0[pos_index3_2[i]] + mu_infec_boost[pos_index3_2[i]] + boost_expose[1]))*rate_up[pos_index3_2[i]],rate_up[pos_index3_2[i]] )
+ gamma_lpdf( ymax - titre3[pos_index3_3[i],i] | (ymax - (mu_0[pos_index3_3[i]] + mu_infec_boost[pos_index3_3[i]] + boost_expose[1]))*rate_up[pos_index3_3[i]],rate_up[pos_index3_3[i]] )
+ normal_lpdf( titre3[pos_index3_1[i],i] | mu_0[pos_index3_1[i]] + mu_cross_boost[pos_index3_1[i]] + boost_unexpose[1] + boost_unexpose[2],sigma[pos_index3_1[i]] )
+ normal_lpdf( titre3[neg_index3_1[i],i] | mu_0[neg_index3_1[i]] + boost_unexpose[1] + boost_unexpose[2],sigma[neg_index3_1[i]] );


lps[7] = log1m_exp( -lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]-l[pos_index3_3[i]]))/20 )
+ log1m_exp( -lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]-l[pos_index3_3[i]]))/20 )
+ log1m_exp( -lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]-l[pos_index3_2[i]]))/20 )
- lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]-l[pos_index3_2[i]]-l[pos_index3_3[i]]))/20
+ log1m( beta_3[neg_index3_1[i]] )
+ gamma_lpdf( ymax - titre3[pos_index3_1[i],i] | (ymax - (mu_0[pos_index3_1[i]] + mu_infec_boost[pos_index3_1[i]] + boost_expose[1] + boost_expose[2]))*rate_up[pos_index3_1[i]],rate_up[pos_index3_1[i]] )
+ gamma_lpdf( ymax - titre3[pos_index3_2[i],i] | (ymax - (mu_0[pos_index3_2[i]] + mu_infec_boost[pos_index3_2[i]] + boost_expose[1] + boost_expose[2]))*rate_up[pos_index3_2[i]],rate_up[pos_index3_2[i]] )
+ gamma_lpdf( ymax - titre3[pos_index3_3[i],i] | (ymax - (mu_0[pos_index3_3[i]] + mu_infec_boost[pos_index3_3[i]] + boost_expose[1] + boost_expose[2]))*rate_up[pos_index3_3[i]],rate_up[pos_index3_3[i]] )
+ normal_lpdf( titre3[neg_index3_1[i],i] | mu_0[neg_index3_1[i]] + boost_unexpose[1] + boost_unexpose[2] + boost_unexpose[3],sigma[neg_index3_1[i]] );

target += log_sum_exp(lps);
}

for (i in 1:N4){
vector[15] lps;

lps[1] = log( beta_1[1,2] ) + log( beta_1[1,3] ) + log( beta_1[1,4] )
+ log( (1-exp(-lambda[city_index4[i]][1,]*age_weight_other4[,i]*log1p_exp(20*(age4[i]-l[1]))/20 ))*exp( -(lambda[city_index4[i]][2,]*age_weight4[,i] + lambda[city_index4[i]][3,]*age_weight4[,i]+lambda[city_index4[i]][4,]*age_weight4[,i])*log1p_exp(20*(age4[i]-l[1]))/20 )
+ exp( - sum( lambda[city_index4[i]] * age_weight_other4[,i] )*log1p_exp(20*(age4[i]-l[1]))/20 )*lambda[city_index4[i]][1,]*age_weight_primary4[,i]/(sum( lambda[city_index4[i]] * age_weight_primary4[,i]) )*( 1-exp( -sum( lambda[city_index4[i]] * age_weight_primary4[,i] )*( age4[i]-log1p_exp(20*(age4[i]-l[1]))/20 ) ) ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[1,i] | mu_0[1] + mu_infec_boost[1],sigma[1] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[1,i] | mu_0[1] + mu_infec_boost[1] + add0[1],sigma[1] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[2,i] | mu_0[2] + mu_cross_boost[2] + boost_unexpose[1],sigma[2] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[2,i] | mu_0[2] + mu_cross_boost[2] + boost_unexpose[1] + add0[2],sigma[2] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[3,i] | mu_0[3] + mu_cross_boost[3] + boost_unexpose[1],sigma[3] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[3,i] | mu_0[3] + mu_cross_boost[3] + boost_unexpose[1] + add0[3],sigma[3] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[4,i] | mu_0[4] + mu_cross_boost[4] + boost_unexpose[1],sigma[4] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[4,i] | mu_0[4] + mu_cross_boost[4] + boost_unexpose[1] + add0[4],sigma[4] ) );

lps[2] = log( beta_1[2,1] ) + log( beta_1[2,3] ) + log( beta_1[2,4] )
+ log( (1-exp(-lambda[city_index4[i]][2,]*age_weight_other4[,i]*log1p_exp(20*(age4[i]-l[2]))/20 ))*exp( -(lambda[city_index4[i]][1,]*age_weight4[,i] + lambda[city_index4[i]][3,]*age_weight4[,i]+lambda[city_index4[i]][4,]*age_weight4[,i])*log1p_exp(20*(age4[i]-l[2]))/20 )
+ exp( - sum( lambda[city_index4[i]] * age_weight_other4[,i] )*log1p_exp(20*(age4[i]-l[2]))/20 )*lambda[city_index4[i]][2,]*age_weight_primary4[,i]/(sum( lambda[city_index4[i]] * age_weight_primary4[,i]) )*( 1-exp( -sum( lambda[city_index4[i]] * age_weight_primary4[,i] )*( age4[i]-log1p_exp(20*(age4[i]-l[2]))/20 ) ) ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[2,i] | mu_0[2] + mu_infec_boost[2],sigma[2] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[2,i] | mu_0[2] + mu_infec_boost[2] + add0[2],sigma[2] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[1,i] | mu_0[1] + mu_cross_boost[1] + boost_unexpose[1],sigma[1] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[1,i] | mu_0[1] + mu_cross_boost[1] + boost_unexpose[1] + add0[1],sigma[1] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[3,i] | mu_0[3] + mu_cross_boost[3] + boost_unexpose[1],sigma[3] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[3,i] | mu_0[3] + mu_cross_boost[3] + boost_unexpose[1] + add0[3],sigma[3] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[4,i] | mu_0[4] + mu_cross_boost[4] + boost_unexpose[1],sigma[4] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[4,i] | mu_0[4] + mu_cross_boost[4] + boost_unexpose[1] + add0[4],sigma[4] ) );

lps[3] = log( beta_1[3,1] ) + log( beta_1[3,2] ) + log( beta_1[3,4] )
+ log( (1-exp(-lambda[city_index4[i]][3,]*age_weight_other4[,i]*log1p_exp(20*(age4[i]-l[3]))/20 ))*exp( -(lambda[city_index4[i]][1,]*age_weight4[,i] + lambda[city_index4[i]][2,]*age_weight4[,i]+lambda[city_index4[i]][4,]*age_weight4[,i])*log1p_exp(20*(age4[i]-l[3]))/20 )
+ exp( - sum( lambda[city_index4[i]] * age_weight_other4[,i] )*log1p_exp(20*(age4[i]-l[3]))/20 )*lambda[city_index4[i]][3,]*age_weight_primary4[,i]/(sum( lambda[city_index4[i]] * age_weight_primary4[,i]) )*( 1-exp( -sum( lambda[city_index4[i]] * age_weight_primary4[,i] )*( age4[i]-log1p_exp(20*(age4[i]-l[3]))/20 ) ) ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[3,i] | mu_0[3] + mu_infec_boost[3],sigma[3] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[3,i] | mu_0[3] + mu_infec_boost[3] + add0[3],sigma[3] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[2,i] | mu_0[2] + mu_cross_boost[2] + boost_unexpose[1],sigma[2] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[2,i] | mu_0[2] + mu_cross_boost[2] + boost_unexpose[1] + add0[2],sigma[2] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[1,i] | mu_0[1] + mu_cross_boost[1] + boost_unexpose[1],sigma[1] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[1,i] | mu_0[1] + mu_cross_boost[1] + boost_unexpose[1] + add0[1],sigma[1] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[4,i] | mu_0[4] + mu_cross_boost[4] + boost_unexpose[1],sigma[4] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[4,i] | mu_0[4] + mu_cross_boost[4] + boost_unexpose[1] + add0[4],sigma[4] ) );

lps[4] = log( beta_1[4,1] ) + log( beta_1[4,2] ) + log( beta_1[4,3] )
+ log( (1-exp(-lambda[city_index4[i]][4,]*age_weight_other4[,i]*log1p_exp(20*(age4[i]-l[4]))/20 ))*exp( -(lambda[city_index4[i]][1,]*age_weight4[,i] + lambda[city_index4[i]][2,]*age_weight4[,i]+lambda[city_index4[i]][3,]*age_weight4[,i])*log1p_exp(20*(age4[i]-l[4]))/20 )
+ exp( - sum( lambda[city_index4[i]] * age_weight_other4[,i] )*log1p_exp(20*(age4[i]-l[4]))/20 )*lambda[city_index4[i]][4,]*age_weight_primary4[,i]/(sum( lambda[city_index4[i]] * age_weight_primary4[,i]) )*( 1-exp( -sum( lambda[city_index4[i]] * age_weight_primary4[,i] )*( age4[i]-log1p_exp(20*(age4[i]-l[4]))/20 ) ) ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[4,i] | mu_0[4] + mu_infec_boost[4],sigma[4] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[4,i] | mu_0[4] + mu_infec_boost[4] + add0[4],sigma[4] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[2,i] | mu_0[2] + mu_cross_boost[2] + boost_unexpose[1],sigma[2] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[2,i] | mu_0[2] + mu_cross_boost[2] + boost_unexpose[1] + add0[2],sigma[2] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[3,i] | mu_0[3] + mu_cross_boost[3] + boost_unexpose[1],sigma[3] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[3,i] | mu_0[3] + mu_cross_boost[3] + boost_unexpose[1] + add0[3],sigma[3] ) )
+ log_sum_exp( log1m(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[1,i] | mu_0[1] + mu_cross_boost[1] + boost_unexpose[1],sigma[1] ),
log(prop_other_exposure[city_index4[i]]) + normal_lpdf( titre4[1,i] | mu_0[1] + mu_cross_boost[1] + boost_unexpose[1] + add0[1],sigma[1] ) );

lps[5] = log( beta_2[1,2,3] )
+ log( beta_2[1,2,4] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]))/20 )
- sum(lambda[city_index4[i]][3:4,]*age_weight4[,i])*log1p_exp(20*(age4[i]-l[1]-l[2]))/20
+ gamma_lpdf( ymax - titre4[1,i] | (ymax - (mu_0[1] + mu_infec_boost[1] + boost_expose[1]))*rate_up[1],rate_up[1] )
+ gamma_lpdf( ymax - titre4[2,i] | (ymax - (mu_0[2] + mu_infec_boost[2] + boost_expose[1]))*rate_up[2],rate_up[2] )
+ normal_lpdf( titre4[3,i] | mu_0[3] + mu_cross_boost[3] + boost_unexpose[1] + boost_unexpose[2],sigma[3] )
+ normal_lpdf( titre4[4,i] | mu_0[4] + mu_cross_boost[4] + boost_unexpose[1] + boost_unexpose[2],sigma[4] );

lps[6] = log( beta_2[1,3,2] )
+ log( beta_2[1,3,4] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]))/20 )
- (lambda[city_index4[i]][2,]+lambda[city_index4[i]][4,])*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[3]))/20
+ gamma_lpdf( ymax - titre4[1,i] | (ymax - (mu_0[1] + mu_infec_boost[1] + boost_expose[1]))*rate_up[1],rate_up[1] )
+ gamma_lpdf( ymax - titre4[3,i] | (ymax - (mu_0[3] + mu_infec_boost[3] + boost_expose[1]))*rate_up[3],rate_up[3] )
+ normal_lpdf( titre4[2,i] | mu_0[2] + mu_cross_boost[2] + boost_unexpose[1] + boost_unexpose[2],sigma[2] )
+ normal_lpdf( titre4[4,i] | mu_0[4] + mu_cross_boost[4] + boost_unexpose[1] + boost_unexpose[2],sigma[4] );

lps[7] = log( beta_2[1,4,2] )
+ log( beta_2[1,4,3] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[4]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]))/20 )
- (lambda[city_index4[i]][2,]+lambda[city_index4[i]][3,])*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[4]))/20
+ gamma_lpdf( ymax - titre4[1,i] | (ymax - (mu_0[1] + mu_infec_boost[1] + boost_expose[1]))*rate_up[1],rate_up[1] )
+ gamma_lpdf( ymax - titre4[4,i] | (ymax - (mu_0[4] + mu_infec_boost[4] + boost_expose[1]))*rate_up[4],rate_up[4] )
+ normal_lpdf( titre4[2,i] | mu_0[2] + mu_cross_boost[2] + boost_unexpose[1] + boost_unexpose[2],sigma[2] )
+ normal_lpdf( titre4[3,i] | mu_0[3] + mu_cross_boost[3] + boost_unexpose[1] + boost_unexpose[2],sigma[3] );

lps[8] = log( beta_2[2,3,1] )
+ log( beta_2[2,3,4] )
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]))/20 )  
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]))/20 )
- (lambda[city_index4[i]][1,]+lambda[city_index4[i]][4,])*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[3]))/20
+ gamma_lpdf( ymax - titre4[2,i] | (ymax - (mu_0[2] + mu_infec_boost[2] + boost_expose[1]))*rate_up[2],rate_up[2] )
+ gamma_lpdf( ymax - titre4[3,i] | (ymax - (mu_0[3] + mu_infec_boost[3] + boost_expose[1]))*rate_up[3],rate_up[3] )
+ normal_lpdf( titre4[1,i] | mu_0[1] + mu_cross_boost[1] + boost_unexpose[1] + boost_unexpose[2],sigma[1] )
+ normal_lpdf( titre4[4,i] | mu_0[4] + mu_cross_boost[4] + boost_unexpose[1] + boost_unexpose[2],sigma[4] );

lps[9] = log( beta_2[2,4,1] )
+ log( beta_2[2,4,3] )
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[4]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]))/20 )
- (lambda[city_index4[i]][1,]+lambda[city_index4[i]][3,])*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[4]))/20
+ gamma_lpdf( ymax - titre4[2,i] | (ymax - (mu_0[2] + mu_infec_boost[2] + boost_expose[1]))*rate_up[2],rate_up[2] )
+ gamma_lpdf( ymax - titre4[4,i] | (ymax - (mu_0[4] + mu_infec_boost[4] + boost_expose[1]))*rate_up[4],rate_up[4] )
+ normal_lpdf( titre4[1,i] | mu_0[1] + mu_cross_boost[1] + boost_unexpose[1] + boost_unexpose[2],sigma[1] )
+ normal_lpdf( titre4[3,i] | mu_0[3] + mu_cross_boost[3] + boost_unexpose[1] + boost_unexpose[2],sigma[3] );

lps[10] = log( beta_2[3,4,1] )
+ log( beta_2[3,4,2] )
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[4]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]))/20 )
- (lambda[city_index4[i]][1,]+lambda[city_index4[i]][2,])*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]-l[4]))/20
+ gamma_lpdf( ymax - titre4[3,i] | (ymax - (mu_0[3] + mu_infec_boost[3] + boost_expose[1]))*rate_up[3],rate_up[3] )
+ gamma_lpdf( ymax - titre4[4,i] | (ymax - (mu_0[4] + mu_infec_boost[4] + boost_expose[1]))*rate_up[4],rate_up[4] )
+ normal_lpdf( titre4[1,i] | mu_0[1] + mu_cross_boost[1] + boost_unexpose[1] + boost_unexpose[2],sigma[1] )
+ normal_lpdf( titre4[2,i] | mu_0[2] + mu_cross_boost[2] + boost_unexpose[1] + boost_unexpose[2],sigma[2] );

lps[11] = log( beta_3[4] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[3]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[3]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]))/20 )
- lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]-l[3]))/20
+ gamma_lpdf( ymax - titre4[1,i] | (ymax - (mu_0[1] + mu_infec_boost[1] + boost_expose[1] + boost_expose[2]))*rate_up[1],rate_up[1] )
+ gamma_lpdf( ymax - titre4[2,i] | (ymax - (mu_0[2] + mu_infec_boost[2] + boost_expose[1] + boost_expose[2]))*rate_up[2],rate_up[2] )
+ gamma_lpdf( ymax - titre4[3,i] | (ymax - (mu_0[3] + mu_infec_boost[3] + boost_expose[1] + boost_expose[2]))*rate_up[3],rate_up[3] )
+ gamma_lpdf( ymax - titre4[4,i] | (ymax - (mu_0[4] + mu_cross_boost[4] + boost_unexpose[1] + boost_unexpose[2] + boost_unexpose[3]))*rate_up[4],rate_up[4] );

lps[12] = log( beta_3[3] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[4]))/20 )  
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[4]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]))/20 )
- lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]-l[4]))/20
+ gamma_lpdf( ymax - titre4[1,i] | (ymax - (mu_0[1] + mu_infec_boost[1] + boost_expose[1] + boost_expose[2]))*rate_up[1],rate_up[1] )
+ gamma_lpdf( ymax - titre4[2,i] | (ymax - (mu_0[2] + mu_infec_boost[2] + boost_expose[1] + boost_expose[2]))*rate_up[2],rate_up[2] )
+ gamma_lpdf( ymax - titre4[4,i] | (ymax - (mu_0[4] + mu_infec_boost[4] + boost_expose[1] + boost_expose[2]))*rate_up[4],rate_up[4] )
+ gamma_lpdf( ymax - titre4[3,i] | (ymax - (mu_0[3] + mu_cross_boost[3] + boost_unexpose[1] + boost_unexpose[2] + boost_unexpose[3]))*rate_up[3],rate_up[3] );

lps[13] = log( beta_3[2] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]-l[4]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[4]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[3]))/20 )
- lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[3]-l[4]))/20
+ gamma_lpdf( ymax - titre4[1,i] | (ymax - (mu_0[1] + mu_infec_boost[1] + boost_expose[1] + boost_expose[2]))*rate_up[1],rate_up[1] )
+ gamma_lpdf( ymax - titre4[3,i] | (ymax - (mu_0[3] + mu_infec_boost[3] + boost_expose[1] + boost_expose[2]))*rate_up[3],rate_up[3] )
+ gamma_lpdf( ymax - titre4[4,i] | (ymax - (mu_0[4] + mu_infec_boost[4] + boost_expose[1] + boost_expose[2]))*rate_up[4],rate_up[4] )
+ gamma_lpdf( ymax - titre4[2,i] | (ymax - (mu_0[2] + mu_cross_boost[2] + boost_unexpose[1] + boost_unexpose[2] + boost_unexpose[3]))*rate_up[2],rate_up[2] );

lps[14] = log( beta_3[1] )
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]-l[4]))/20 )  
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[4]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[3]))/20 )
- lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[3]-l[4]))/20
+ gamma_lpdf( ymax - titre4[2,i] | (ymax - (mu_0[2] + mu_infec_boost[2] + boost_expose[1] + boost_expose[2]))*rate_up[2],rate_up[2] )
+ gamma_lpdf( ymax - titre4[3,i] | (ymax - (mu_0[3] + mu_infec_boost[3] + boost_expose[1] + boost_expose[2]))*rate_up[3],rate_up[3] )
+ gamma_lpdf( ymax - titre4[4,i] | (ymax - (mu_0[4] + mu_infec_boost[4] + boost_expose[1] + boost_expose[2]))*rate_up[4],rate_up[4] )
+ gamma_lpdf( ymax - titre4[1,i] | (ymax - (mu_0[1] + mu_cross_boost[1] + boost_unexpose[1] + boost_unexpose[2] + boost_unexpose[3]))*rate_up[1],rate_up[1] );

lps[15] = log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[3]-l[4]))/20 ) +
log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[3]-l[4]))/20 ) +
log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]-l[4]))/20 ) +
log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]-l[3]))/20 )
+ gamma_lpdf( ymax - titre4[1,i] | (ymax - (mu_0[1] + mu_infec_boost[1] + boost_expose[1] + boost_expose[2]))*rate_up[1],rate_up[1] )
+ gamma_lpdf( ymax - titre4[2,i] | (ymax - (mu_0[2] + mu_infec_boost[2] + boost_expose[1] + boost_expose[2]))*rate_up[2],rate_up[2] )
+ gamma_lpdf( ymax - titre4[3,i] | (ymax - (mu_0[3] + mu_infec_boost[3] + boost_expose[1] + boost_expose[2]))*rate_up[3],rate_up[3] )
+ gamma_lpdf( ymax - titre4[4,i] | (ymax - (mu_0[4] + mu_infec_boost[4] + boost_expose[1] + boost_expose[2]))*rate_up[4],rate_up[4] );

target += log_sum_exp(lps);
}


1/lambda_mean1 ~ exponential(1);
1/lambda_mean2 ~ exponential(1);
l ~ normal(1.8,0.15);

shape_0 ~ exponential(1);
rate_0 ~ exponential(1);

mu_infec_boost ~ exponential(1);
sigma ~ normal(0,1);

mu_cross_boost ~ exponential(1);
mu_cross_boost_neg ~ exponential(1);

boost_expose ~ exponential(1);
boost_unexpose ~ exponential(1);

add0 ~ exponential(1);
rate_up ~ exponential(1);
}
"


titremodel_compiled <- rstan::stan_model(
  model_name = 'titremodel',
  model_code = gsub('\t','',titremodel_txt)
)

stan_data <- readRDS("stan_data_TitreModel.rds")

model_fit <- rstan::sampling(titremodel_compiled,
                             data=stan_data,
                             chains=8,
                             cores=8,
                             init=list(list(l=rep(1.8,4),lambda_mean1=20,lambda_mean2=20,shape_0=rep(0.3,4),rate_0=rep(4,4),mu_infec_boost=rep(1,4),boost_expose=rep(0.4,2),mu_cross_boost_neg=rep(0.1,4),mu_cross_boost=rep(0.9,4),boost_unexpose=rep(0.24,3)),
                                       list(l=rep(1.8,4),lambda_mean1=25,lambda_mean2=25,shape_0=rep(0.35,4),rate_0=rep(5,4),mu_infec_boost=rep(1.05,4),boost_expose=rep(0.38,2),mu_cross_boost_neg=rep(0.125,4),mu_cross_boost=rep(0.925,4),boost_unexpose=rep(0.22,3)),
                                       list(l=rep(1.8,4),lambda_mean1=30,lambda_mean2=30,shape_0=rep(0.4,4),rate_0=rep(6,4),mu_infec_boost=rep(1.1,4),boost_expose=rep(0.36,2),mu_cross_boost_neg=rep(0.15,4),mu_cross_boost=rep(0.95,4),boost_unexpose=rep(0.2,3)),
                                       list(l=rep(1.8,4),lambda_mean1=35,lambda_mean2=35,shape_0=rep(0.45,4),rate_0=rep(7,4),mu_infec_boost=rep(1.15,4),boost_expose=rep(0.34,2),mu_cross_boost_neg=rep(0.175,4),mu_cross_boost=rep(0.975,4),boost_unexpose=rep(0.18,3)),
                                       list(l=rep(1.8,4),lambda_mean1=40,lambda_mean2=40,shape_0=rep(0.5,4),rate_0=rep(8,4),mu_infec_boost=rep(1.2,4),boost_expose=rep(0.32,2),mu_cross_boost_neg=rep(0.2,4),mu_cross_boost=rep(1,4),boost_unexpose=rep(0.16,3)),
                                       list(l=rep(1.8,4),lambda_mean1=45,lambda_mean2=45,shape_0=rep(0.55,4),rate_0=rep(9,4),mu_infec_boost=rep(1.25,4),boost_expose=rep(0.3,2),mu_cross_boost_neg=rep(0.225,4),mu_cross_boost=rep(1.025,4),boost_unexpose=rep(0.1,3)),
                                       list(l=rep(1.8,4),lambda_mean1=50,lambda_mean2=50,shape_0=rep(0.6,4),rate_0=rep(10,4),mu_infec_boost=rep(1.3,4),boost_expose=rep(0.28,2),mu_cross_boost_neg=rep(0.25,4),mu_cross_boost=rep(1.05,4),boost_unexpose=rep(0.08,3)),
                                       list(l=rep(1.8,4),lambda_mean1=55,lambda_mean2=55,shape_0=rep(0.65,4),rate_0=rep(11,4),mu_infec_boost=rep(1.35,4),boost_expose=rep(0.26,2),mu_cross_boost_neg=rep(0.275,4),mu_cross_boost=rep(1.075,4),boost_unexpose=rep(0.06,3))),
                             warmup=5e2,
                             iter=4e3)

