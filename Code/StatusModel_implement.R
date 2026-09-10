
library(rstan)

statusmodel_txt <- "
data{
int<lower=1> K_virus;
int<lower=1> K_city;
int<lower=1> T_num;

int<lower=1> N0;
int<lower=1> age0[N0];
int<lower=1> count0[N0];
matrix<lower=0,upper=1>[T_num,N0] age_weight0;
int<lower=1,upper=K_city> city_index0[N0];

int<lower=1> N1;
int<lower=1> age1[N1];
int<lower=1> count1[N1];
int<lower=1,upper=K_virus> pos_index1_1[N1];
int<lower=1,upper=K_virus> neg_index1_1[N1];
int<lower=1,upper=K_virus> neg_index1_2[N1];
int<lower=1,upper=K_virus> neg_index1_3[N1];
matrix<lower=0,upper=1>[T_num,N1] age_weight1;
matrix<lower=0,upper=1>[T_num,N1] age_weight_primary1;
matrix<lower=0,upper=1>[T_num,N1] age_weight_other1;
int<lower=1,upper=K_city> city_index1[N1];


int<lower=1> N2;
int<lower=1> age2[N2];
int<lower=1> count2[N2];
int<lower=1,upper=K_virus> pos_index2_1[N2];
int<lower=1,upper=K_virus> pos_index2_2[N2];
int<lower=1,upper=K_virus> neg_index2_1[N2];
int<lower=1,upper=K_virus> neg_index2_2[N2];
matrix<lower=0,upper=1>[T_num,N2] age_weight2;
matrix<lower=0,upper=1>[T_num,N2] age_weight_primary2;
matrix<lower=0,upper=1>[T_num,N2] age_weight_other2;
int<lower=1,upper=K_city> city_index2[N2];

int<lower=1> N3;
int<lower=1> age3[N3];
int<lower=1> count3[N3];
int<lower=1,upper=K_virus> pos_index3_1[N3];
int<lower=1,upper=K_virus> pos_index3_2[N3];
int<lower=1,upper=K_virus> pos_index3_3[N3];
int<lower=1,upper=K_virus> neg_index3_1[N3];
matrix<lower=0,upper=1>[T_num,N3] age_weight3;
matrix<lower=0,upper=1>[T_num,N3] age_weight_primary3;
matrix<lower=0,upper=1>[T_num,N3] age_weight_other3;
int<lower=1,upper=K_city> city_index3[N3];

int<lower=1> N4;
int<lower=1> age4[N4];
int<lower=1> count4[N4];
matrix<lower=0,upper=1>[T_num,N4] age_weight4;
matrix<lower=0,upper=1>[T_num,N4] age_weight_primary4;
matrix<lower=0,upper=1>[T_num,N4] age_weight_other4;
int<lower=1,upper=K_city> city_index4[N4];

real b1;
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


}
transformed parameters{
matrix<lower=0>[K_virus,T_num] lambda[2]; 
matrix<lower=0.01,upper=0.99>[K_virus,K_virus] beta_1;
real beta_2[K_virus,K_virus,K_virus];
vector[K_virus] beta_3;

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
target += count0[i]*( -sum( lambda[city_index0[i]] * age_weight0[,i] )*age0[i] );
}

for (i in 1:N1){
target += count1[i]*(log1m(beta_1[pos_index1_1[i],neg_index1_1[i]]) + log1m(beta_1[pos_index1_1[i],neg_index1_2[i]]) + log1m(beta_1[pos_index1_1[i],neg_index1_3[i]])
+ log( (1-exp(-lambda[city_index1[i]][pos_index1_1[i],]*age_weight_other1[,i]*log1p_exp(20*(age1[i]-l[pos_index1_1[i]]))/20 ))*exp( -(lambda[city_index1[i]][neg_index1_1[i],]*age_weight1[,i] + lambda[city_index1[i]][neg_index1_2[i],]*age_weight1[,i]+lambda[city_index1[i]][neg_index1_3[i],]*age_weight1[,i])*log1p_exp(20*(age1[i]-l[pos_index1_1[i]]))/20 )
+ exp( - sum( lambda[city_index1[i]] * age_weight_other1[,i] )*log1p_exp(20*(age1[i]-l[pos_index1_1[i]]))/20 )*lambda[city_index1[i]][pos_index1_1[i],]*age_weight_primary1[,i]/(sum( lambda[city_index1[i]] * age_weight_primary1[,i]) )*( 1-exp( -sum( lambda[city_index1[i]] * age_weight_primary1[,i] )*( age1[i]-log1p_exp(20*(age1[i]-l[pos_index1_1[i]]))/20 ) ) ) ));

}


for (i in 1:N2){
vector[3] lps;
lps[1] = log(beta_1[pos_index2_1[i],pos_index2_2[i]])+log1m(beta_1[pos_index2_1[i],neg_index2_1[i]])+log1m(beta_1[pos_index2_1[i],neg_index2_2[i]])
+ log( (1-exp(-lambda[city_index2[i]][pos_index2_1[i],]*age_weight_other2[,i]*log1p_exp(20*(age2[i]-l[pos_index2_1[i]]))/20 ))*exp( -(lambda[city_index2[i]][pos_index2_2[i],]*age_weight2[,i] + lambda[city_index2[i]][neg_index2_1[i],]*age_weight2[,i]+lambda[city_index2[i]][neg_index2_2[i],]*age_weight2[,i])*log1p_exp(20*(age2[i]-l[pos_index2_1[i]]))/20 )
+ exp( - sum( lambda[city_index2[i]] * age_weight_other2[,i] )*log1p_exp(20*(age2[i]-l[pos_index2_1[i]]))/20 )*lambda[city_index2[i]][pos_index2_1[i],]*age_weight_primary2[,i]/(sum( lambda[city_index2[i]] * age_weight_primary2[,i]) )*( 1-exp( -sum( lambda[city_index2[i]] * age_weight_primary2[,i] )*( age2[i]-log1p_exp(20*(age2[i]-l[pos_index2_1[i]]))/20 ) ) ) );

lps[2] = log(beta_1[pos_index2_2[i],pos_index2_1[i]])+log1m(beta_1[pos_index2_2[i],neg_index2_1[i]])+log1m(beta_1[pos_index2_2[i],neg_index2_2[i]])
+ log( (1-exp(-lambda[city_index2[i]][pos_index2_2[i],]*age_weight_other2[,i]*log1p_exp(20*(age2[i]-l[pos_index2_2[i]]))/20 ))*exp( -(lambda[city_index2[i]][pos_index2_1[i],]*age_weight2[,i] + lambda[city_index2[i]][neg_index2_1[i],]*age_weight2[,i]+lambda[city_index2[i]][neg_index2_2[i],]*age_weight2[,i])*log1p_exp(20*(age2[i]-l[pos_index2_2[i]]))/20 )
+ exp( - sum( lambda[city_index2[i]] * age_weight_other2[,i] )*log1p_exp(20*(age2[i]-l[pos_index2_2[i]]))/20 )*lambda[city_index2[i]][pos_index2_2[i],]*age_weight_primary2[,i]/(sum( lambda[city_index2[i]] * age_weight_primary2[,i]) )*( 1-exp( -sum( lambda[city_index2[i]] * age_weight_primary2[,i] )*( age2[i]-log1p_exp(20*(age2[i]-l[pos_index2_2[i]]))/20 ) ) ) );

lps[3] = log1m( beta_2[ pos_index2_1[i],pos_index2_2[i],neg_index2_1[i] ] )
+ log1m( beta_2[ pos_index2_1[i],pos_index2_2[i],neg_index2_2[i] ] )
+ log1m_exp( -lambda[city_index2[i]][pos_index2_1[i],]*age_weight2[,i]*log1p_exp(20*(age2[i]-l[pos_index2_2[i]]))/20 )
+ log1m_exp( -lambda[city_index2[i]][pos_index2_2[i],]*age_weight2[,i]*log1p_exp(20*(age2[i]-l[pos_index2_1[i]]))/20 )
- (lambda[city_index2[i]][neg_index2_1[i],]*age_weight2[,i]+lambda[city_index2[i]][neg_index2_2[i],]*age_weight2[,i])*log1p_exp(20*(age2[i]-l[pos_index2_1[i]]-l[pos_index2_2[i]]))/20;

target += count2[i]*log_sum_exp(lps);
}

for (i in 1:N3){
vector[7] lps;

lps[1] = log( beta_1[pos_index3_1[i],pos_index3_2[i]] ) + log( beta_1[pos_index3_1[i],pos_index3_3[i]] ) + log1m( beta_1[pos_index3_1[i],neg_index3_1[i]] )
+ log( (1-exp(-lambda[city_index3[i]][pos_index3_1[i],]*age_weight_other3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 ))*exp( -(lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i] + lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 )
+ exp( - sum( lambda[city_index3[i]] * age_weight_other3[,i] )*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 )*lambda[city_index3[i]][pos_index3_1[i],]*age_weight_primary3[,i]/(sum( lambda[city_index3[i]] * age_weight_primary3[,i]) )*( 1-exp( -sum( lambda[city_index3[i]] * age_weight_primary3[,i] )*( age3[i]-log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 ) ) ) );

lps[2] = log( beta_1[pos_index3_2[i],pos_index3_1[i]] ) + log( beta_1[pos_index3_2[i],pos_index3_3[i]] ) + log1m( beta_1[pos_index3_2[i],neg_index3_1[i]] )
+ log( (1-exp(-lambda[city_index3[i]][pos_index3_2[i],]*age_weight_other3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 ))*exp( -(lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i] + lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 )
+ exp( - sum( lambda[city_index3[i]] * age_weight_other3[,i] )*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 )*lambda[city_index3[i]][pos_index3_2[i],]*age_weight_primary3[,i]/(sum( lambda[city_index3[i]] * age_weight_primary3[,i]) )*( 1-exp( -sum( lambda[city_index3[i]] * age_weight_primary3[,i] )*( age3[i]-log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 ) ) ) );

lps[3] = log( beta_1[pos_index3_3[i],pos_index3_1[i]] ) + log( beta_1[pos_index3_3[i],pos_index3_2[i]] ) + log1m( beta_1[pos_index3_3[i],neg_index3_1[i]] )
+ log( (1-exp(-lambda[city_index3[i]][pos_index3_3[i],]*age_weight_other3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 ))*exp( -(lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i] + lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 )
+ exp( - sum( lambda[city_index3[i]] * age_weight_other3[,i] )*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 )*lambda[city_index3[i]][pos_index3_3[i],]*age_weight_primary3[,i]/(sum( lambda[city_index3[i]] * age_weight_primary3[,i]) )*( 1-exp( -sum( lambda[city_index3[i]] * age_weight_primary3[,i] )*( age3[i]-log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 ) ) ) );

lps[4] = log( beta_2[pos_index3_1[i],pos_index3_2[i],pos_index3_3[i]] )
+ log1m( beta_2[pos_index3_1[i],pos_index3_2[i],neg_index3_1[i]] )
+ log1m_exp( -lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 )
+ log1m_exp( -lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 )
- (lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]-l[pos_index3_1[i]]))/20;

lps[5] = log( beta_2[pos_index3_1[i],pos_index3_3[i],pos_index3_2[i]] )
+ log1m( beta_2[pos_index3_1[i],pos_index3_3[i],neg_index3_1[i]] )
+ log1m_exp( -lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 )
+ log1m_exp( -lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]))/20 ) 
- (lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]-l[pos_index3_1[i]]))/20;

lps[6] = log( beta_2[pos_index3_2[i],pos_index3_3[i],pos_index3_1[i]] )
+ log1m( beta_2[pos_index3_2[i],pos_index3_3[i],neg_index3_1[i]] )
+ log1m_exp( -lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]))/20 )
+ log1m_exp( -lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]))/20 ) 
- (lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i]+lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i])*log1p_exp(20*(age3[i]-l[pos_index3_3[i]]-l[pos_index3_2[i]]))/20;

lps[7] = log1m( beta_3[neg_index3_1[i]] )
+ log1m_exp( -lambda[city_index3[i]][pos_index3_1[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_2[i]]-l[pos_index3_3[i]]))/20 ) +
+ log1m_exp( -lambda[city_index3[i]][pos_index3_2[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]-l[pos_index3_3[i]]))/20 ) +
+ log1m_exp( -lambda[city_index3[i]][pos_index3_3[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]-l[pos_index3_2[i]]))/20 ) +
- lambda[city_index3[i]][neg_index3_1[i],]*age_weight3[,i]*log1p_exp(20*(age3[i]-l[pos_index3_1[i]]-l[pos_index3_2[i]]-l[pos_index3_3[i]]))/20;

target += count3[i]*log_sum_exp(lps);
}

for (i in 1:N4){
vector[15] lps;

lps[1] = log( beta_1[1,2] ) + log( beta_1[1,3] ) + log( beta_1[1,4] )
+ log( (1-exp(-lambda[city_index4[i]][1,]*age_weight_other4[,i]*log1p_exp(20*(age4[i]-l[1]))/20 ))*exp( -(lambda[city_index4[i]][2,]*age_weight4[,i] + lambda[city_index4[i]][3,]*age_weight4[,i]+lambda[city_index4[i]][4,]*age_weight4[,i])*log1p_exp(20*(age4[i]-l[1]))/20 )
+ exp( - sum( lambda[city_index4[i]] * age_weight_other4[,i] )*log1p_exp(20*(age4[i]-l[1]))/20 )*lambda[city_index4[i]][1,]*age_weight_primary4[,i]/(sum( lambda[city_index4[i]] * age_weight_primary4[,i]) )*( 1-exp( -sum( lambda[city_index4[i]] * age_weight_primary4[,i] )*( age4[i]-log1p_exp(20*(age4[i]-l[1]))/20 ) ) ) );

lps[2] = log( beta_1[2,1] ) + log( beta_1[2,3] ) + log( beta_1[2,4] )
+ log( (1-exp(-lambda[city_index4[i]][2,]*age_weight_other4[,i]*log1p_exp(20*(age4[i]-l[2]))/20 ))*exp( -(lambda[city_index4[i]][1,]*age_weight4[,i] + lambda[city_index4[i]][3,]*age_weight4[,i]+lambda[city_index4[i]][4,]*age_weight4[,i])*log1p_exp(20*(age4[i]-l[2]))/20 )
+ exp( - sum( lambda[city_index4[i]] * age_weight_other4[,i] )*log1p_exp(20*(age4[i]-l[2]))/20 )*lambda[city_index4[i]][2,]*age_weight_primary4[,i]/(sum( lambda[city_index4[i]] * age_weight_primary4[,i]) )*( 1-exp( -sum( lambda[city_index4[i]] * age_weight_primary4[,i] )*( age4[i]-log1p_exp(20*(age4[i]-l[2]))/20 ) ) ) );

lps[3] = log( beta_1[3,1] ) + log( beta_1[3,2] ) + log( beta_1[3,4] )
+ log( (1-exp(-lambda[city_index4[i]][3,]*age_weight_other4[,i]*log1p_exp(20*(age4[i]-l[3]))/20 ))*exp( -(lambda[city_index4[i]][1,]*age_weight4[,i] + lambda[city_index4[i]][2,]*age_weight4[,i]+lambda[city_index4[i]][4,]*age_weight4[,i])*log1p_exp(20*(age4[i]-l[3]))/20 )
+ exp( - sum( lambda[city_index4[i]] * age_weight_other4[,i] )*log1p_exp(20*(age4[i]-l[3]))/20 )*lambda[city_index4[i]][3,]*age_weight_primary4[,i]/(sum( lambda[city_index4[i]] * age_weight_primary4[,i]) )*( 1-exp( -sum( lambda[city_index4[i]] * age_weight_primary4[,i] )*( age4[i]-log1p_exp(20*(age4[i]-l[3]))/20 ) ) ) );

lps[4] = log( beta_1[4,1] ) + log( beta_1[4,2] ) + log( beta_1[4,3] )
+ log( (1-exp(-lambda[city_index4[i]][4,]*age_weight_other4[,i]*log1p_exp(20*(age4[i]-l[4]))/20 ))*exp( -(lambda[city_index4[i]][1,]*age_weight4[,i] + lambda[city_index4[i]][2,]*age_weight4[,i]+lambda[city_index4[i]][3,]*age_weight4[,i])*log1p_exp(20*(age4[i]-l[4]))/20 )
+ exp( - sum( lambda[city_index4[i]] * age_weight_other4[,i] )*log1p_exp(20*(age4[i]-l[4]))/20 )*lambda[city_index4[i]][4,]*age_weight_primary4[,i]/(sum( lambda[city_index4[i]] * age_weight_primary4[,i]) )*( 1-exp( -sum( lambda[city_index4[i]] * age_weight_primary4[,i] )*( age4[i]-log1p_exp(20*(age4[i]-l[4]))/20 ) ) ) );

lps[5] = log( beta_2[1,2,3] ) + log( beta_2[1,2,4] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]))/20 )
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]))/20 )
- sum(lambda[city_index4[i]][3:4,]*age_weight4[,i])*log1p_exp(20*(age4[i]-l[1]-l[2]))/20;

lps[6] = log( beta_2[1,3,2] ) + log( beta_2[1,3,4] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]))/20 )
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]))/20 )
- (lambda[city_index4[i]][2,]+lambda[city_index4[i]][4,])*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[3]))/20;

lps[7] = log( beta_2[1,4,2] ) + log( beta_2[1,4,3] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[4]))/20 )
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]))/20 )
- (lambda[city_index4[i]][2,]+lambda[city_index4[i]][3,])*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[4]))/20;

lps[8] = log( beta_2[2,3,1] ) + log( beta_2[2,3,4] )
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]))/20 )
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]))/20 )
- (lambda[city_index4[i]][1,]+lambda[city_index4[i]][4,])*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[3]))/20;

lps[9] = log( beta_2[2,4,1] ) + log( beta_2[2,4,3] )
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[4]))/20 )
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]))/20 )
- (lambda[city_index4[i]][1,]+lambda[city_index4[i]][3,])*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[4]))/20;

lps[10] = log( beta_2[3,4,1] ) + log( beta_2[3,4,2] )
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[4]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]))/20 )
- (lambda[city_index4[i]][1,]+lambda[city_index4[i]][2,])*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]-l[4]))/20;

lps[11] = log( beta_3[4] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[3]))/20 )
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[3]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]))/20 )
- lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]-l[3]))/20;

lps[12] = log( beta_3[3] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[4]))/20 ) 
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[4]))/20 )
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]))/20 )
- lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]-l[4]))/20;

lps[13] = log( beta_3[2] )
+ log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]-l[4]))/20 )
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[4]))/20 )
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[3]))/20 )
- lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[3]-l[4]))/20;

lps[14] = log( beta_3[1] )
+ log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[3]-l[4]))/20 )
+ log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[4]))/20 )
+ log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[3]))/20 )
- lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[3]-l[4]))/20;

lps[15] = log1m_exp( -lambda[city_index4[i]][1,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[2]-l[3]-l[4]))/20 ) +
log1m_exp( -lambda[city_index4[i]][2,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[3]-l[4]))/20 ) +
log1m_exp( -lambda[city_index4[i]][3,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]-l[4]))/20 ) +
log1m_exp( -lambda[city_index4[i]][4,]*age_weight4[,i]*log1p_exp(20*(age4[i]-l[1]-l[2]-l[3]))/20 );

target += count4[i]*log_sum_exp(lps);
}


1/lambda_mean1 ~ exponential(1);
1/lambda_mean2 ~ exponential(1);
l ~ normal(1.8,0.15);

}
"


statusmodel_compiled <- rstan::stan_model(
  model_name = 'statusmodel',
  model_code = gsub('\t','',statusmodel_txt)
)

stan_data <- readRDS("stan_data_StatusModel.rds")

model_fit <- rstan::sampling(statusmodel_compiled,
                             data=stan_data,
                             chains=8,
                             cores=8,
                             init=list(list(l=rep(1.8,4)),
                                       list(l=rep(1.8,4)),
                                       list(l=rep(1.8,4)),
                                       list(l=rep(1.8,4)),
                                       list(l=rep(1.8,4)),
                                       list(l=rep(1.8,4)),
                                       list(l=rep(1.8,4)),
                                       list(l=rep(1.8,4))),
                             warmup=5e2,
                             iter=4e3)


