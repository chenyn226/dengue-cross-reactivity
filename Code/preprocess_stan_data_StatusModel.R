
library(data.table)

time_index_data <- data.table(time_group=as.character(1983:2017),
                              year=1983:2017,time_index=1:35)
below_age <- 1983

time_index_data <- time_index_data[,c(2,3)]
T_num_this <- max(time_index_data$time_index)

dataall <- read.csv("vietnam_serosurvey.csv")
dataall <- as.data.table(dataall)
dataall <- subset(dataall,is.na(PanbioUnit))

colnames(dataall)
dataall[,status:=DV1_status+DV2_status+DV3_status+DV4_status]
colnames(dataall)[15]<-"sample_year"
colnames(dataall)[31]<-"d1_status"
colnames(dataall)[32]<-"d2_status"
colnames(dataall)[33]<-"d3_status"
colnames(dataall)[34]<-"d4_status"

dataall[,age:=round(AGE_MIN)]


stan_data <- list()
stan_data$K_virus <- 4
stan_data$b1 <- 0.99
stan_data$T_num <- max(time_index_data$time_index)
stan_data$K_city <- 2

unique(dataall$id1)
city_index_data <- data.table(id1=c("HC","KH"),cityindex=c(1:2))

##### seropositive:0 #####
colnames(dataall)

sero0 <- subset(dataall,status==0)
sero0 <- sero0[,list(count=.N),by=c("age","sample_year","id1")]
sero0[,self_index:=1:dim(sero0)[1]]

age_weight_matrix <- matrix(0,nrow=dim(sero0)[1],ncol=T_num_this)
decay_length_matrix <- matrix(0,nrow=dim(sero0)[1],ncol=T_num_this)


i=1
year_end = sero0[i,]$sample_year
year_start = sero0[i,]$sample_year - sero0[i,]$age + 1 
tmp1 <- data.frame(year=year_start:year_end)
stopifnot(dim(tmp1)[1] == sero0[i,]$age)

tmp1_1 <- subset(tmp1,year<=below_age)
tmp1_2 <- subset(tmp1,year>below_age)

if (dim(tmp1_1)[1]==0){
  tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
  tmp3 <- tmp1_2
}else{
  tmp1_1$time_index=1
  tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
  tmp3 <- rbind(tmp1_1,tmp1_2)
}

tmp3 <- as.data.table(tmp3)

tmp3 <- as.data.table(tmp3)
tmp3[,length_x:=((sero0[i,]$age-1):0)+0.5]

tmp4 <- tmp3[,list(count=.N,length1=mean(length_x)),by=time_index]
tmp4[,age_weight:=count/sero0[i,]$age]
stopifnot(sum(tmp4$age_weight)>0.999&sum(tmp4$age_weight)<1.001)

tmp5 <- data.table(time_index=1:T_num_this)
tmp5 <- merge(tmp5,tmp4,by="time_index",all.x = TRUE)
set(tmp5,which(tmp5[,is.na(age_weight)]),"age_weight",0)
set(tmp5,which(tmp5[,is.na(length1)]),"length1",0)
tmp5 <- tmp5[order(tmp5[,time_index]),]

age_weight_matrix[i,] = tmp5$age_weight
decay_length_matrix[i,] = tmp5$length1

for (i in 2:dim(sero0)[1]){
  print(i)
  year_end = sero0[i,]$sample_year
  year_start = sero0[i,]$sample_year - sero0[i,]$age + 1 
  tmp1 <- data.frame(year=year_start:year_end)
  stopifnot(dim(tmp1)[1] == sero0[i,]$age)
  
  tmp1_1 <- subset(tmp1,year<=below_age)
  tmp1_2 <- subset(tmp1,year>below_age)
  
  if (dim(tmp1_1)[1]==0){
    tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
    tmp3 <- tmp1_2
  }else{
    tmp1_1$time_index=1
    tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
    tmp3 <- rbind(tmp1_1,tmp1_2)
  }
  
  tmp3 <- as.data.table(tmp3)
  
  tmp3 <- as.data.table(tmp3)
  tmp3[,length_x:=((sero0[i,]$age-1):0)+0.5]
  
  tmp4 <- tmp3[,list(count=.N,length1=mean(length_x)),by=time_index]
  tmp4[,age_weight:=count/sero0[i,]$age]
  stopifnot(sum(tmp4$age_weight)>0.999&sum(tmp4$age_weight)<1.001)
  
  tmp5 <- data.table(time_index=1:T_num_this)
  tmp5 <- merge(tmp5,tmp4,by="time_index",all.x = TRUE)
  set(tmp5,which(tmp5[,is.na(age_weight)]),"age_weight",0)
  set(tmp5,which(tmp5[,is.na(length1)]),"length1",0)
  tmp5 <- tmp5[order(tmp5[,time_index]),]
  
  age_weight_matrix[i,] = tmp5$age_weight
  decay_length_matrix[i,] = tmp5$length1
  
  
  
}

sero0 <- merge(sero0,city_index_data,by="id1")
sero0 <- sero0[order(sero0[,self_index]),]

stan_data$N0 <- dim(sero0)[1]
stan_data$age0 <- sero0$age
stan_data$age_weight0 <- t(age_weight_matrix)
stan_data$count0 <- sero0$count
stan_data$city_index0 <- sero0$cityindex

##### seropositive:1 #####
sero1 <- subset(dataall,status==1)
sero1 <- sero1[,list(count=.N),by=c("age","sample_year","d1_status","d2_status","d3_status","d4_status","id1")]
sero1[,self_index:=1:dim(sero1)[1]]

for (k in 1:dim(sero1)[1]){
  type_status <- sero1[k,]
  if (type_status$d1_status==1){
    set(sero1,k,"pos_index",1)
  }else if (type_status$d2_status==1){
    set(sero1,k,"pos_index",2)
  }else if (type_status$d3_status==1){
    set(sero1,k,"pos_index",3)
  }else if (type_status$d4_status==1){
    set(sero1,k,"pos_index",4)
  }
  
}
sero1 <- as.data.table(sero1)
sero1 <- sero1[order(sero1[,self_index]),]

for (k in 1:dim(sero1)[1]){
  pos_index <- sero1[k,]$pos_index
  neg_index <- c(1:4)[which(!c(1:4)%in%pos_index)]
  stopifnot(length(neg_index)==3)
  
  set(sero1,k,"neg_index1",neg_index[1])
  set(sero1,k,"neg_index2",neg_index[2])
  set(sero1,k,"neg_index3",neg_index[3])
  
}

sero1 <- sero1[order(sero1[,self_index]),]

age_weight_matrix <- matrix(0,nrow=dim(sero1)[1],ncol=T_num_this)
age_weight_matrix_primary <- matrix(0,nrow=dim(sero1)[1],ncol=T_num_this)
age_weight_matrix_other <- matrix(0,nrow=dim(sero1)[1],ncol=T_num_this)

i=1
year_end = sero1[i,]$sample_year
year_start = sero1[i,]$sample_year - sero1[i,]$age + 1 
tmp1 <- data.frame(year=year_start:year_end)
stopifnot(dim(tmp1)[1] == sero1[i,]$age)

tmp1_1 <- subset(tmp1,year<=below_age)
tmp1_2 <- subset(tmp1,year>below_age)

if (dim(tmp1_1)[1]==0){
  tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
  tmp3 <- tmp1_2
}else{
  tmp1_1$time_index=1
  tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
  tmp3 <- rbind(tmp1_1,tmp1_2)
}
tmp3 <- as.data.table(tmp3)
tmp3[,length_x:=((sero1[i,]$age-1):0)+0.5]

tmp4 <- tmp3[,list(count=.N,length1=mean(length_x)),by=time_index]
tmp4[,age_weight:=count/sero1[i,]$age]
stopifnot(sum(tmp4$age_weight)>0.999&sum(tmp4$age_weight)<1.001)

tmp5 <- data.table(time_index=1:T_num_this)
tmp5 <- merge(tmp5,tmp4,by="time_index",all.x = TRUE)
set(tmp5,which(tmp5[,is.na(age_weight)]),"age_weight",0)
set(tmp5,which(tmp5[,is.na(length1)]),"length1",0)
tmp5 <- tmp5[order(tmp5[,time_index]),]

if (dim(tmp3)[1]>2){
  tmp3 <- tmp3[order(tmp3[,time_index]),]
  tmp6 <- tmp3[c(dim(tmp3)[1]-1,dim(tmp3)[1]),]
  tmp6 <- tmp6[,list(count=.N),by=time_index]
  tmp6[,age_weight:=count/2]
  tmp8 <- tmp3[1:(dim(tmp3)[1]-2),]
  tmp8 <- tmp8[,list(count=.N),by=time_index]
  tmp8[,age_weight:=count/(sero1[i,]$age-2)]
  
  tmp7 <- data.table(time_index=1:T_num_this)
  tmp7 <- merge(tmp7,tmp6,by="time_index",all.x = TRUE)
  set(tmp7,which(tmp7[,is.na(age_weight)]),"age_weight",0)
  tmp7 <- tmp7[order(tmp7[,time_index]),]
  
  tmp9 <- data.table(time_index=1:T_num_this)
  tmp9 <- merge(tmp9,tmp8,by="time_index",all.x = TRUE)
  set(tmp9,which(tmp9[,is.na(age_weight)]),"age_weight",0)
  tmp9 <- tmp9[order(tmp9[,time_index]),]
}else{
  tmp9 <- data.table(age_weight=rep(0,dim(tmp5)[1]))
  tmp7 <- tmp5
}

age_weight_matrix[i,] = tmp5$age_weight
age_weight_matrix_primary[i,] = tmp7$age_weight
age_weight_matrix_other[i,] = tmp9$age_weight

for (i in 2:dim(sero1)[1]){
  print(i)
  year_end = sero1[i,]$sample_year
  year_start = sero1[i,]$sample_year - sero1[i,]$age + 1 
  tmp1 <- data.frame(year=year_start:year_end)
  stopifnot(dim(tmp1)[1] == sero1[i,]$age)
  
  tmp1_1 <- subset(tmp1,year<=below_age)
  tmp1_2 <- subset(tmp1,year>below_age)
  
  if (dim(tmp1_1)[1]==0){
    tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
    tmp3 <- tmp1_2
  }else{
    tmp1_1$time_index=1
    tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
    tmp3 <- rbind(tmp1_1,tmp1_2)
  }
  tmp3 <- as.data.table(tmp3)
  tmp3[,length_x:=((sero1[i,]$age-1):0)+0.5]
  
  tmp4 <- tmp3[,list(count=.N,length1=mean(length_x)),by=time_index]
  tmp4[,age_weight:=count/sero1[i,]$age]
  stopifnot(sum(tmp4$age_weight)>0.999&sum(tmp4$age_weight)<1.001)
  
  tmp5 <- data.table(time_index=1:T_num_this)
  tmp5 <- merge(tmp5,tmp4,by="time_index",all.x = TRUE)
  set(tmp5,which(tmp5[,is.na(age_weight)]),"age_weight",0)
  set(tmp5,which(tmp5[,is.na(length1)]),"length1",0)
  tmp5 <- tmp5[order(tmp5[,time_index]),]
  
  if (dim(tmp3)[1]>2){
    tmp3 <- tmp3[order(tmp3[,time_index]),]
    tmp6 <- tmp3[c(dim(tmp3)[1]-1,dim(tmp3)[1]),]
    tmp6 <- tmp6[,list(count=.N),by=time_index]
    tmp6[,age_weight:=count/2]
    tmp8 <- tmp3[1:(dim(tmp3)[1]-2),]
    tmp8 <- tmp8[,list(count=.N),by=time_index]
    tmp8[,age_weight:=count/(sero1[i,]$age-2)]
    
    tmp7 <- data.table(time_index=1:T_num_this)
    tmp7 <- merge(tmp7,tmp6,by="time_index",all.x = TRUE)
    set(tmp7,which(tmp7[,is.na(age_weight)]),"age_weight",0)
    tmp7 <- tmp7[order(tmp7[,time_index]),]
    
    tmp9 <- data.table(time_index=1:T_num_this)
    tmp9 <- merge(tmp9,tmp8,by="time_index",all.x = TRUE)
    set(tmp9,which(tmp9[,is.na(age_weight)]),"age_weight",0)
    tmp9 <- tmp9[order(tmp9[,time_index]),]
  }else{
    tmp9 <- data.table(age_weight=rep(0,dim(tmp5)[1]))
    tmp7 <- tmp5
  }
  
  
  age_weight_matrix[i,] = tmp5$age_weight
  age_weight_matrix_primary[i,] = tmp7$age_weight
  age_weight_matrix_other[i,] = tmp9$age_weight
  
}



sero1 <- merge(sero1,city_index_data,by="id1")
sero1 <- sero1[order(sero1[,self_index]),]

stan_data$N1 <- dim(sero1)[1]
stan_data$age1 <- sero1$age
stan_data$pos_index1_1 <- sero1$pos_index
stan_data$neg_index1_1 <- sero1$neg_index1
stan_data$neg_index1_2 <- sero1$neg_index2
stan_data$neg_index1_3 <- sero1$neg_index3
stan_data$age_weight1 <- t(age_weight_matrix)
stan_data$age_weight_primary1 <- t(age_weight_matrix_primary)
stan_data$age_weight_other1 <- t(age_weight_matrix_other)
stan_data$count1 <- sero1$count
stan_data$decay_length1 <- t(decay_length_matrix)
stan_data$city_index1 <- sero1$cityindex

##### seropositive:2 #####
sero2 <- subset(dataall,status==2)
sero2 <- sero2[,list(count=.N),by=c("age","sample_year","d1_status","d2_status","d3_status","d4_status","id1")]
sero2[,self_index:=1:dim(sero2)[1]]

colnames(sero2)

for (k in 1:dim(sero2)[1]){
  print(k)
  type_status <- sero2[k,c(3:6)][1,]
  pos_index <- which(type_status==1)
  neg_index <- which(!type_status==1)
  stopifnot(length(pos_index)==2)
  stopifnot(length(neg_index)==2)
  set(sero2,k,"pos_index1",pos_index[1])
  set(sero2,k,"pos_index2",pos_index[2])
  set(sero2,k,"neg_index1",neg_index[1])
  set(sero2,k,"neg_index2",neg_index[2])
  
}
sero2 <- sero2[order(sero2[,self_index]),]

age_weight_matrix <- matrix(0,nrow=dim(sero2)[1],ncol=T_num_this)
age_weight_matrix_primary <- matrix(0,nrow=dim(sero2)[1],ncol=T_num_this)
age_weight_matrix_other <- matrix(0,nrow=dim(sero2)[1],ncol=T_num_this)

i=1
year_end = sero2[i,]$sample_year
year_start = sero2[i,]$sample_year - sero2[i,]$age + 1 
tmp1 <- data.frame(year=year_start:year_end)
stopifnot(dim(tmp1)[1] == sero2[i,]$age)

tmp1_1 <- subset(tmp1,year<=below_age)
tmp1_2 <- subset(tmp1,year>below_age)

if (dim(tmp1_1)[1]==0){
  tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
  tmp3 <- tmp1_2
}else{
  tmp1_1$time_index=1
  tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
  tmp3 <- rbind(tmp1_1,tmp1_2)
}

tmp3 <- as.data.table(tmp3)

tmp3[,length_x:=((sero2[i,]$age-1):0)+0.5]

tmp4 <- tmp3[,list(count=.N,length1=mean(length_x)),by=time_index]
tmp4[,age_weight:=count/sero2[i,]$age]
stopifnot(sum(tmp4$age_weight)>0.999&sum(tmp4$age_weight)<1.001)

tmp5 <- data.table(time_index=1:T_num_this)
tmp5 <- merge(tmp5,tmp4,by="time_index",all.x = TRUE)
set(tmp5,which(tmp5[,is.na(age_weight)]),"age_weight",0)
set(tmp5,which(tmp5[,is.na(length1)]),"length1",0)
tmp5 <- tmp5[order(tmp5[,time_index]),]

if (dim(tmp3)[1]>2){
  tmp3 <- tmp3[order(tmp3[,time_index]),]
  tmp6 <- tmp3[c(dim(tmp3)[1]-1,dim(tmp3)[1]),]
  tmp6 <- tmp6[,list(count=.N),by=time_index]
  tmp6[,age_weight:=count/2]
  tmp8 <- tmp3[1:(dim(tmp3)[1]-2),]
  tmp8 <- tmp8[,list(count=.N),by=time_index]
  tmp8[,age_weight:=count/(sero2[i,]$age-2)]
  
  tmp7 <- data.table(time_index=1:T_num_this)
  tmp7 <- merge(tmp7,tmp6,by="time_index",all.x = TRUE)
  set(tmp7,which(tmp7[,is.na(age_weight)]),"age_weight",0)
  tmp7 <- tmp7[order(tmp7[,time_index]),]
  
  tmp9 <- data.table(time_index=1:T_num_this)
  tmp9 <- merge(tmp9,tmp8,by="time_index",all.x = TRUE)
  set(tmp9,which(tmp9[,is.na(age_weight)]),"age_weight",0)
  tmp9 <- tmp9[order(tmp9[,time_index]),]
}else{
  tmp9 <- data.table(age_weight=rep(0,dim(tmp5)[1]))
  tmp7 <- tmp5
}

age_weight_matrix[i,] = tmp5$age_weight
age_weight_matrix_primary[i,] = tmp7$age_weight
age_weight_matrix_other[i,] = tmp9$age_weight

for (i in 2:dim(sero2)[1]){
  print(i)
  year_end = sero2[i,]$sample_year
  year_start = sero2[i,]$sample_year - sero2[i,]$age + 1 
  tmp1 <- data.frame(year=year_start:year_end)
  stopifnot(dim(tmp1)[1] == sero2[i,]$age)
  
  tmp1_1 <- subset(tmp1,year<=below_age)
  tmp1_2 <- subset(tmp1,year>below_age)
  
  if (dim(tmp1_1)[1]==0){
    tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
    tmp3 <- tmp1_2
  }else{
    tmp1_1$time_index=1
    tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
    tmp3 <- rbind(tmp1_1,tmp1_2)
  }
  
  tmp3 <- as.data.table(tmp3)
  
  tmp3[,length_x:=((sero2[i,]$age-1):0)+0.5]
  
  tmp4 <- tmp3[,list(count=.N,length1=mean(length_x)),by=time_index]
  tmp4[,age_weight:=count/sero2[i,]$age]
  stopifnot(sum(tmp4$age_weight)>0.999&sum(tmp4$age_weight)<1.001)
  
  tmp5 <- data.table(time_index=1:T_num_this)
  tmp5 <- merge(tmp5,tmp4,by="time_index",all.x = TRUE)
  set(tmp5,which(tmp5[,is.na(age_weight)]),"age_weight",0)
  set(tmp5,which(tmp5[,is.na(length1)]),"length1",0)
  tmp5 <- tmp5[order(tmp5[,time_index]),]
  
  if (dim(tmp3)[1]>2){
    tmp3 <- tmp3[order(tmp3[,time_index]),]
    tmp6 <- tmp3[c(dim(tmp3)[1]-1,dim(tmp3)[1]),]
    tmp6 <- tmp6[,list(count=.N),by=time_index]
    tmp6[,age_weight:=count/2]
    tmp8 <- tmp3[1:(dim(tmp3)[1]-2),]
    tmp8 <- tmp8[,list(count=.N),by=time_index]
    tmp8[,age_weight:=count/(sero2[i,]$age-2)]
    
    tmp7 <- data.table(time_index=1:T_num_this)
    tmp7 <- merge(tmp7,tmp6,by="time_index",all.x = TRUE)
    set(tmp7,which(tmp7[,is.na(age_weight)]),"age_weight",0)
    tmp7 <- tmp7[order(tmp7[,time_index]),]
    
    tmp9 <- data.table(time_index=1:T_num_this)
    tmp9 <- merge(tmp9,tmp8,by="time_index",all.x = TRUE)
    set(tmp9,which(tmp9[,is.na(age_weight)]),"age_weight",0)
    tmp9 <- tmp9[order(tmp9[,time_index]),]
  }else{
    tmp9 <- data.table(age_weight=rep(0,dim(tmp5)[1]))
    tmp7 <- tmp5
  }
  
  
  age_weight_matrix[i,] = tmp5$age_weight
  age_weight_matrix_primary[i,] = tmp7$age_weight
  age_weight_matrix_other[i,] = tmp9$age_weight
}


sero2 <- merge(sero2,city_index_data,by="id1")
sero2 <- sero2[order(sero2[,self_index]),]

stan_data$N2 <- dim(sero2)[1]
stan_data$age2 <- sero2$age
stan_data$pos_index2_1 <- sero2$pos_index1
stan_data$pos_index2_2 <- sero2$pos_index2
stan_data$neg_index2_1 <- sero2$neg_index1
stan_data$neg_index2_2 <- sero2$neg_index2
stan_data$age_weight2 <- t(age_weight_matrix)
stan_data$age_weight_primary2 <- t(age_weight_matrix_primary)
stan_data$age_weight_other2 <- t(age_weight_matrix_other)
stan_data$count2 <- sero2$count
stan_data$decay_length2 <- t(decay_length_matrix)
stan_data$city_index2 <- sero2$cityindex


##### seropositive:3 #####
sero3 <- subset(dataall,status==3)
sero3 <- sero3[,list(count=.N),by=c("age","sample_year","d1_status","d2_status","d3_status","d4_status","id1")]
sero3[,self_index:=1:dim(sero3)[1]]



for (k in 1:dim(sero3)[1]){
  type_status <- sero3[k,c(3:6)][1,]
  pos_index <- which(type_status==1)
  neg_index <- which(!type_status==1)
  stopifnot(length(pos_index)==3)
  stopifnot(length(neg_index)==1)
  set(sero3,k,"pos_index1",pos_index[1])
  set(sero3,k,"pos_index2",pos_index[2])
  set(sero3,k,"pos_index3",pos_index[3])
  set(sero3,k,"neg_index",neg_index[1])
  
}
sero3 <- sero3[order(sero3[,self_index]),]

age_weight_matrix <- matrix(0,nrow=dim(sero3)[1],ncol=T_num_this)
age_weight_matrix_primary <- matrix(0,nrow=dim(sero3)[1],ncol=T_num_this)
age_weight_matrix_other <- matrix(0,nrow=dim(sero3)[1],ncol=T_num_this)

i=1
year_end = sero3[i,]$sample_year
year_start = sero3[i,]$sample_year - sero3[i,]$age + 1 
tmp1 <- data.frame(year=year_start:year_end)
stopifnot(dim(tmp1)[1] == sero3[i,]$age)

tmp1_1 <- subset(tmp1,year<=below_age)
tmp1_2 <- subset(tmp1,year>below_age)

if (dim(tmp1_1)[1]==0){
  tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
  tmp3 <- tmp1_2
}else{
  tmp1_1$time_index=1
  tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
  tmp3 <- rbind(tmp1_1,tmp1_2)
}

tmp3 <- as.data.table(tmp3)
tmp3[,length_x:=((sero3[i,]$age-1):0)+0.5]

tmp4 <- tmp3[,list(count=.N,length1=mean(length_x)),by=time_index]
tmp4[,age_weight:=count/sero3[i,]$age]
stopifnot(sum(tmp4$age_weight)>0.999&sum(tmp4$age_weight)<1.001)

tmp5 <- data.table(time_index=1:T_num_this)
tmp5 <- merge(tmp5,tmp4,by="time_index",all.x = TRUE)
set(tmp5,which(tmp5[,is.na(age_weight)]),"age_weight",0)
set(tmp5,which(tmp5[,is.na(length1)]),"length1",0)
tmp5 <- tmp5[order(tmp5[,time_index]),]

if (dim(tmp3)[1]>2){
  tmp3 <- tmp3[order(tmp3[,time_index]),]
  tmp6 <- tmp3[c(dim(tmp3)[1]-1,dim(tmp3)[1]),]
  tmp6 <- tmp6[,list(count=.N),by=time_index]
  tmp6[,age_weight:=count/2]
  tmp8 <- tmp3[1:(dim(tmp3)[1]-2),]
  tmp8 <- tmp8[,list(count=.N),by=time_index]
  tmp8[,age_weight:=count/(sero3[i,]$age-2)]
  
  tmp7 <- data.table(time_index=1:T_num_this)
  tmp7 <- merge(tmp7,tmp6,by="time_index",all.x = TRUE)
  set(tmp7,which(tmp7[,is.na(age_weight)]),"age_weight",0)
  tmp7 <- tmp7[order(tmp7[,time_index]),]
  
  tmp9 <- data.table(time_index=1:T_num_this)
  tmp9 <- merge(tmp9,tmp8,by="time_index",all.x = TRUE)
  set(tmp9,which(tmp9[,is.na(age_weight)]),"age_weight",0)
  tmp9 <- tmp9[order(tmp9[,time_index]),]
}else{
  tmp9 <- data.table(age_weight=rep(0,dim(tmp5)[1]))
  tmp7 <- tmp5
}


age_weight_matrix[i,] = tmp5$age_weight
age_weight_matrix_primary[i,] = tmp7$age_weight
age_weight_matrix_other[i,] = tmp9$age_weight

for (i in 2:dim(sero3)[1]){
  print(i)
  year_end = sero3[i,]$sample_year
  year_start = sero3[i,]$sample_year - sero3[i,]$age + 1 
  tmp1 <- data.frame(year=year_start:year_end)
  stopifnot(dim(tmp1)[1] == sero3[i,]$age)
  
  tmp1_1 <- subset(tmp1,year<=below_age)
  tmp1_2 <- subset(tmp1,year>below_age)
  
  if (dim(tmp1_1)[1]==0){
    tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
    tmp3 <- tmp1_2
  }else{
    tmp1_1$time_index=1
    tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
    tmp3 <- rbind(tmp1_1,tmp1_2)
  }
  
  tmp3 <- as.data.table(tmp3)
  tmp3[,length_x:=((sero3[i,]$age-1):0)+0.5]
  
  
  tmp4 <- tmp3[,list(count=.N,length1=mean(length_x)),by=time_index]
  tmp4[,age_weight:=count/sero3[i,]$age]
  stopifnot(sum(tmp4$age_weight)>0.999&sum(tmp4$age_weight)<1.001)
  
  tmp5 <- data.table(time_index=1:T_num_this)
  tmp5 <- merge(tmp5,tmp4,by="time_index",all.x = TRUE)
  set(tmp5,which(tmp5[,is.na(age_weight)]),"age_weight",0)
  set(tmp5,which(tmp5[,is.na(length1)]),"length1",0)
  tmp5 <- tmp5[order(tmp5[,time_index]),]
  
  if (dim(tmp3)[1]>2){
    tmp3 <- tmp3[order(tmp3[,time_index]),]
    tmp6 <- tmp3[c(dim(tmp3)[1]-1,dim(tmp3)[1]),]
    tmp6 <- tmp6[,list(count=.N),by=time_index]
    tmp6[,age_weight:=count/2]
    tmp8 <- tmp3[1:(dim(tmp3)[1]-2),]
    tmp8 <- tmp8[,list(count=.N),by=time_index]
    tmp8[,age_weight:=count/(sero3[i,]$age-2)]
    
    tmp7 <- data.table(time_index=1:T_num_this)
    tmp7 <- merge(tmp7,tmp6,by="time_index",all.x = TRUE)
    set(tmp7,which(tmp7[,is.na(age_weight)]),"age_weight",0)
    tmp7 <- tmp7[order(tmp7[,time_index]),]
    
    tmp9 <- data.table(time_index=1:T_num_this)
    tmp9 <- merge(tmp9,tmp8,by="time_index",all.x = TRUE)
    set(tmp9,which(tmp9[,is.na(age_weight)]),"age_weight",0)
    tmp9 <- tmp9[order(tmp9[,time_index]),]
  }else{
    tmp9 <- data.table(age_weight=rep(0,dim(tmp5)[1]))
    tmp7 <- tmp5
  }
  
  
  age_weight_matrix[i,] = tmp5$age_weight
  age_weight_matrix_primary[i,] = tmp7$age_weight
  age_weight_matrix_other[i,] = tmp9$age_weight
  
}


sero3 <- merge(sero3,city_index_data,by="id1")
sero3 <- sero3[order(sero3[,self_index]),]

stan_data$N3 <- dim(sero3)[1]
stan_data$age3 <- sero3$age
stan_data$pos_index3_1 <- sero3$pos_index1
stan_data$pos_index3_2 <- sero3$pos_index2
stan_data$pos_index3_3 <- sero3$pos_index3
stan_data$neg_index3_1 <- sero3$neg_index
stan_data$age_weight3 <- t(age_weight_matrix)
stan_data$age_weight_primary3 <- t(age_weight_matrix_primary)
stan_data$age_weight_other3 <- t(age_weight_matrix_other)
stan_data$count3 <- sero3$count
stan_data$decay_length3 <- t(decay_length_matrix)
stan_data$city_index3 <- sero3$cityindex

##### seropositive:4 #####
sero4 <- subset(dataall,status==4)
sero4 <- sero4[,list(count=.N),by=c("age","sample_year","id1")]
sero4[,self_index:=1:dim(sero4)[1]]

age_weight_matrix <- matrix(0,nrow=dim(sero4)[1],ncol=T_num_this)
age_weight_matrix_primary <- matrix(0,nrow=dim(sero4)[1],ncol=T_num_this)
age_weight_matrix_other <- matrix(0,nrow=dim(sero4)[1],ncol=T_num_this)

i=1
year_end = sero4[i,]$sample_year
year_start = sero4[i,]$sample_year - sero4[i,]$age + 1 
tmp1 <- data.frame(year=year_start:year_end)
stopifnot(dim(tmp1)[1] == sero4[i,]$age)

tmp1_1 <- subset(tmp1,year<=below_age)
tmp1_2 <- subset(tmp1,year>below_age)

if (dim(tmp1_1)[1]==0){
  tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
  tmp3 <- tmp1_2
}else{
  tmp1_1$time_index=1
  tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
  tmp3 <- rbind(tmp1_1,tmp1_2)
}

tmp3 <- as.data.table(tmp3)
tmp3[,length_x:=((sero4[i,]$age-1):0)+0.5]

tmp4 <- tmp3[,list(count=.N,length1=mean(length_x)),by=time_index]
tmp4[,age_weight:=count/sero4[i,]$age]
stopifnot(sum(tmp4$age_weight)>0.999&sum(tmp4$age_weight)<1.001)

tmp5 <- data.table(time_index=1:T_num_this)
tmp5 <- merge(tmp5,tmp4,by="time_index",all.x = TRUE)
set(tmp5,which(tmp5[,is.na(age_weight)]),"age_weight",0)
set(tmp5,which(tmp5[,is.na(length1)]),"length1",0)
tmp5 <- tmp5[order(tmp5[,time_index]),]

if (dim(tmp3)[1]>2){
  tmp3 <- tmp3[order(tmp3[,time_index]),]
  tmp6 <- tmp3[c(dim(tmp3)[1]-1,dim(tmp3)[1]),]
  tmp6 <- tmp6[,list(count=.N),by=time_index]
  tmp6[,age_weight:=count/2]
  tmp8 <- tmp3[1:(dim(tmp3)[1]-2),]
  tmp8 <- tmp8[,list(count=.N),by=time_index]
  tmp8[,age_weight:=count/(sero4[i,]$age-2)]
  
  tmp7 <- data.table(time_index=1:T_num_this)
  tmp7 <- merge(tmp7,tmp6,by="time_index",all.x = TRUE)
  set(tmp7,which(tmp7[,is.na(age_weight)]),"age_weight",0)
  tmp7 <- tmp7[order(tmp7[,time_index]),]
  
  tmp9 <- data.table(time_index=1:T_num_this)
  tmp9 <- merge(tmp9,tmp8,by="time_index",all.x = TRUE)
  set(tmp9,which(tmp9[,is.na(age_weight)]),"age_weight",0)
  tmp9 <- tmp9[order(tmp9[,time_index]),]
}else{
  tmp9 <- data.table(age_weight=rep(0,dim(tmp5)[1]))
  tmp7 <- tmp5
}

age_weight_matrix[i,] = tmp5$age_weight
age_weight_matrix_primary[i,] = tmp7$age_weight
age_weight_matrix_other[i,] = tmp9$age_weight

for (i in 2:dim(sero4)[1]){
  print(i)
  year_end = sero4[i,]$sample_year
  year_start = sero4[i,]$sample_year - sero4[i,]$age + 1 
  tmp1 <- data.frame(year=year_start:year_end)
  stopifnot(dim(tmp1)[1] == sero4[i,]$age)
  
  tmp1_1 <- subset(tmp1,year<=below_age)
  tmp1_2 <- subset(tmp1,year>below_age)
  
  if (dim(tmp1_1)[1]==0){
    tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
    tmp3 <- tmp1_2
  }else{
    tmp1_1$time_index=1
    tmp1_2 <- merge(tmp1_2,time_index_data,by="year")
    tmp3 <- rbind(tmp1_1,tmp1_2)
  }
  
  tmp3 <- as.data.table(tmp3)
  tmp3[,length_x:=((sero4[i,]$age-1):0)+0.5]
  
  
  tmp4 <- tmp3[,list(count=.N,length1=mean(length_x)),by=time_index]
  tmp4[,age_weight:=count/sero4[i,]$age]
  stopifnot(sum(tmp4$age_weight)>0.999&sum(tmp4$age_weight)<1.001)
  
  tmp5 <- data.table(time_index=1:T_num_this)
  tmp5 <- merge(tmp5,tmp4,by="time_index",all.x = TRUE)
  set(tmp5,which(tmp5[,is.na(age_weight)]),"age_weight",0)
  set(tmp5,which(tmp5[,is.na(length1)]),"length1",0)
  tmp5 <- tmp5[order(tmp5[,time_index]),]
  
  if (dim(tmp3)[1]>2){
    tmp3 <- tmp3[order(tmp3[,time_index]),]
    tmp6 <- tmp3[c(dim(tmp3)[1]-1,dim(tmp3)[1]),]
    tmp6 <- tmp6[,list(count=.N),by=time_index]
    tmp6[,age_weight:=count/2]
    tmp8 <- tmp3[1:(dim(tmp3)[1]-2),]
    tmp8 <- tmp8[,list(count=.N),by=time_index]
    tmp8[,age_weight:=count/(sero4[i,]$age-2)]
    
    tmp7 <- data.table(time_index=1:T_num_this)
    tmp7 <- merge(tmp7,tmp6,by="time_index",all.x = TRUE)
    set(tmp7,which(tmp7[,is.na(age_weight)]),"age_weight",0)
    tmp7 <- tmp7[order(tmp7[,time_index]),]
    
    tmp9 <- data.table(time_index=1:T_num_this)
    tmp9 <- merge(tmp9,tmp8,by="time_index",all.x = TRUE)
    set(tmp9,which(tmp9[,is.na(age_weight)]),"age_weight",0)
    tmp9 <- tmp9[order(tmp9[,time_index]),]
  }else{
    tmp9 <- data.table(age_weight=rep(0,dim(tmp5)[1]))
    tmp7 <- tmp5
  }
  
  
  age_weight_matrix[i,] = tmp5$age_weight
  age_weight_matrix_primary[i,] = tmp7$age_weight
  age_weight_matrix_other[i,] = tmp9$age_weight  
}


sero4 <- merge(sero4,city_index_data,by="id1")
sero4 <- sero4[order(sero4[,self_index]),]

stan_data$N4 <- dim(sero4)[1]
stan_data$age4 <- sero4$age
stan_data$age_weight4 <- t(age_weight_matrix)
stan_data$age_weight_primary4 <- t(age_weight_matrix_primary)
stan_data$age_weight_other4 <- t(age_weight_matrix_other)
stan_data$count4 <- sero4$count
stan_data$decay_length4 <- t(decay_length_matrix)
stan_data$city_index4 <- sero4$cityindex

#saveRDS(stan_data,"stan_data_StatusModel.rds")
