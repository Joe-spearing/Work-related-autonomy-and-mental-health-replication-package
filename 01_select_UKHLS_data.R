rm(list=ls())
library(haven)

#load indresp 2,4,6,8,10
#####################
#Define repositories#
#####################
data.repository<-'C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\Autonomy_and_mental_health\\data'
setwd(data.repository)

letters<-c('a','b','c','d','e','f','g','h','i','j')
for (val in seq(from=1,to=10,by=1)){
  
  file.name<-paste(letters[val],'_indresp.SAV',sep='')
  
  df<-read_sav(file.name)
  
  assign(paste('indresp_',val,sep=''),df)
  
  
}

#make a list of indresps
indresp_list<-list(indresp_1,indresp_2,indresp_3,indresp_4,indresp_5,
                   indresp_6,indresp_7,indresp_8,indresp_9,indresp_10)

#extract the variables I want and stack them in a dataset for the even numbers
for (val in seq(from=2,to=10,by=2)){

  #get the right df
  this.df<-as.data.frame(indresp_list[val])
    
  #get my vars
  #mh measures
  ghq1<-paste(letters[val],'_scghqa',sep='')
  ghq2<-paste(letters[val],'_scghqb',sep='')
  ghq3<-paste(letters[val],'_scghqc',sep='')
  ghq4<-paste(letters[val],'_scghqd',sep='')
  ghq5<-paste(letters[val],'_scghqe',sep='')
  ghq6<-paste(letters[val],'_scghqf',sep='')
  ghq7<-paste(letters[val],'_scghqg',sep='')
  ghq8<-paste(letters[val],'_scghqh',sep='')
  ghq9<-paste(letters[val],'_scghqi',sep='')
  ghq10<-paste(letters[val],'_scghqj',sep='')
  ghq11<-paste(letters[val],'_scghqk',sep='')
  ghq12<-paste(letters[val],'_scghql',sep='')
  
  sf12<-paste(letters[val],'_sf12mcs_dv',sep='')
  
  #sex
  sex<-paste(letters[val],'_sex',sep='')
  #age
  age<-paste(letters[val],'_age_dv',sep='')
  #race
  race<-paste(letters[val],'_racel',sep='')
  #education
  educ<-paste(letters[val],'_qfhigh_dv',sep='')
  #marital status
  mar.stat<-paste(letters[val],'_marstat',sep='')
  #income
  income<-paste(letters[val],'_fimnlabgrs_dv',sep='')
  #autonomy
  wkaut1<-paste(letters[val],'_wkaut1',sep='')
  wkaut2<-paste(letters[val],'_wkaut2',sep='')
  wkaut3<-paste(letters[val],'_wkaut3',sep='')
  wkaut4<-paste(letters[val],'_wkaut4',sep='')
  wkaut5<-paste(letters[val],'_wkaut5',sep='')
  #occupation
  occ<-paste(letters[val],'_jbisco88_cc',sep='')
  #first occupation
  first.occ<-paste(letters[val],'_j1soc00_cc',sep='')
  #employment status
  jb.stat<-paste(letters[val],'_jbstat',sep='')
  #same employer
  same.employer<-paste(letters[val],'_jbsamr',sep='')
  #number of biological children
  nchild<-paste(letters[val],'_nnatch',sep='')
  #year
  interview.year<-paste(letters[val],'_istrtdaty',sep='')
  #full time/part time
  ft.pt<-paste(letters[val],'_jbft_dv',sep='')
  
  df<-as.data.frame(
    cbind(
      this.df[,1],
      this.df[,ghq1],
      this.df[,ghq2],
      this.df[,ghq3],
      this.df[,ghq4],
      this.df[,ghq5],
      this.df[,ghq6],
      this.df[,ghq7],
      this.df[,ghq8],
      this.df[,ghq9],
      this.df[,ghq10],
      this.df[,ghq11],
      this.df[,ghq12],
      this.df[,sf12],
      this.df[,sex],
      this.df[,age],
      this.df[,race],
      this.df[,educ],
      this.df[,mar.stat],
      this.df[,income],
      this.df[,wkaut1],
      this.df[,wkaut2],
      this.df[,wkaut3],
      this.df[,wkaut4],
      this.df[,wkaut5],
      this.df[,occ],
      this.df[,ft.pt],
      this.df[,first.occ],
      this.df[,jb.stat],
      this.df[,same.employer],
      this.df[,nchild],
      matrix(data=val,nrow=nrow(this.df),ncol=1),
      this.df[,interview.year]
    )
  )
  
  colnames(df)<-c('pid','ghq1','ghq2','ghq3','ghq4',
                  'ghq5','ghq6','ghq7','ghq8','ghq9',
                  'ghq10','ghq11','ghq12','SF12',
                  'sex','age','race','education','marital_stat',
                  'labor_income','wkaut1','wkaut2',
                  'wkaut3','wkaut4','wkaut5','occ','full_time',
                  'first_occ',
                  'jb_stat','same_employer',
                  'no_children','wave_number','year')

  assign(paste('df_',val,sep=''),df)
}  

#Likewise for the even numbers
for (val in seq(from=1,to=9,by=2)){
  
  #get the right df
  this.df<-as.data.frame(indresp_list[val])
  
  #get my vars
  #mh measures
  ghq1<-paste(letters[val],'_scghqa',sep='')
  ghq2<-paste(letters[val],'_scghqb',sep='')
  ghq3<-paste(letters[val],'_scghqc',sep='')
  ghq4<-paste(letters[val],'_scghqd',sep='')
  ghq5<-paste(letters[val],'_scghqe',sep='')
  ghq6<-paste(letters[val],'_scghqf',sep='')
  ghq7<-paste(letters[val],'_scghqg',sep='')
  ghq8<-paste(letters[val],'_scghqh',sep='')
  ghq9<-paste(letters[val],'_scghqi',sep='')
  ghq10<-paste(letters[val],'_scghqj',sep='')
  ghq11<-paste(letters[val],'_scghqk',sep='')
  ghq12<-paste(letters[val],'_scghql',sep='')
  
  sf12<-paste(letters[val],'_sf12mcs_dv',sep='')
  
  #sex
  sex<-paste(letters[val],'_sex',sep='')
  #age
  age<-paste(letters[val],'_age_dv',sep='')
  #race
  race<-paste(letters[val],'_racel',sep='')
  #education
  educ<-paste(letters[val],'_qfhigh_dv',sep='')
  #marital status
  mar.stat<-paste(letters[val],'_marstat',sep='')
  #income
  income<-paste(letters[val],'_fimnlabgrs_dv',sep='')
  #occupation
  occ<-paste(letters[val],'_jbisco88_cc',sep='')
  #first occupation
  first.occ<-paste(letters[val],'_j1soc00_cc',sep='')
  #employment status
  jb.stat<-paste(letters[val],'_jbstat',sep='')
  #same employer
  same.employer<-paste(letters[val],'_jbsamr',sep='')
  #number of biological children
  nchild<-paste(letters[val],'_nnatch',sep='')
  #year
  interview.year<-paste(letters[val],'_istrtdaty',sep='')
  #full time/part time
  ft.pt<-paste(letters[val],'_jbft_dv',sep='')
  
  same.employer.dat<-ifelse(val==1,matrix(data=NA,nrow=nrow(this.df),ncol=1),this.df[,same.employer])
  
  df<-as.data.frame(
    cbind(
      this.df[,1],
      this.df[,ghq1],
      this.df[,ghq2],
      this.df[,ghq3],
      this.df[,ghq4],
      this.df[,ghq5],
      this.df[,ghq6],
      this.df[,ghq7],
      this.df[,ghq8],
      this.df[,ghq9],
      this.df[,ghq10],
      this.df[,ghq11],
      this.df[,ghq12],
      this.df[,sf12],
      this.df[,sex],
      this.df[,age],
      this.df[,race],
      this.df[,educ],
      this.df[,mar.stat],
      this.df[,income],
      matrix(data=NA,nrow=nrow(this.df),ncol=5),
      this.df[,occ],
      this.df[,ft.pt],
      this.df[,first.occ],
      this.df[,jb.stat],
      same.employer.dat,
      this.df[,nchild],
      matrix(data=val,nrow=nrow(this.df),ncol=1),
      this.df[,interview.year]
    )
  )
  
  colnames(df)<-c('pid','ghq1','ghq2','ghq3','ghq4',
                  'ghq5','ghq6','ghq7','ghq8','ghq9',
                  'ghq10','ghq11','ghq12','SF12',
                  'sex','age','race','education','marital_stat',
                  'labor_income','wkaut1','wkaut2',
                  'wkaut3','wkaut4','wkaut5','occ','full_time',
                  'first_occ',
                  'jb_stat','same_employer',
                  'no_children','wave_number','year')
  
  assign(paste('df_',val,sep=''),df)
}  

#stitch data together
full.data.set<-as.data.frame(rbind(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9,df_10))

#now add in personality traits
file.name<-paste(letters[3],'_indresp.SAV',sep='')
df<-read_sav(file.name)

pers.vars<-as.data.frame(
  cbind(df[match(as.character(unlist(full.data.set[1])),
                as.character(unlist(df[,1]))),'c_big5a_dv'],
        df[match(as.character(unlist(full.data.set[1])),
                 as.character(unlist(df[,1]))),'c_big5c_dv'],
        df[match(as.character(unlist(full.data.set[1])),
                 as.character(unlist(df[,1]))),'c_big5e_dv'],
        df[match(as.character(unlist(full.data.set[1])),
                 as.character(unlist(df[,1]))),'c_big5n_dv'],
        df[match(as.character(unlist(full.data.set[1])),
                 as.character(unlist(df[,1]))),'c_big5o_dv']))

colnames(pers.vars)<-c('agreeableness','conscientiousness','extraversion',
                       'neuroticism','openness')

full.data.set<-as.data.frame(cbind(full.data.set,pers.vars))

write.csv(full.data.set,'autonomy_mh_data_set.CSV')
