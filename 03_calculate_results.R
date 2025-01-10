rm(list=ls())
library(sandwich)
library(splus2R)
library(lmtest)
library(plm)
library(fixest)
library(stats)
library(lfe)
library(testthat)
library(car)
library(haven)
library(stargazer)
library(rlang)
library(dplyr)
library(ivreg)
XTSUM <- function(data, varname, unit) {
  varname <- enquo(varname)
  loc.unit <- enquo(unit)
  ores <- data %>% summarise(ovr.mean=mean(!! varname, na.rm=TRUE), ovr.sd=sd(!! varname, na.rm=TRUE), ovr.min = min(!! varname, na.rm=TRUE), ovr.max=max(!! varname, na.rm=TRUE), ovr.N=sum(as.numeric((!is.na(!! varname)))))
  bmeans <- data %>% group_by(!! loc.unit) %>% summarise(meanx=mean(!! varname, na.rm=T), t.count=sum(as.numeric(!is.na(!! varname))))
  bres <- bmeans %>% ungroup() %>% summarise(between.sd = sd(meanx, na.rm=TRUE), between.min = min(meanx, na.rm=TRUE), between.max=max(meanx, na.rm=TRUE), Units=sum(as.numeric(!is.na(t.count))), t.bar=mean(t.count, na.rm=TRUE))
  wdat <- data %>% group_by(!! loc.unit) %>% mutate(W.x = scale(!! varname, scale=FALSE))
  wres <- wdat %>% ungroup() %>% summarise(within.sd=sd(W.x, na.rm=TRUE), within.min=min(W.x, na.rm=TRUE), within.max=max(W.x, na.rm=TRUE))
  return(list(ores=ores,bres=bres,wres=wres))
}

#####################
#Define repositories#
#####################

data.repository<-'C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\Autonomy_and_mental_health\\data'
output.repository<-'C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\Autonomy_and_mental_health\\data'

##########
#Load data
##########

setwd(data.repository)
full.data.set<-read.csv('autonomy_mh_data_set.CSV')
work.context<-read.csv('work_context_2019.CSV')
cw.isco88_soc00<-as.data.frame(read_dta('isco88_soc00.DTA'))
cw.soc00_soc10<-as.data.frame(read_dta('soc00_soc10.DTA'))

dv.soc10_isco88<-cbind(cw.soc00_soc10[,2],
                       cw.isco88_soc00[match(cw.soc00_soc10[,1],cw.isco88_soc00[,2]),1])


#create the ghq12 caseness
for (val in seq(from=1,to=12,by=1)){
  full.data.set[,paste('ghq',val,sep='')]<-
    round(full.data.set[,paste('ghq',val,sep='')]/4)
}


ghq12.caseness<-as.data.frame(full.data.set[,'ghq1']+full.data.set[,'ghq2']+
  full.data.set[,'ghq3']+full.data.set[,'ghq4']+
  full.data.set[,'ghq5']+full.data.set[,'ghq6']+
  full.data.set[,'ghq7']+full.data.set[,'ghq8']+
  full.data.set[,'ghq9']+full.data.set[,'ghq10']+
  full.data.set[,'ghq11']+full.data.set[,'ghq12'])
colnames(ghq12.caseness)<-'GHQ12_caseness'
full.data.set<-cbind(full.data.set,ghq12.caseness)

#Convert negatives to NAs
for (var in c('GHQ12_caseness','SF12','agreeableness',
              'conscientiousness','extraversion','neuroticism','openness',
              'marital_stat','education','occ','first_occ',
              'no_children')){
full.data.set[,var]<-ifelse(full.data.set[,var]<0,
                          NA, full.data.set[,var])
}

#create an autonomy variable
for (val in seq(from=1,to=5,by=1)){
  wkaut.no<-as.data.frame(
    ifelse(full.data.set[,paste('wkaut',val,sep='')]<0,
           NA,
           ifelse(full.data.set[,paste('wkaut',val,sep='')]==4,
                  1,0)))
  colnames(wkaut.no)<-paste('no_autonomy_','wkaut',val,sep='')
  
  full.data.set<-cbind(full.data.set,wkaut.no)
  
    full.data.set[,paste('wkaut',val,sep='')]<-
    ifelse(full.data.set[,paste('wkaut',val,sep='')]<0,
           NA,round(full.data.set[,paste('wkaut',val,sep='')]/4))
}

autonomy<-as.data.frame(round((full.data.set[,'wkaut1']+
       full.data.set[,'wkaut2']+full.data.set[,'wkaut3']+
         full.data.set[,'wkaut4']+full.data.set[,'wkaut5'])/5))
colnames(autonomy)<-'autonomy'
full.data.set<-cbind(full.data.set,autonomy)

#create lagged autonomy
lagged.autonomy<-as.data.frame(
full.data.set[match(interaction(full.data.set[,'pid'],full.data.set[,'wave_number']-2),
                    interaction(full.data.set[,'pid'],full.data.set[,'wave_number'])),'autonomy']
)
colnames(lagged.autonomy)<-'lagged_autonomy'
full.data.set<-cbind(full.data.set,lagged.autonomy)
#########################
#Create 1-digit occupation
one.dig.occ<-as.data.frame(substr(full.data.set[,'occ'],1,1))
colnames(one.dig.occ)<-'one_dig_occ'
full.data.set<-cbind(full.data.set,one.dig.occ)
#########################
#Also create an autonomy by occ-wave var
aut.occ.wv<-as.data.frame(aggregate(full.data.set[,'autonomy']~
                      interaction(full.data.set[,'occ'],full.data.set[,'wave_number']),FUN=mean))
colnames(aut.occ.wv)<-c('occ_wv','ave_autonomy')
aut.occ.wv<-as.data.frame(aut.occ.wv[match(interaction(full.data.set[,'occ'],full.data.set[,'wave_number']),
      aut.occ.wv[,'occ_wv']),'ave_autonomy'])
colnames(aut.occ.wv)<-'aut_occ_wv'
full.data.set<-cbind(full.data.set,aut.occ.wv)
#########################
#Convert to log income
full.data.set[,'labor_income']<-ifelse(full.data.set[,'labor_income']>0,
  log(full.data.set[,'labor_income']),NA)

#Add in a 1 digit occupation
one.dig.occ<-as.data.frame(substr(full.data.set[,'occ'],1,1))
colnames(one.dig.occ)<-'one_dig_occ'
full.data.set<-cbind(full.data.set,one.dig.occ)

#created lagged mental health variables
for (var in c('GHQ12_caseness','SF12')){

  lagged.mh<-as.data.frame(full.data.set[match(interaction(full.data.set[,'pid'],
                  full.data.set[,'wave_number']-1),
                  interaction(full.data.set[,'pid'],
                 full.data.set[,'wave_number'])),var])
  
  colnames(lagged.mh)<-paste('lagged_',var,sep='')
  
  full.data.set<-cbind(full.data.set,lagged.mh)
  
}

#start by creating different samples: those who always work
#those who always work fulltime, those who always work part time
#a data set which includes people who do not always work
ever.not.working<-unique(subset(full.data.set,jb_stat!=2)[,'pid'])
ever.not.ft<-unique(subset(full.data.set,full_time!=1)[,'pid'])
ever.not.pt<-unique(subset(full.data.set,full_time!=2)[,'pid'])
full.data.set.incl.non.workers<-full.data.set
full.data.set.ft<-subset(full.data.set,pid%in%ever.not.ft==FALSE)
full.data.set.pt<-subset(full.data.set,pid%in%ever.not.pt==FALSE)
full.data.set<-subset(full.data.set,pid%in%ever.not.working==FALSE)

#Remove all null observations
full.data.set.keep.no.pers<-subset(full.data.set,
                        education>=0&
                        age>0&
                        marital_stat>=0&
                        labor_income>=0&
                        occ>=0&
                        no_children>=0)
full.data.set<-subset(full.data.set.keep.no.pers,is.na(openness)==FALSE&
                        is.na(agreeableness)==FALSE&
                        is.na(conscientiousness)==FALSE&
                        is.na(extraversion)==FALSE&
                        is.na(neuroticism)==FALSE)
full.data.set.ft<-subset(full.data.set.ft,is.na(openness)==FALSE&
                        is.na(agreeableness)==FALSE&
                        is.na(conscientiousness)==FALSE&
                        is.na(extraversion)==FALSE&
                        is.na(neuroticism)==FALSE&
                        education>=0&
                        age>0&
                        marital_stat>=0&
                        labor_income>=0&
                        occ>=0&
                        no_children>=0)
full.data.set.pt<-subset(full.data.set.pt,is.na(openness)==FALSE&
                        is.na(agreeableness)==FALSE&
                        is.na(conscientiousness)==FALSE&
                        is.na(extraversion)==FALSE&
                        is.na(neuroticism)==FALSE&
                        education>=0&
                        age>0&
                        marital_stat>=0&
                        labor_income>=0&
                        occ>=0&
                        no_children>=0)
full.data.set.incl.non.workers<-subset(full.data.set.incl.non.workers,is.na(openness)==FALSE&
                                         is.na(agreeableness)==FALSE&
                                         is.na(conscientiousness)==FALSE&
                                         is.na(extraversion)==FALSE&
                                         is.na(neuroticism)==FALSE&
                                         education>=0&
                                         age>0&
                                         marital_stat>=0&
                                         labor_income>=0&
                                         occ>=0&
                                         no_children>=0)

#######################
#create a table of scores by occupation
occupatonal.employment<-
  as.matrix(sort(table(full.data.set[,'occ']),decreasing=TRUE))

aut.by.occ<-as.data.frame(aggregate(autonomy~occ,data=full.data.set,FUN=mean))

aut.by.occ.this<-aggregate(wkaut1~occ,data=full.data.set,FUN=mean)
aut.by.occ<-(cbind(aut.by.occ,
      aut.by.occ.this[match(aut.by.occ[,1],aut.by.occ.this[,1]),2]))

aut.by.occ.this<-aggregate(wkaut2~occ,data=full.data.set,FUN=mean)
aut.by.occ<-(cbind(aut.by.occ,
       aut.by.occ.this[match(aut.by.occ[,1],aut.by.occ.this[,1]),2]))
                   
aut.by.occ.this<-aggregate(wkaut3~occ,data=full.data.set,FUN=mean)
aut.by.occ<-(cbind(aut.by.occ,
        aut.by.occ.this[match(aut.by.occ[,1],aut.by.occ.this[,1]),2]))
                  
aut.by.occ.this<-aggregate(wkaut4~occ,data=full.data.set,FUN=mean)
aut.by.occ<-(cbind(aut.by.occ,
         aut.by.occ.this[match(aut.by.occ[,1],aut.by.occ.this[,1]),2]))
                     
aut.by.occ.this<-aggregate(wkaut5~occ,data=full.data.set,FUN=mean)
aut.by.occ<-(cbind(aut.by.occ,
        aut.by.occ.this[match(aut.by.occ[,1],aut.by.occ.this[,1]),2]))

#what share of employment is the 15 largest occupations?                      
sum(occupatonal.employment[1:15,])/sum(occupatonal.employment[,])

aut.by.occ<-as.data.frame(aut.by.occ)
colnames(aut.by.occ)<-c('Occupation',
                        'Low overall autonomy',
                        'Low autonomy over job tasks',
                        'Low autonomy over work pace',
                        'Low autonomy over work manner',
                        'Low autonomy over task order',
                        'Low autonomy over work hours')
rownames(aut.by.occ)<-aut.by.occ[,1]

plot.table<-subset(aut.by.occ,
             Occupation%in%rownames(occupatonal.employment)[1:15])[,2:7]

plot.table<-plot.table[match(sort(plot.table[,1]),plot.table[,1]),]

#########
#Table 2#
#########
stargazer(plot.table,summary=FALSE)

#######################################
#Test correlation with O*NET
work.context<-read.csv('work_context_2019.CSV')
my.onet.vars<-c('Freedom to Make Decisions',
                'Structured versus Unstructured Work',
                'Pace Determined by Speed of Equipment')
work.context<-subset(work.context,Element.Name%in%my.onet.vars&
                       (Scale.ID=='CX'|Scale.ID=='CT'))

isco88_soc10<-cbind(cw.isco88_soc00[,1],cw.soc00_soc10[match(cw.isco88_soc00[,2],cw.soc00_soc10[,1]),2])

work.context[,1]<-gsub('-','',work.context[,1])
work.context[,1]<-substr(work.context[,1],1,6)
isco88<-as.data.frame(isco88_soc10[match(as.numeric(work.context[,1]),as.numeric(isco88_soc10[,2])),1])
colnames(isco88)<-'isco88'
work.context<-cbind(work.context,isco88)
work.context[,'isco88']<-substr(work.context[,'isco88'],1,3)
str.vs.unstr.isco<-aggregate(Data.Value~isco88,data=subset(work.context,Element.Name=='Freedom to Make Decisions'),FUN=mean)
for (var in my.onet.vars[2:3]){
  str.vs.unstr.isco<-cbind(str.vs.unstr.isco,
    aggregate(Data.Value~isco88,data=subset(work.context,Element.Name==var),FUN=mean))
}
colnames(str.vs.unstr.isco)[seq(from=2,to=6,by=2)]<-my.onet.vars

low.autonomy<-as.data.frame(aut.by.occ[match(str.vs.unstr.isco[,1],aut.by.occ[,1]),2:7])
colnames(low.autonomy)<-c('low_overall_autonomy',
                          'low_autonomy_over_job_tasks',
                          'low_autonomy_over_work_pace',
                          'low_autonomy_over_work_manner',
                          'low_autonomy_over_task_order',
                          'low_autonomy_over_work_hours')
str.vs.unstr.isco<-cbind(str.vs.unstr.isco,low.autonomy)

full.data.set<-cbind(full.data.set,str.vs.unstr.isco[
                     match(full.data.set[,'occ'],
                           str.vs.unstr.isco[,'isco88']),
                     'Freedom to Make Decisions'],
                     str.vs.unstr.isco[
                       match(full.data.set[,'occ'],
                             str.vs.unstr.isco[,'isco88']),
                       'Structured versus Unstructured Work'],
                     str.vs.unstr.isco[
                       match(full.data.set[,'occ'],
                             str.vs.unstr.isco[,'isco88']),
                       'Pace Determined by Speed of Equipment'])
colnames(full.data.set)[(length(colnames(full.data.set))-2):
                          length(colnames(full.data.set))]<-
  c('make_decisions','struc_vs_unstruc','pace_equip')

rank.cor<-matrix(ncol=4,nrow=18)

for (var in my.onet.vars){
  for (var.aut  in c('low_overall_autonomy',
                     'low_autonomy_over_job_tasks',
                     'low_autonomy_over_work_pace',
                     'low_autonomy_over_work_manner',
                     'low_autonomy_over_task_order',
                     'low_autonomy_over_work_hours')){
corr <- cor.test(subset(str.vs.unstr.isco, is.na(low_overall_autonomy)==FALSE)[,var], 
                 subset(str.vs.unstr.isco, is.na(low_overall_autonomy)==FALSE)[,var.aut],
                 method = 'spearman')
row<-6*(match(var,my.onet.vars)-1)+
  match(var.aut,c('low_overall_autonomy',
                  'low_autonomy_over_job_tasks',
                  'low_autonomy_over_work_pace',
                  'low_autonomy_over_work_manner',
                  'low_autonomy_over_task_order',
                  'low_autonomy_over_work_hours'))
rank.cor[row,3]<-round(corr$estimate,3)
rank.cor[row,4]<-round(corr$p.value,3)
rank.cor[row,1]<-var
rank.cor[row,2]<-var.aut
  }
}

rank.cor[,2]<-gsub('_',' ',rank.cor[,2])

colnames(rank.cor)<-c('O*NET variable',
                      'Autonomy variable',
                      'Rank Correlation',
                      'p value')

##########
#Table C1#
##########
stargazer(rank.cor)

#######################################
#Summary statistics
sum.vars<-c('GHQ12_caseness','SF12','autonomy','wkaut1','wkaut2','wkaut3','wkaut4','wkaut5')
sum.tab<-matrix(nrow=length(sum.vars),ncol=5)

for (var in sum.vars){
  
  df<-full.data.set
  colnames(df)[match(var,colnames(df))]<-'var'
  df<-subset(df,var>=0)
  
  sum.tab[match(var,sum.vars),1]<-mean(df[,'var'])
  sum.tab[match(var,sum.vars),2]<-sqrt(var(df[,'var']))
  sum.tab[match(var,sum.vars),3]<-min(df[,'var'])
  sum.tab[match(var,sum.vars),4]<-max(df[,'var'])
  xtsum.object<-XTSUM(df, varname = var, unit=pid)
  sum.tab[match(var,sum.vars),5]<-unlist(xtsum.object$bres[1])

}

rownames(sum.tab)<-sum.vars
colnames(sum.tab)<-c('mean','standard deviation','min','max','within-person standard deviation')

#Create a correlation matrix for components of autonomy
autonomy.components.correlation.matix<-cor(subset(full.data.set,wkaut1>=0&wkaut2>=0&wkaut3>=0&wkaut4>=0&wkaut5>=0)[,
          c('wkaut1','wkaut2','wkaut3','wkaut4','wkaut5')],
    subset(full.data.set,wkaut1>=0&wkaut2>=0&wkaut3>=0&wkaut4>=0&wkaut5>=0)[,
          c('wkaut1','wkaut2','wkaut3','wkaut4','wkaut5')])
colnames(autonomy.components.correlation.matix)<-
  c('Low autonomy over job tasks',
    'Low autonomy over work pace',
    'Low autonomy over work manner',
    'Low autonomy over task order',
    'Low autonomy over work hours')
rownames(autonomy.components.correlation.matix)<-
c('Low autonomy over job tasks',
  'Low autonomy over work pace',
  'Low autonomy over work manner',
  'Low autonomy over task order',
  'Low autonomy over work hours')
#################
#Table 1 Panel B#
#################
stargazer(autonomy.components.correlation.matix)

#################
#Table 1 Panel A#
#################
stargazer(sum.vars)

#######################
#Return sample sizes
nrow(subset(full.data.set,autonomy>=0))
nrow(subset(full.data.set,autonomy>=0&sex==1))
nrow(subset(full.data.set,autonomy>=0&sex==2))
length(unique(subset(full.data.set,autonomy>=0)[,'pid']))

nrow(subset(full.data.set.incl.non.workers,autonomy>=0))
nrow(subset(full.data.set.incl.non.workers,autonomy>=0&sex==1))
nrow(subset(full.data.set.incl.non.workers,autonomy>=0&sex==2))
length(unique(subset(full.data.set.incl.non.workers,autonomy>=0)[,'pid']))

#Return sample sizes
nrow(subset(full.data.set,autonomy>=0&sex==1&GHQ12_caseness>=0&
              lagged_GHQ12_caseness>=0))
nrow(subset(full.data.set,autonomy>=0&sex==1&SF12>=0&
              lagged_SF12>=0))
nrow(subset(full.data.set,autonomy>=0&sex==2&GHQ12_caseness>=0&
              lagged_GHQ12_caseness>=0))
nrow(subset(full.data.set,autonomy>=0&sex==2&SF12>=0&
              lagged_SF12>=0))

nrow(subset(full.data.set,autonomy>=0&sex==1&GHQ12_caseness>=0&
              lagged_GHQ12_caseness>=0&
              education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0))
nrow(subset(full.data.set,autonomy>=0&sex==1&SF12>=0&
              lagged_SF12>=0&education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0))
nrow(subset(full.data.set,autonomy>=0&sex==2&GHQ12_caseness>=0&
              lagged_GHQ12_caseness>=0&education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0))
nrow(subset(full.data.set,autonomy>=0&sex==2&SF12>=0&
              lagged_SF12>=0&education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0))

nrow(subset(full.data.set.incl.non.workers,autonomy>=0&sex==1&GHQ12_caseness>=0&
              lagged_GHQ12_caseness>=0&
              education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0))
nrow(subset(full.data.set.incl.non.workers,autonomy>=0&sex==1&SF12>=0&
              education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0&lagged_SF12>=0))
nrow(subset(full.data.set.incl.non.workers,autonomy>=0&sex==2&GHQ12_caseness>=0&
              education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0&lagged_GHQ12_caseness>=0))
nrow(subset(full.data.set.incl.non.workers,autonomy>=0&sex==2&SF12>=0&
              education>=0&age>=0&marital_stat>=0&lagged_SF12>=0&
              occ>=0&no_children>=0))

nrow(subset(full.data.set.ft,autonomy>=0&sex==1&GHQ12_caseness>=0&
              education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0&lagged_GHQ12_caseness>=0))
nrow(subset(full.data.set.ft,autonomy>=0&sex==1&SF12>=0&
              education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0))
nrow(subset(full.data.set.ft,autonomy>=0&sex==2&GHQ12_caseness>=0&
              education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0))
nrow(subset(full.data.set.ft,autonomy>=0&sex==2&SF12>=0&
              education>=0&age>=0&marital_stat>=0&
              occ>=0&no_children>=0))

nrow(subset(full.data.set.keep.no.pers,autonomy>=0&sex==1&GHQ12_caseness>=0&
              lagged_GHQ12_caseness>=0))
nrow(subset(full.data.set.keep.no.pers,autonomy>=0&sex==1&SF12>=0&
              lagged_SF12>=0))
nrow(subset(full.data.set.keep.no.pers,autonomy>=0&sex==2&GHQ12_caseness>=0&
              lagged_GHQ12_caseness>=0))
nrow(subset(full.data.set.keep.no.pers,autonomy>=0&sex==2&SF12>=0&
              lagged_SF12>=0))

#Calculate the correlation between the caseness and  the SF12
cor(subset(full.data.set,GHQ12_caseness>=0&SF12>=0)[,'GHQ12_caseness'],
subset(full.data.set,GHQ12_caseness>=0&SF12>=0)[,'SF12'])

#######################
#Create a function which runs all of my regression specifications

return_baseline_results<-function(df_this){
  
  res.cond.eff<-matrix(ncol=28*2,nrow=13)

for (s in c(1,2)){
for (var in c('GHQ12_caseness','SF12')){

  mydf<-subset(df_this,sex==s&
           education>=0&
           age>0&
           marital_stat>=0&
           labor_income>=0&
           occ>=0&
           no_children>=0)  
  
  if(var=='GHQ12_caseness'){
    mydf<-subset(mydf,lagged_GHQ12_caseness>=0)
  }
  if(var=='SF12'){
    mydf<-subset(mydf,lagged_SF12>=0)
  }
  
  
controls.list<-c('','factor(age)',
  'factor(age)+factor(marital_stat)+factor(education)',
  'factor(age)+factor(marital_stat)+factor(education)+labor_income',
  'factor(age)+factor(marital_stat)+factor(education)+factor(agreeableness)+factor(conscientiousness)+factor(extraversion)+factor(neuroticism)+factor(openness)',
  'factor(age)+factor(marital_stat)+factor(education)+factor(agreeableness)+factor(conscientiousness)+factor(extraversion)+factor(neuroticism)+factor(openness)+factor(occ)',
  paste('factor(age)+factor(marital_stat)+factor(education)+factor(agreeableness)+factor(conscientiousness)+factor(extraversion)+factor(neuroticism)+factor(openness)+factor(occ)+lagged_',
  var,sep=''),
  paste('factor(age)+factor(marital_stat)+factor(education)+factor(agreeableness)+factor(conscientiousness)+factor(extraversion)+factor(neuroticism)+factor(openness)+factor(occ)+labor_income+lagged_',
        var,sep=''),
  paste('factor(age)+factor(marital_stat)+factor(education)+factor(agreeableness)+factor(conscientiousness)+factor(extraversion)+factor(neuroticism)+factor(openness)+
        factor(occ)+labor_income+(no_children>0)+lagged_',
        var,sep=''))

for (val in seq(from=1,to=9,by=1)){

formula.this<-paste(var,'~',paste('autonomy+',
  controls.list[val],'+factor(wave_number)',sep=''))
csness.reg<-lm(formula.this,data=mydf)
se <- summary(csness.reg,cluster=~pid)$coefficients
res.cond.eff[val,1+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28]<-se[2,1]
res.cond.eff[val,2+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28]<-se[2,2]
res.cond.eff[val,3+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28]<-summary(csness.reg)$r.squared

formula.this<-paste(var,'~',
    paste('wkaut1+wkaut2+wkaut3+wkaut4+wkaut5+',
        controls.list[val],'+factor(wave_number)',sep=''))
csness.reg.disag<-lm(formula.this,data=mydf)
se <- summary(csness.reg.disag,cluster=~pid)$coefficients
res.cond.eff[val,(4+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28):(8+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28)]<-se[2:6,1]
res.cond.eff[val,(9+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28):(13+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28)]<-se[2:6,2]
res.cond.eff[val,14+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28]<-summary(csness.reg.disag)$r.squared

}
}

aut.conts<-c('autonomy','wkaut1+wkaut2+wkaut3+wkaut4+wkaut5')

for (var in c('GHQ12_caseness','SF12')){

  lagged.conts<-c('',paste('+factor(occ)',
      '+factor(education)+factor(marital_stat)+factor(age)+(no_children>0)',
        sep=''))
  
  for (conts in lagged.conts){

  for (aut.cont in aut.conts){  
      
#Now for the remaining ones, I need a plm function
formula<-as.formula(paste(var,'~',aut.cont,conts,'|pid+wave_number',sep=''))
    
plm.caseness.fe<-feols(formula, data=mydf, cluster=~pid)

se<-cbind(plm.caseness.fe$coefficients,plm.caseness.fe$se)

row<-9+match(conts,lagged.conts)

col.shifts<-14*(match(var,c('GHQ12_caseness','SF12'))-1)
var.shift<-(match(aut.cont,aut.conts)-1)*4
se.shift<-1+(aut.cont=='wkaut1+wkaut2+wkaut3+wkaut4+wkaut5')*4

col.first<-1+col.shifts+(match(aut.cont,aut.conts)-1)*3+(s-1)*28
col.last<-col.first+var.shift

df<-mydf

colnames(df)[match(var,colnames(df))]<-'var'

res.cond.eff[row,col.first:col.last]<-as.matrix(se[1:(1+(match(aut.cont,aut.conts)-1)*4),1])
res.cond.eff[row,(se.shift+col.first):(se.shift+col.last)]<-as.matrix(se[1:(1+(match(aut.cont,aut.conts)-1)*4),2])
res.cond.eff[row,(se.shift+col.last+1)]<-1-(var(plm.caseness.fe$residuals))/
  (var(subset(df,var>=0)[,'var']))
res.cond.eff[13,1+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28]<-mean(subset(df,var>=0)[,'var'])

}
  }
  
  for (aut.cont in aut.conts){   
  
  formula<-as.formula(paste(var,'~',aut.cont,conts,'|wave_number',sep=''))
  plm.caseness.fe<-feols(formula, data=mydf, cluster=~pid)
  
  row<-9+match(conts,lagged.conts)
  
  col.shifts<-14*(match(var,c('GHQ12_caseness','SF12'))-1)
  var.shift<-(match(aut.cont,aut.conts)-1)*4
  se.shift<-1+(aut.cont=='wkaut1+wkaut2+wkaut3+wkaut4+wkaut5')*4
  
  col.first<-1+col.shifts+(match(aut.cont,aut.conts)-1)*3+(s-1)*28
  col.last<-col.first+var.shift
  
  df<-mydf
  
  colnames(df)[match(var,colnames(df))]<-'var'
  
  res.cond.eff[12,col.first:col.last]<-
    plm.caseness.fe$coeftable[1:(1+(match(aut.cont,aut.conts)-1)*4),1]
  res.cond.eff[12,(se.shift+col.first):(se.shift+col.last)]<-
    plm.caseness.fe$coeftable[1:(1+(match(aut.cont,aut.conts)-1)*4),2]
  res.cond.eff[12,(se.shift+col.last+1)]<-1-(var(plm.caseness.fe$residuals))/
    (var(subset(df,var>=0)[,'var']))
  
  }
}
}
 
return(res.cond.eff)
   
}

#Get baselines results
res.cond.eff<-return_baseline_results(full.data.set)

###########################################################
#Construct Results Table
##########################################################

construct_results_table<-function(res.cond.eff){
  
  regression.results.table.ghq12<-matrix(ncol=16,nrow=28)
  #for men and women
  for (s in c(1,2)){
    
    #for ghq12 and sf12
    for (dep_var in c(1,2)){
      
      #no controls
      regression.results.table.ghq12[1+14*(s-1),1+8*(dep_var-1)]<-res.cond.eff[1,1+28*(s-1)+14*(dep_var-1)]
      regression.results.table.ghq12[2+14*(s-1),1+8*(dep_var-1)]<-res.cond.eff[1,2+28*(s-1)+14*(dep_var-1)]
      regression.results.table.ghq12[13+14*(s-1),1+8*(dep_var-1)]<-res.cond.eff[1,3+28*(s-1)+14*(dep_var-1)]
      
      #controls no fe
      regression.results.table.ghq12[1+14*(s-1),2+8*(dep_var-1)]<-res.cond.eff[12,1+28*(s-1)+14*(dep_var-1)]
      regression.results.table.ghq12[2+14*(s-1),2+8*(dep_var-1)]<-res.cond.eff[12,2+28*(s-1)+14*(dep_var-1)]
      regression.results.table.ghq12[13+14*(s-1),2+8*(dep_var-1)]<-res.cond.eff[12,3+28*(s-1)+14*(dep_var-1)]
      
      #fixed effects
      regression.results.table.ghq12[1+14*(s-1),3+8*(dep_var-1)]<-res.cond.eff[10,1+28*(s-1)+14*(dep_var-1)]
      regression.results.table.ghq12[2+14*(s-1),3+8*(dep_var-1)]<-res.cond.eff[10,2+28*(s-1)+14*(dep_var-1)]
      regression.results.table.ghq12[13+14*(s-1),3+8*(dep_var-1)]<-res.cond.eff[10,3+28*(s-1)+14*(dep_var-1)]
      
      #fixed effects + controls
      regression.results.table.ghq12[1+14*(s-1),4+8*(dep_var-1)]<-res.cond.eff[11,1+28*(s-1)+14*(dep_var-1)]
      regression.results.table.ghq12[2+14*(s-1),4+8*(dep_var-1)]<-res.cond.eff[11,2+28*(s-1)+14*(dep_var-1)]
      regression.results.table.ghq12[13+14*(s-1),4+8*(dep_var-1)]<-res.cond.eff[11,3+28*(s-1)+14*(dep_var-1)]
      
      #same for wkaut disagg
      for (val in seq(from=1,to=5,by=1)){
        regression.results.table.ghq12[1+2*val+14*(s-1),5+8*(dep_var-1)]<-res.cond.eff[1,3+val+28*(s-1)+14*(dep_var-1)]
        regression.results.table.ghq12[2+2*val+14*(s-1),5+8*(dep_var-1)]<-res.cond.eff[1,8+val+28*(s-1)+14*(dep_var-1)]
        regression.results.table.ghq12[13+14*(s-1),5+8*(dep_var-1)]<-res.cond.eff[1,14+28*(s-1)+14*(dep_var-1)]
      }
      
      for (val in seq(from=1,to=5,by=1)){
        regression.results.table.ghq12[1+2*val+14*(s-1),6+8*(dep_var-1)]<-res.cond.eff[12,3+val+28*(s-1)+14*(dep_var-1)]
        regression.results.table.ghq12[2+2*val+14*(s-1),6+8*(dep_var-1)]<-res.cond.eff[12,8+val+28*(s-1)+14*(dep_var-1)]
        regression.results.table.ghq12[13+14*(s-1),6+8*(dep_var-1)]<-res.cond.eff[12,14+28*(s-1)+14*(dep_var-1)]
      }
      
      #same for wkaut disagg
      for (val in seq(from=1,to=5,by=1)){
        regression.results.table.ghq12[1+2*val+14*(s-1),7+8*(dep_var-1)]<-res.cond.eff[10,3+val+28*(s-1)+14*(dep_var-1)]
        regression.results.table.ghq12[2+2*val+14*(s-1),7+8*(dep_var-1)]<-res.cond.eff[10,8+val+28*(s-1)+14*(dep_var-1)]
        regression.results.table.ghq12[13+14*(s-1),7+8*(dep_var-1)]<-res.cond.eff[10,14+28*(s-1)+14*(dep_var-1)]
      }
      
      #same for wkaut disagg
      for (val in seq(from=1,to=5,by=1)){
        regression.results.table.ghq12[1+2*val+14*(s-1),8+8*(dep_var-1)]<-res.cond.eff[11,3+val+28*(s-1)+14*(dep_var-1)]
        regression.results.table.ghq12[2+2*val+14*(s-1),8+8*(dep_var-1)]<-res.cond.eff[11,8+val+28*(s-1)+14*(dep_var-1)]
        regression.results.table.ghq12[13+14*(s-1),8+8*(dep_var-1)]<-res.cond.eff[11,14+28*(s-1)+14*(dep_var-1)]
      }
    }
  }
  
  #Add in dep var means
  regression.results.table.ghq12[14,1:8]<-res.cond.eff[13,1]
  regression.results.table.ghq12[14,9:16]<-res.cond.eff[13,15]
  regression.results.table.ghq12[28,1:8]<-res.cond.eff[13,29]
  regression.results.table.ghq12[28,9:16]<-res.cond.eff[13,43]
  
  regression.results.table.ghq12<-round(regression.results.table.ghq12,3)
  
  #Add significance stars
  for (val in c(seq(from=1,to=11,by=2),seq(from=15,to=25,by=2))){
    regression.results.table.ghq12[val,]<-
    ifelse(abs(as.numeric(regression.results.table.ghq12[val,])/as.numeric(regression.results.table.ghq12[val+1,]))>1.64&
           abs(as.numeric(regression.results.table.ghq12[val,])/as.numeric(regression.results.table.ghq12[val+1,]))<1.96,
           paste(regression.results.table.ghq12[val,],'*',sep=''),
           ifelse(abs(as.numeric(regression.results.table.ghq12[val,])/as.numeric(regression.results.table.ghq12[val+1,]))>1.96&
                    abs(as.numeric(regression.results.table.ghq12[val,])/as.numeric(regression.results.table.ghq12[val+1,]))<2.56,
                  paste(regression.results.table.ghq12[val,],'**',sep=''),
                  ifelse(abs(as.numeric(regression.results.table.ghq12[val,])/as.numeric(regression.results.table.ghq12[val+1,]))>2.56,
                         paste(regression.results.table.ghq12[val,],'***',sep=''),
                         regression.results.table.ghq12[val,])))
    
  }
  
  #Add brackets to all standard errors
  for (val in c(seq(from=2,to=12,by=2),seq(from=16,to=26,by=2))){
    regression.results.table.ghq12[val,]<-
      ifelse(is.na(regression.results.table.ghq12[val,]),
             ' ',paste('(',regression.results.table.ghq12[val,],')',sep=''))
  }
  
  #remove NAs
  regression.results.table.ghq12<-ifelse(is.na(regression.results.table.ghq12),' ',regression.results.table.ghq12)
  
  return(regression.results.table.ghq12)
}

regression.results.table.ghq12<-construct_results_table(res.cond.eff)

#########
#Table 3#
#########
stargazer(regression.results.table.ghq12[1:14,1:8])
stargazer(regression.results.table.ghq12[1:14,9:16])
#########
#Table 4#
#########
stargazer(regression.results.table.ghq12[15:28,1:8])
stargazer(regression.results.table.ghq12[15:28,9:16])

#Get a matrix of controls for the plots
cont.mat<-matrix(data=0,nrow=11,ncol=10)
colnames(cont.mat)<-c('wave','age','marital status','education',
                      'income','personality',
                      'occupation',
                      'person fe','lagged mh','children')

cont.mat[,1]<-1
cont.mat[2:9,2]<-2
cont.mat[11,2]<-2
cont.mat[3:9,3]<-3
cont.mat[11,3]<-3
cont.mat[3:9,4]<-4
cont.mat[11,4]<-4
cont.mat[4,5]<-5
cont.mat[8:9,5]<-5
cont.mat[10:11,8]<-8
cont.mat[5:9,6]<-6
cont.mat[6:9,7]<-7
cont.mat[11,7]<-7
#cont.mat[11,9]<-9
cont.mat[8,9]<-9
cont.mat[7,9]<-9
cont.mat[9,9]<-9
cont.mat[9,10]<-10
cont.mat[11,10]<-10
################
#Figures 1 to 8#
################
setwd(output.repository)
#Plot out the changing specifications with different controls
setEPS(width=8,height=(16/3))
# naming the eps file
postscript("GHQ12_caseness_cond_assoc_men.eps")

par(mar=c(1,9,1,1),mfrow=c(3,1))

plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,1],xaxt='n',
     type='p',main='Conditional association, GHQ12 caseness and autonomy, men',
     ylab='',
     ylim=c(0,
            1.2*max(res.cond.eff[1:11,1]+1.96*res.cond.eff[1:11,2])))

points(seq(from=1,to=11,by=1),res.cond.eff[1:11,1],type='p')
segments(seq(from=1,to=11,by=1), res.cond.eff[1:11,1]-1.96*res.cond.eff[1:11,2], 
         seq(from=1,to=11,by=1), res.cond.eff[1:11,1]+1.96*res.cond.eff[1:11,2],
         lwd = 1.5)
arrows(seq(from=1,to=11,by=1), res.cond.eff[1:11,1]-1.96*res.cond.eff[1:11,2], 
       seq(from=1,to=11,by=1), res.cond.eff[1:11,1]+1.96*res.cond.eff[1:11,2],
       angle=90,length=0.025,code=3,lwd=1.5)
for (val in seq(from=1,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,
                      length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}
lines(seq(from=0,to=12,by=1),seq(from=0,to=0,length.out=nrow(res.cond.eff)))

plot(seq(from=1,to=11,by=1),cont.mat[,1],type='p',ylim=c(1,10),xaxt='n',yaxt='n',
     xlab='',
     ylab='',las=2)
axis(2, at = seq(from=1,to=10,by=1), colnames(cont.mat),
     las=2)
lines(seq(from=0,to=12,by=1),seq(from=1,to=1,length.out=nrow(res.cond.eff)),lty=2,
      col='grey')
for (val in seq(from=2,to=10,by=1)){
  points(seq(from=1,to=11,by=1),cont.mat[,val])
  lines(seq(from=0,to=12,by=1),seq(from=val,to=val,
  length.out=nrow(res.cond.eff)),lty=2,
  col='grey')
}

plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,3],type='o',xlab='',ylab='R-squared')
for (val in seq(from=1,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,
                      length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}

dev.off()
######
wkaut.list<-c('autonomy over job tasks',
              'autonomy over work pace',
              'autonomy over work manner',
              'autonomy over task order',
              'autonomy over work hours')

setEPS(width=8,height=(16/2))
postscript("GHQ12_caseness_cond_assoc_disagg_men.eps")

par(mar=c(1,9,1,1))
layout(matrix(ncol=1,nrow=8,data=c(1,2,3,4,5,6,6,7)))
for (val in seq(from=1,to=5,by=1)){

plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,3+val],xaxt='n',
     type='p',main=paste('Conditional association between GHQ12 caseness and ',
                         wkaut.list[val],', men',sep=''),
     ylab='',
     ylim=c(min(0,min(res.cond.eff[1:11,3+val]-1.96*res.cond.eff[1:11,8+val])),
            1.2*max(res.cond.eff[1:11,3+val]+1.96*res.cond.eff[1:11,8+val])),
     cex.main=1.1,xaxt='n')

segments(seq(from=1,to=11,by=1),res.cond.eff[1:11,3+val]-1.96*res.cond.eff[1:11,8+val], 
         seq(from=1,to=11,by=1),res.cond.eff[1:11,3+val]+1.96*res.cond.eff[1:11,8+val],
         lwd = 1.5)
arrows(seq(from=1,to=11,by=1), res.cond.eff[1:11,3+val]-1.96*res.cond.eff[1:11,8+val], 
       seq(from=1,to=11,by=1),res.cond.eff[1:11,3+val]+1.96*res.cond.eff[1:11,8+val],
       angle=90,length=0.025,code=3,lwd=1.5)
for (val in seq(from=-10,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,
                                   length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}
lines(seq(from=0,to=12,by=1),seq(from=0,to=0,length.out=nrow(res.cond.eff)))
}

plot(seq(from=1,to=11,by=1),cont.mat[,1],type='p',ylim=c(1,10),xaxt='n',yaxt='n',
     xlab='',
     ylab='',las=2)
axis(2, at = seq(from=1,to=10,by=1), colnames(cont.mat),
     las=2,cex.axis=0.6)
lines(seq(from=0,to=12,by=1),seq(from=1,to=1,length.out=nrow(res.cond.eff)),lty=2,
      col='grey')
for (val in seq(from=2,to=10,by=1)){
  points(seq(from=1,to=11,by=1),cont.mat[,val])
  lines(seq(from=0,to=12,by=1),seq(from=val,to=val,length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}

plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,14],type='o',xlab='',ylab='R-squared')
for (val in seq(from=-10,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,
                                   length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}

dev.off()
######
setEPS(width=8,height=(16/3))
postscript("conditional_association_SF12_autonomy_men.eps")

par(mar=c(1,9,1,1),mfrow=c(3,1))
plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,15],type='p',
     main='SF12 and autonomy, men',
     ylim=c(min(res.cond.eff[1:11,15]-1.96*res.cond.eff[1:11,16]),
            0),ylab='',xaxt='n')
segments(seq(from=1,to=11,by=1), res.cond.eff[1:11,15]-1.96*res.cond.eff[1:11,16], 
         seq(from=1,to=11,by=1), res.cond.eff[1:11,15]+1.96*res.cond.eff[1:11,16],
         lwd = 1.5)
arrows(seq(from=1,to=11,by=1), res.cond.eff[1:11,15]-1.96*res.cond.eff[1:11,16], 
       seq(from=1,to=11,by=1), res.cond.eff[1:11,15]+1.96*res.cond.eff[1:11,16],
       angle=90,length=0.025,code=3,lwd=1.5)
for (val in seq(from=-1,to=-10,by=-1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/2,to=val/2,length.out=nrow(res.cond.eff)),
        col='grey',lty=2)
}
lines(seq(from=0,to=12,by=1),seq(from=0,to=0,length.out=nrow(res.cond.eff)))

plot(seq(from=1,to=11,by=1),cont.mat[,1],type='p',ylim=c(1,10),xaxt='n',yaxt='n',
     xlab='',
     ylab='',las=2)
axis(2, at = seq(from=1,to=10,by=1), colnames(cont.mat),
     las=2)
for (val in seq(from=2,to=10,by=1)){
  points(seq(from=1,to=11,by=1),cont.mat[,val])
  lines(seq(from=0,to=12,by=1),seq(from=val,to=val,length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}
lines(seq(from=0,to=12,by=1),seq(from=1,to=1,length.out=nrow(res.cond.eff)),lty=2,
      col='grey')
plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,17],type='o',xlab='',ylab='R-squared')
for (val in seq(from=1,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=nrow(res.cond.eff)),
        col='grey',lty=2)
}
dev.off()


setEPS(width=8,height=(16/2))
postscript("SF12_cond_assoc_disagg_men.eps")

par(mar=c(1,8,1,1))
layout(matrix(ncol=1,nrow=8,data=c(1,2,3,4,5,6,6,7)))

for (val in seq(from=1,to=5,by=1)){
  plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,17+val],
                     main=paste('Conditional association between SF12 and low ',
                                wkaut.list[val],', men',sep=''),
    cex.main=1.1,
    ylim=c(min(0,min(res.cond.eff[1:11,17+val]-1.96*res.cond.eff[1:11,22+val])),
  1.2*max(0,max(res.cond.eff[1:11,17+val]+1.96*res.cond.eff[1:11,22+val]))),
  ylab='',xaxt='n')

  segments(seq(from=1,to=11,by=1), res.cond.eff[1:11,17+val]-1.96*res.cond.eff[1:11,22+val], 
           seq(from=1,to=11,by=1), res.cond.eff[1:11,17+val]+1.96*res.cond.eff[1:11,22+val],
           lwd = 1.5)
  arrows(seq(from=1,to=11,by=1), res.cond.eff[1:11,17+val]-1.96*res.cond.eff[1:11,22+val], 
         seq(from=1,to=11,by=1), res.cond.eff[1:11,17+val]+1.96*res.cond.eff[1:11,22+val],
         angle=90,length=0.025,code=3,lwd=1.5)
  lines(seq(from=1,to=11,by=1),seq(from=0,to=0,length.out=nrow(res.cond.eff)-2))
  for (val in seq(from=-10,to=10,by=1)){
    lines(seq(from=0,to=12,by=1),seq(from=val/5,to=val/5,length.out=nrow(res.cond.eff)),
          col='grey',lty=2)
  }
  lines(seq(from=0,to=12,by=1),seq(from=0,to=0,length.out=nrow(res.cond.eff)))
}

plot(seq(from=1,to=11,by=1),cont.mat[,1],type='p',ylim=c(1,10),xaxt='n',yaxt='n',
     xlab='',
     ylab='',las=2)
axis(2, at = seq(from=1,to=10,by=1), colnames(cont.mat),
     las=2,cex.axis=0.6)
lines(seq(from=0,to=12,by=1),seq(from=1,to=1,length.out=nrow(res.cond.eff)),lty=2,
      col='grey')
for (val in seq(from=2,to=10,by=1)){
  points(seq(from=1,to=11,by=1),cont.mat[,val])
  lines(seq(from=0,to=12,by=1),seq(from=val,to=val,length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}

plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,14],type='o',xlab='',ylab='R-squared')
for (val in seq(from=0,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=nrow(res.cond.eff)),
        col='grey',lty=2)
}
dev.off()

##########################################################
#Plots for women

setEPS(width=8,height=(16/3))
# naming the eps file
postscript("GHQ12_caseness_cond_assoc_women.eps")

par(mar=c(1,9,1,1),mfrow=c(3,1))
plot(seq(from=1,to=11,by=1),
     res.cond.eff[1:11,1+28],
                   main='Conditional association, GHQ12 caseness and autonomy, women',
                   ylim=c(0,
                          1.2*max(res.cond.eff[1:11,1+28]+1.96*res.cond.eff[1:11,2+28])),
     xaxt='n',type='p',ylab='')
segments(seq(from=1,to=11,by=1), res.cond.eff[1:11,1+28]-1.96*res.cond.eff[1:11,2+28], 
         seq(from=1,to=11,by=1), res.cond.eff[1:11,1+28]+1.96*res.cond.eff[1:11,2+28],
         lwd = 1.5)
arrows(seq(from=1,to=11,by=1), res.cond.eff[1:11,1+28]-1.96*res.cond.eff[1:11,2+28], 
       seq(from=1,to=11,by=1), res.cond.eff[1:11,1+28]+1.96*res.cond.eff[1:11,2+28],
       angle=90,length=0.025,code=3,lwd=1.5)
for (val in seq(from=1,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=nrow(res.cond.eff)),
        lty=2,col='grey')
}
lines(seq(from=0,to=12,by=1),seq(from=0,to=0,length.out=nrow(res.cond.eff)))

plot(seq(from=1,to=11,by=1),cont.mat[1:11,1],type='p',ylim=c(1,10),xaxt='n',yaxt='n',
     xlab='',
     ylab='',las=2)
axis(2, at = seq(from=1,to=10,by=1), colnames(cont.mat),
     las=2)
for (val in seq(from=2,to=10,by=1)){
  points(seq(from=1,to=11,by=1),cont.mat[1:11,val])
  lines(seq(from=0,to=12,by=1),seq(from=val,to=val,
                      length.out=nrow(res.cond.eff)),lty=2,col='grey')
}
lines(seq(from=0,to=12,by=1),seq(from=1,to=1,length.out=nrow(res.cond.eff)),lty=2,
      col='grey')

plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,3+28],type='o',xlab='',ylab='R-squared')
for (val in seq(from=1,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=nrow(res.cond.eff)),
        lty=2,col='grey')
}
dev.off()

setEPS(width=8,height=(16/2))
postscript("GHQ12_caseness_cond_assoc_disagg_women.eps")

par(mar=c(1,9,1,1))
layout(matrix(ncol=1,nrow=8,data=c(1,2,3,4,5,6,6,7)))
for (val in seq(from=1,to=5,by=1)){
  plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,3+val+28],
         main=paste('Conditional association between GHQ12 caseness and ',
                      wkaut.list[val],', women',sep=''),
           ylim=c(min(0,min(res.cond.eff[1:11,3+val+28]-1.96*res.cond.eff[1:11,8+val+28])),
                1.2*max(res.cond.eff[1:11,3+val+28]+1.96*res.cond.eff[1:11,8+val+28])),
           cex.main=1.1,
       ylab='',xaxt='n')
  segments(seq(from=1,to=11,by=1),res.cond.eff[1:11,3+val+28]-1.96*res.cond.eff[1:11,8+val+28], 
           seq(from=1,to=11,by=1),res.cond.eff[1:11,3+val+28]+1.96*res.cond.eff[1:11,8+val+28],
           lwd = 1.5)
  arrows(seq(from=1,to=11,by=1),res.cond.eff[1:11,3+val+28]-1.96*res.cond.eff[1:11,8+val+28], 
         seq(from=1,to=11,by=1),res.cond.eff[1:11,3+val+28]+1.96*res.cond.eff[1:11,8+val+28],
         angle=90,length=0.025,code=3,lwd=1.5)
  for (val in seq(from=-10,to=10,by=1)){
    lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=nrow(res.cond.eff)),
          col='grey',lty=2)  
  }
  lines(seq(from=0,to=12,by=1),seq(from=0,to=0,length.out=nrow(res.cond.eff)))
}
plot(seq(from=1,to=11,by=1),cont.mat[,1],type='p',ylim=c(1,10),xaxt='n',yaxt='n',
     xlab='',
     ylab='',las=2)
axis(2, at = seq(from=1,to=10,by=1), colnames(cont.mat),
     las=2,cex.axis=0.6)
lines(seq(from=1,to=11,by=1),seq(from=1,to=1,length.out=nrow(res.cond.eff)-2),lty=2,
      col='grey')
for (val in seq(from=2,to=10,by=1)){
  points(seq(from=1,to=11,by=1),cont.mat[,val])
  lines(seq(from=0,to=12,by=1),seq(from=val,to=val,length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}

plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,14+28],type='o',xlab='',ylab='R-squared')
for (val in seq(from=1,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}

dev.off()
######
setEPS(width=8,height=(16/3))
postscript("conditional_association_SF12_autonomy_women.eps")

par(mar=c(1,9,1,1),mfrow=c(3,1))
plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,15+28],
                   main='SF12 and autonomy, women',
                   ylim=c(min(res.cond.eff[1:11,15+28]-1.96*res.cond.eff[1:11,16+28]),
                          0),ylab='',xaxt='n')
segments(seq(from=1,to=11,by=1), res.cond.eff[1:11,15+28]-1.96*res.cond.eff[1:11,16+28], 
         seq(from=1,to=11,by=1), res.cond.eff[1:11,15+28]+1.96*res.cond.eff[1:11,16+28],
         lwd = 1.5)
arrows(seq(from=1,to=11,by=1), res.cond.eff[1:11,15+28]-1.96*res.cond.eff[1:11,16+28], 
       seq(from=1,to=11,by=1), res.cond.eff[1:11,15+28]+1.96*res.cond.eff[1:11,16+28],
       angle=90,length=0.025,code=3,lwd=1.5)
for (val in seq(from=1,to=-20,by=-1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=nrow(res.cond.eff)),
        col='grey',lty=2)  
}
lines(seq(from=0,to=12,by=1),seq(from=0,to=0,length.out=nrow(res.cond.eff)))

plot(seq(from=1,to=11,by=1),cont.mat[,1],type='p',ylim=c(1,10),xaxt='n',yaxt='n',
     xlab='',
     ylab='',las=2)
axis(2, at = seq(from=1,to=10,by=1), colnames(cont.mat),
     las=2)
lines(seq(from=0,to=12,by=1),seq(from=1,to=1,length.out=nrow(res.cond.eff)),lty=2,
      col='grey')
for (val in seq(from=2,to=10,by=1)){
  points(seq(from=1,to=11,by=1),cont.mat[,val])
  lines(seq(from=0,to=12,by=1),seq(from=val,to=val,length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}

plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,17+28],type='o',xlab='',ylab='R-squared')
for (val in seq(from=1,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}

dev.off()


setEPS(width=8,height=(16/2))
postscript("SF12_cond_assoc_disagg_women.eps")

par(mar=c(1,8,1,1))
layout(matrix(ncol=1,nrow=8,data=c(1,2,3,4,5,6,6,7)))
for (val in seq(from=1,to=5,by=1)){
  plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,17+val+28],
  main=paste('Conditional association between SF12 and low ',
                      wkaut.list[val],', women',sep=''),
                  cex.main=1.1,ylab='',
 ylim=c(min(0,min(res.cond.eff[1:11,17+val+28]-1.96*res.cond.eff[1:11,22+val+28])),
  1.2*max(0,max(res.cond.eff[1:11,17+val+28]+1.96*res.cond.eff[1:11,22+val+28]))),
 type='p',xaxt='n')
  segments(seq(from=1,to=11,by=1), res.cond.eff[1:11,17+val+28]-1.96*res.cond.eff[1:11,22+val+28], 
           seq(from=1,to=11,by=1), res.cond.eff[1:11,17+val+28]+1.96*res.cond.eff[1:11,22+val+28],
           lwd = 1.5)
  arrows(seq(from=1,to=11,by=1), res.cond.eff[1:11,17+val+28]-1.96*res.cond.eff[1:11,22+val+28], 
         seq(from=1,to=11,by=1), res.cond.eff[1:11,17+val+28]+1.96*res.cond.eff[1:11,22+val+28],
         angle=90,length=0.025,code=3,lwd=1.5)
  for (val in seq(from=-10,to=10,by=1)){
    lines(seq(from=0,to=12,by=1),seq(from=val/5,to=val/5,length.out=nrow(res.cond.eff)),
          col='grey',lty=2)
  }
  lines(seq(from=0,to=12,by=1),seq(from=0,to=0,length.out=nrow(res.cond.eff)))
}
plot(seq(from=1,to=11,by=1),cont.mat[,1],type='p',ylim=c(1,10),xaxt='n',yaxt='n',
     xlab='',
     ylab='',las=2)
axis(2, at = seq(from=1,to=10,by=1), colnames(cont.mat),
     las=2,cex.axis=0.6)
lines(seq(from=0,to=12,by=1),seq(from=1,to=1,length.out=nrow(res.cond.eff)),lty=2,
      col='grey')
for (val in seq(from=2,to=10,by=1)){
  points(seq(from=1,to=11,by=1),cont.mat[,val])
  lines(seq(from=0,to=12,by=1),seq(from=val,to=val,length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}

plot(seq(from=1,to=11,by=1),res.cond.eff[1:11,14+28],type='o',xlab='',ylab='R-squared')
for (val in seq(from=1,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=nrow(res.cond.eff)),lty=2,
        col='grey')
}

dev.off()

setEPS(width=8,height=(16/3))
postscript("conditional_association_SF12_autonomy_women.eps")

par(mar=c(1,9,1,1),mfrow=c(3,1))
plot(seq(from=1,to=11,by=1),res.cond.eff[,15+28],
     main='SF12 and autonomy, women',
     ylim=c(min(res.cond.eff[,15+28]-1.96*res.cond.eff[,16+28]),
            0),ylab='',xaxt='n')
segments(seq(from=1,to=11,by=1), res.cond.eff[,15+28]-1.96*res.cond.eff[,16+28], 
         seq(from=1,to=11,by=1), res.cond.eff[,15+28]+1.96*res.cond.eff[,16+28],
         lwd = 1.5)
arrows(seq(from=1,to=11,by=1), res.cond.eff[,15+28]-1.96*res.cond.eff[,16+28], 
       seq(from=1,to=11,by=1), res.cond.eff[,15+28]+1.96*res.cond.eff[,16+28],
       angle=90,length=0.025,code=3,lwd=1.5)
for (val in seq(from=1,to=-20,by=-1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=2+nrow(res.cond.eff)),
        col='grey',lty=2)  
}
lines(seq(from=0,to=12,by=1),seq(from=0,to=0,length.out=2+nrow(res.cond.eff)))

plot(seq(from=1,to=11,by=1),cont.mat[,1],type='p',ylim=c(1,10),xaxt='n',yaxt='n',
     xlab='',
     ylab='',las=2)
axis(2, at = seq(from=1,to=10,by=1), colnames(cont.mat),
     las=2)
lines(seq(from=0,to=12,by=1),seq(from=1,to=1,length.out=2+nrow(res.cond.eff)),lty=2,
      col='grey')
for (val in seq(from=2,to=10,by=1)){
  points(seq(from=1,to=11,by=1),cont.mat[,val])
  lines(seq(from=0,to=12,by=1),seq(from=val,to=val,length.out=2+nrow(res.cond.eff)),lty=2,
        col='grey')
}

plot(seq(from=1,to=11,by=1),res.cond.eff[,17+28],type='o',xlab='',ylab='R-squared')
for (val in seq(from=1,to=10,by=1)){
  lines(seq(from=0,to=12,by=1),seq(from=val/10,to=val/10,length.out=2+nrow(res.cond.eff)),lty=2,
        col='grey')
}

dev.off()

##########################################################
#Mechanisms: does the relationship change after 2019?
after.2019.results.table<-matrix(nrow=7,ncol=4)

  for (s in c(1,2)){
    for (var in c('GHQ12_caseness','SF12')){
      
      mydf<-subset(full.data.set,sex==s&
                     education>=0&
                     age>0&
                     marital_stat>=0&
                     labor_income>=0&
                     occ>=0&
                     no_children>=0)  
      
      if(var=='GHQ12_caseness'){
        mydf<-subset(mydf,lagged_GHQ12_caseness>=0)
      }
      if(var=='SF12'){
        mydf<-subset(mydf,lagged_SF12>=0)
      }
      
    aut.conts<-c('autonomy','wkaut1+wkaut2+wkaut3+wkaut4+wkaut5')
      
      lagged.conts<-c('',paste('+factor(occ)',
                               '+factor(education)+factor(marital_stat)+factor(age)+(no_children>0)',
                               sep=''))
      
      conts<-lagged.conts[2]
        
      aut.cont<-aut.conts[1] 
          
          formula<-as.formula(paste(var,'~',aut.cont,'*(year>2019)',conts,'|pid+wave_number',sep=''))
          
          plm.caseness.fe<-feols(formula, data=mydf, cluster=~pid)
          
          after.2019.results.table[1:2,match(var,c('GHQ12_caseness','SF12'))+2*(s-1)]<-plm.caseness.fe$coeftable[1,1:2]
          
          after.2019.results.table[3:4,match(var,c('GHQ12_caseness','SF12'))+2*(s-1)]<-plm.caseness.fe$coeftable['autonomy:year > 2019TRUE',1:2]
          
          df<-mydf
          
          colnames(df)[match(var,colnames(df))]<-'var'
          
          after.2019.results.table[5,match(var,c('GHQ12_caseness','SF12'))+2*(s-1)]<-1-(var(plm.caseness.fe$residuals))/
            (var(subset(df,var>=0)[,'var']))
          
          after.2019.results.table[6,match(var,c('GHQ12_caseness','SF12'))+2*(s-1)]<-mean(subset(df,var>=0)[,'var'])
          
          after.2019.results.table[7,match(var,c('GHQ12_caseness','SF12'))+2*(s-1)]<-nrow(subset(df,var>=0&autonomy>=0))
          
        
  }
}

##########
#Table 10#
##########
stargazer(after.2019.results.table)

autonomy.names<-c('low autonomy','low task autonomy','low pace autonomy ',
                  'low manner autonomy','low order autonomy','low hours autonomy')
mh.list<-c('GHQ12 caseness','SF12')

###########################################################
#Do the Oster correction
#First question: what is the implied "delta" on the 
#last controls:

mh.list<-c('GHQ12_caseness','SF12')

#Estimate the deltas implied by existing controls
implied.deltas<-matrix(nrow=24,ncol=2)

for (mh in c(1,2)){
  for (s in c(1,2)){

    formula<-paste(mh.list[mh],
      '~autonomy+factor(age)+factor(education)+factor(marital_stat)+factor(occ)+factor(wave_number)',sep='')
  
    plm.obj<-
      plm(formula,index='pid',
          data=subset(full.data.set,sex==s))
    
    implied.deltas[6*(s-1)+12*(mh-1)+1,1]<-
      cov(predict(plm.obj)-
            coef(plm.obj)['autonomy']*plm.obj$model[,'autonomy'],
          coef(plm.obj)['autonomy']*plm.obj$model[,'autonomy'])
    
    implied.deltas[6*(s-1)+12*(mh-1)+1,2]<-
    cov(coef(plm.obj)[1]*plm.obj$model[,1],
        coef(plm.obj)['autonomy']*plm.obj$model[,'autonomy'])
    
    formula<-paste(mh.list[mh],'~wkaut1+wkaut2+wkaut3+
                   wkaut4+wkaut5+factor(age)+
            factor(education)+
            factor(marital_stat)+factor(occ)+
            factor(wave_number)+(no_children>0)',
                   sep='')
    
    plm.obj<-
      plm(formula,index='pid',
          data=subset(full.data.set,sex==s))
    
    
    for (w in seq(from=1,to=5,by=1)){
    
      implied.deltas[6*(s-1)+12*(mh-1)+1+w,1]<-
      cov(predict(plm.obj)-
      coef(plm.obj)[paste('wkaut',w,sep='')]*plm.obj$model[,paste('wkaut',w,sep='')],
      coef(plm.obj)[paste('wkaut',w,sep='')]*plm.obj$model[,paste('wkaut',w,sep='')])
    
    implied.deltas[6*(s-1)+12*(mh-1)+1+w,2]<-
      cov(coef(plm.obj)[1]*plm.obj$model[,1],
      coef(plm.obj)[paste('wkaut',w,sep='')]*plm.obj$model[,paste('wkaut',w,sep='')])
    
      
    }
  }
}


###########################################################
#Main Oster corrections:
wkaut.controls<-matrix(ncol=5,nrow=4)
wkaut.controls[,1]<-seq(from=2,to=5,by=1)
wkaut.controls[,2]<-c(1,seq(from=3,to=5,by=1))
wkaut.controls[,3]<-c(seq(from=1,to=2,by=1),seq(from=4,to=5,by=1))
wkaut.controls[,4]<-c(seq(from=1,to=3,by=1),5)
wkaut.controls[,5]<-seq(from=1,to=4,by=1)

res.mat.ghq12<-matrix(ncol=12,nrow=68)
res.mat.SF12<-matrix(ncol=12,nrow=68)
res.mat.ghq12.rsq<-matrix(ncol=4,nrow=68)
res.mat.SF12.rsq<-matrix(ncol=4,nrow=68)

delta.list<-c(seq(from=-1.3,to=-0.9,by=0.025),
              seq(from=0.9,to=1.3,by=0.025))

#Function for running the Oster correction
run_oster_correction<-function(delta,R_max,R_short,sigma_y,beta_basic,
                               beta_short,sigma_X,tau_x,R_basic){
  
  d<-delta*((R_max-R_short)*sigma_y)*(beta_basic-beta_short)*sigma_X
  c<-delta*((R_max-R_short)*sigma_y)*(sigma_X-tau_x)-((R_short-R_basic)*sigma_y)*tau_x-
    sigma_X*tau_x*(beta_basic-beta_short)^2
  b<-tau_x*(beta_basic-beta_short)*sigma_X*(delta-2)
  a<-(delta-1)*(tau_x*sigma_X-tau_x^2)
  
  sol.this<-polyroot(z=c(d,c,b,a))
  #ditch imaginary numbers
  sol.this<-as.numeric(sol.this[(as.numeric(sol.this)^2)==as.numeric((sol.this)^2)])
  #If I have too many left, ditch based on sign:
  sol.this<-ifelse(length(sol.this)>1,
                   sol.this[sign(beta_short-beta_basic)*(-as.numeric(sol.this))>0],
                   sol.this)
  
  corrected.coefficient<-beta_short-
    as.numeric(sol.this[match(min(abs(as.numeric(sol.this))),
                              abs(as.numeric(sol.this)))])
  
  return(corrected.coefficient)
  
}

#Test code: does this reproduce the asymptote in Masten and p?
oster.tests<-c()
for (delta in seq(from=0.5,to=3,by=0.0001)){
  oster.tests<-c(oster.tests,run_oster_correction(delta,0.8,0.6,1,1.5,1,1.5,1.3,0.4))
}

plot(seq(from=0.5,to=3,by=0.0001),oster.tests,type='l')
lines(c(0,25000),c(0,0))
#A: yes

#Loop over every option and calculate the Oster-corrected variables
for (s in c(1,2)){

for (delta in delta.list){
  
  #and for each max r.sq ratio
  for (rsq.ratio in seq(from=1.1,to=1.3,by=0.2)){
    
    #get the row
    row<-(match(delta,delta.list)-1)*2+
      match(rsq.ratio,seq(from=1.1,to=1.3,by=0.2))
    
    #######################################
    #Run short reg and basic reg
    sigma_y<-var(subset(full.data.set,GHQ12_caseness>=0&sex==s)[,'GHQ12_caseness'])
    sigma_X<-var(subset(full.data.set,autonomy>=0&sex==s)[,'autonomy'])
    reg.x<-plm(autonomy~factor(education)+factor(occ)+
                 factor(marital_stat)+(no_children>0)+
                 factor(wave_number),
               effect='individual',
               index=c('pid'),data=subset(full.data.set,sex==s))
    tau_x<-var(reg.x$residuals)
    R_short<-res.cond.eff[11,3+(s-1)*28]
    R_basic<-res.cond.eff[1,3+(s-1)*28]
    beta_short<-res.cond.eff[11,1+(s-1)*28]
    beta_basic<-res.cond.eff[1,1+(s-1)*28]
    R_max<-min(res.cond.eff[11,3+(s-1)*28]*rsq.ratio,1)
    res.mat.ghq12.rsq[row,1+2*(s-1)]<-R_max
    
    res.mat.ghq12[row,1+6*(s-1)]<-
    run_oster_correction(delta,R_max,R_short,sigma_y,beta_basic,
                         beta_short,sigma_X,tau_x)
    
    #######################################
    #same for SF12
    sigma_y<-var(subset(full.data.set,SF12>=0&sex==s)[,'SF12'])
    sigma_X<-var(subset(full.data.set,autonomy>=0&sex==s)[,'autonomy'])
    reg.x<-plm(autonomy~factor(education)+factor(occ)+
                 factor(marital_stat)+(no_children>0)+factor(wave_number),
               effect='individual',
               index=c('pid'),data=subset(full.data.set,sex==s))
    tau_x<-var(reg.x$residuals)
    R_short<-res.cond.eff[11,17+(s-1)*28]
    R_basic<-res.cond.eff[1,17+(s-1)*28]
    beta_short<-res.cond.eff[11,15+(s-1)*28]
    beta_basic<-res.cond.eff[1,15+(s-1)*28]
    R_max<-min(R_short*rsq.ratio,1)
    res.mat.SF12.rsq[row,1+2*(s-1)]<-R_max

    res.mat.SF12[row,1+6*(s-1)]<-
      run_oster_correction(delta,R_max,R_short,sigma_y,beta_basic,
                                   beta_short,sigma_X,tau_x)
    
    ###################################
    #wkaut individually
    sigma_y<-var(subset(full.data.set,GHQ12_caseness>=0&sex==s)[,'GHQ12_caseness'])
    R_short<-res.cond.eff[11,14+(s-1)*28]
    R_basic<-res.cond.eff[1,14+(s-1)*28]
    R_max<-min(R_short*rsq.ratio,1)
    res.mat.ghq12.rsq[row,2+2*(s-1)]<-R_max
    
    #for each autonomy variable
    count.to.five<-seq(from=1,to=5,by=1)
    for (val in count.to.five){
      
      sigma_X<-var(subset(full.data.set,autonomy>=0&sex==s)[,paste('wkaut',val,sep='')])
      reg.x<-plm(paste('wkaut',val,'~','wkaut',wkaut.controls[1,val],
                       '+wkaut',wkaut.controls[2,val],
                       '+wkaut',wkaut.controls[3,val],
                       '+wkaut',wkaut.controls[4,val],
                       '+factor(education)+factor(occ)+',
                       'factor(marital_stat)+(no_children>0)+factor(wave_number)',
                       sep=''), effect='individual',
                 index=c('pid'),data=subset(full.data.set,sex==s))
      tau_x<-var(reg.x$residuals)
      beta_short<-res.cond.eff[11,3+val+(s-1)*28]
      beta_basic<-res.cond.eff[1,3+val+(s-1)*28]
      
      
      res.mat.ghq12[row,1+val+6*(s-1)]<-
        run_oster_correction(delta,R_max,R_short,sigma_y,beta_basic,
                             beta_short,sigma_X,tau_x)
        
      
    }
    
    ###################################
    #wkaut individually for SF12
    sigma_y<-var(subset(full.data.set,SF12>=0&sex==s)[,'SF12'])
    R_short<-res.cond.eff[11,28+(s-1)*28]
    R_basic<-res.cond.eff[1,28+(s-1)*28]
    R_max<-min(R_short*rsq.ratio,1)
    res.mat.SF12.rsq[row,2]<-R_max
    
    #for each autonomy variable
    count.to.five<-seq(from=1,to=5,by=1)
    for (val in count.to.five){
      
      sigma_X<-var(subset(full.data.set,autonomy>=0&sex==s)[,paste('wkaut',val,sep='')])
      reg.x<-plm(paste('wkaut',val,'~','wkaut',wkaut.controls[1,val],
                       '+wkaut',wkaut.controls[2,val],
                       '+wkaut',wkaut.controls[3,val],
                       '+wkaut',wkaut.controls[4,val],
                       '+factor(education)+factor(occ)+',
                       'factor(marital_stat)+(no_children>0)+factor(wave_number)',
                       sep=''), effect='individual',
                 index=c('pid'),data=subset(full.data.set,sex==s))
      tau_x<-var(reg.x$residuals)
      beta_short<-res.cond.eff[11,17+val+(s-1)*28]
      beta_basic<-res.cond.eff[1,17+val+(s-1)*28]
      
      res.mat.SF12[row,1+val+6*(s-1)]<-
        run_oster_correction(delta,R_max,R_short,sigma_y,beta_basic,
                             beta_short,sigma_X,tau_x)
      
    }
  }
}
}

############################
#Read in the standard errors
setwd(data.repository)
oster.res.ghq12<-read.csv('res_mat_ghq12_draws.CSV')
oster.res.SF12<-read.csv('res_mat_sf12_draws.CSV')
oster.res.ghq12.disagg<-read.csv('res_mat_ghq12_draws_disagg.CSV')
oster.res.SF12.disagg<-read.csv('res_mat_sf12_draws_disagg.CSV')

oster.res.ghq12<-oster.res.ghq12[,2:length(oster.res.ghq12)]
oster.res.SF12<-oster.res.SF12[,2:length(oster.res.SF12)]
oster.res.ghq12.disagg<-oster.res.ghq12.disagg[,2:length(oster.res.ghq12.disagg)]
oster.res.SF12.disagg<-oster.res.SF12.disagg[,2:length(oster.res.SF12.disagg)]

standard.errors.tab.men<-matrix(nrow=12,ncol=8)
standard.errors.tab.men[,1]<-c(-1.3,-1.3,-1.1,-1.1,-0.9,-0.9,0.9,0.9,1.1,1.1,1.3,1.3)
standard.errors.tab.men[,2]<-c(1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3)
standard.errors.tab.sf12.men<-matrix(nrow=12,ncol=8)
standard.errors.tab.sf12.men[,1]<-c(-1.3,-1.3,-1.1,-1.1,-0.9,-0.9,0.9,0.9,1.1,1.1,1.3,1.3)
standard.errors.tab.sf12.men[,2]<-c(1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3)
standard.errors.tab.women<-matrix(nrow=12,ncol=8)
standard.errors.tab.women[,1]<-c(-1.3,-1.3,-1.1,-1.1,-0.9,-0.9,0.9,0.9,1.1,1.1,1.3,1.3)
standard.errors.tab.women[,2]<-c(1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3)
standard.errors.tab.sf12.women<-matrix(nrow=12,ncol=8)
standard.errors.tab.sf12.women[,1]<-c(-1.3,-1.3,-1.1,-1.1,-0.9,-0.9,0.9,0.9,1.1,1.1,1.3,1.3)
standard.errors.tab.sf12.women[,2]<-c(1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3)
res.mat.ghq12.red.men<-matrix(nrow=12,ncol=8)
res.mat.ghq12.red.women<-matrix(nrow=12,ncol=8)
res.mat.sf12.red.men<-matrix(nrow=12,ncol=8)
res.mat.sf12.red.women<-matrix(nrow=12,ncol=8)
res.mat.ghq12.red.men[,1]<-c(-1.3,-1.3,-1.1,-1.1,-0.9,-0.9,0.9,0.9,1.1,1.1,1.3,1.3)
res.mat.ghq12.red.men[,2]<-c(1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3)
res.mat.ghq12.red.women[,1]<-c(-1.3,-1.3,-1.1,-1.1,-0.9,-0.9,0.9,0.9,1.1,1.1,1.3,1.3)
res.mat.ghq12.red.women[,2]<-c(1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3)
res.mat.sf12.red.men[,1]<-c(-1.3,-1.3,-1.1,-1.1,-0.9,-0.9,0.9,0.9,1.1,1.1,1.3,1.3)
res.mat.sf12.red.men[,2]<-c(1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3)
res.mat.sf12.red.women[,1]<-c(-1.3,-1.3,-1.1,-1.1,-0.9,-0.9,0.9,0.9,1.1,1.1,1.3,1.3)
res.mat.sf12.red.women[,2]<-c(1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3, 1.1, 1.3)

#for each delta and each r-sq ratio, fill out the point estimates and standard errors
for (delta in c(-1.3,-1.1,-0.9,0.9,1.1,1.3)){
 for (rsq.ratio in c(1.1,1.3)){
  res<-1
   
   row.no.point.est<-(match(delta,delta.list)-1)*2+
                      match(rsq.ratio,seq(from=1.1,to=1.3,by=0.2))
   col.no.se.res<-match(interaction(delta,rsq.ratio),
                         interaction(standard.errors.tab.men[,1],standard.errors.tab.men[,2]))
   col.no.se.calc<-match(delta,c(-1.3,-1.1,-0.9,0.9,1.1,1.3))+6*(match(rsq.ratio,seq(from=1.1,to=1.3,by=0.2))-1)
   
  res.mat.ghq12.red.men[col.no.se.res,res+2]<-res.mat.ghq12[row.no.point.est,res]
  res.mat.ghq12.red.women[col.no.se.res,res+2]<-res.mat.ghq12[row.no.point.est,res+6]
  res.mat.sf12.red.men[col.no.se.res,res+2]<-res.mat.SF12[row.no.point.est,res]
  res.mat.sf12.red.women[col.no.se.res,res+2]<-res.mat.SF12[row.no.point.est,res+6]
  
  standard.errors.tab.men[col.no.se.res,res+2]<-sqrt(var(oster.res.ghq12[,col.no.se.calc]))
  standard.errors.tab.women[col.no.se.res,res+2]<-sqrt(var(oster.res.ghq12[,col.no.se.calc+12]))
  standard.errors.tab.sf12.men[col.no.se.res,res+2]<-sqrt(var(oster.res.SF12[,col.no.se.calc]))
  standard.errors.tab.sf12.women[col.no.se.res,res+2]<-sqrt(var(oster.res.SF12[,col.no.se.calc+12]))
  
   for (res in seq(from=2,to=6,by=1)){ 
     
  res.mat.ghq12.red.men[col.no.se.res,res+2]<-res.mat.ghq12[row.no.point.est,res]
  res.mat.ghq12.red.women[col.no.se.res,res+2]<-res.mat.ghq12[row.no.point.est,res+6]
  res.mat.sf12.red.men[col.no.se.res,res+2]<-res.mat.SF12[row.no.point.est,res]
  res.mat.sf12.red.women[col.no.se.res,res+2]<-res.mat.SF12[row.no.point.est,res+6]
  
  standard.errors.tab.men[col.no.se.res,res+2]<-sqrt(var(oster.res.ghq12.disagg[,col.no.se.calc+12*(res-2)]))
  standard.errors.tab.women[col.no.se.res,res+2]<-sqrt(var(oster.res.ghq12.disagg[,60+col.no.se.calc+12*(res-2)]))
  standard.errors.tab.sf12.men[col.no.se.res,res+2]<-sqrt(var(oster.res.SF12.disagg[,col.no.se.calc+12*(res-2)]))
  standard.errors.tab.sf12.women[col.no.se.res,res+2]<-sqrt(var(oster.res.SF12.disagg[,60+col.no.se.calc+12*(res-2)]))
     
   }
 } 
}  

#stack into results table to produce the tables
my_tab_generate<-function(res.mat,standard.errors.tab){

results.table.for.calcs<-rbind(res.mat[1,],
                     standard.errors.tab[1,]
)
for (val in seq(from=2,to=nrow(res.mat))){
  results.table.for.calcs<-rbind(results.table.for.calcs,
                                 res.mat[val,],
                                 standard.errors.tab[val,])
}
results.table<-results.table.for.calcs
colnames(results.table)<-c('delta','r-squared ratio','autonomy',
                         'job tasks', 'work pace', 'work manner', 'task order', 'work hours')

results.table[1:24,3:8]<-round(results.table[1:24,3:8],3)

for (val in seq(from=1,to=23,by=2)){
  results.table[val,3:8]<-
  ifelse(abs(results.table.for.calcs[val,3:8]/results.table.for.calcs[val+1,3:8])>2.58,
         paste(results.table[val,3:8],'***',sep=''),
  ifelse(abs(results.table.for.calcs[val,3:8]/results.table.for.calcs[val+1,3:8])>1.96,
         paste(results.table[val,3:8],'**',sep=''),
  ifelse(abs(results.table.for.calcs[val,3:8]/results.table.for.calcs[val+1,3:8])>1.645,
         paste(results.table[val,3:8],'*',sep=''),
         results.table[val,3:8])))
  results.table[val+1,3:8]<-paste('(',results.table[val+1,3:8],')',sep='')
  
  results.table[val+1,1:2]<-paste('')
}

stargazer(results.table)
}

###############
#Tables 5 to 8#
###############

my_tab_generate(res.mat.ghq12.red.men,standard.errors.tab.men)
my_tab_generate(res.mat.ghq12.red.women,standard.errors.tab.women)
my_tab_generate(res.mat.sf12.red.men,standard.errors.tab.sf12.men)
my_tab_generate(res.mat.sf12.red.women,standard.errors.tab.sf12.women)

################################
#Figure 9
setEPS(width=10,height=13)
postscript("oster_bounds_chart.eps")
par(mfrow=c(2,2),mar=c(4,4,2,2))
#GHQ12 men
maxes<-c()
mins<-c()
max.conf.int<-c()
min.conf.int<-c()
for (i in 1:6){
  maxes<-c(maxes,max(res.mat.ghq12.red.men[,2+i]))
  mins<-c(mins,min(res.mat.ghq12.red.men[,2+i]))
  max.conf.int<-c(max.conf.int,
                  max(res.mat.ghq12.red.men[,2+i]+
                    1.96*standard.errors.tab.men[,2+i]))
  min.conf.int<-c(min.conf.int,
                  min(res.mat.ghq12.red.men[,2+i]-
                        1.96*standard.errors.tab.men[,2+i]))
}

plot(seq(from=1,to=6,by=1),
     y=res.mat.ghq12.red.men[3,3:8],
     ylim=c(min(min.conf.int),
            max(max.conf.int)),pch=1,
     xlim=c(0.5,6.5),
     xlab=' ',ylab=' ',
     main='GHQ12, men',xaxt='n')
points(res.mat.ghq12.red.men[10,3:8],pch=2,col=3)
lines(c(0,7),c(0,0))
arrows(seq(from=1,to=6,by=1),
       maxes,
       seq(from=1,to=6,by=1), 
         mins,
         code=1,angle=90,col=2)
arrows(seq(from=1,to=6,by=1),
       mins,
       seq(from=1,to=6,by=1),
       maxes,
       code=1,angle=90,col=2)
segments(seq(from=1,to=6,by=1),
         min.conf.int,
         seq(from=1,to=6,by=1),
         max.conf.int)
legend('topleft',pch=c(1,2),col=c(1,3),
       legend=c('delta=-1.1, R-sq ratio = 1.3',
                'delta=1.1, R-sq ratio = 1.3'))
axis(1,at=seq(from=1,to=6,by=1),
     labels=c('overall','tasks',
              'pace','manner','order',
              'hours'))
#SF12 men
maxes<-c()
mins<-c()
max.conf.int<-c()
min.conf.int<-c()
for (i in 1:6){
  maxes<-c(maxes,max(res.mat.sf12.red.men[,2+i]))
  mins<-c(mins,min(res.mat.sf12.red.men[,2+i]))
  max.conf.int<-c(max.conf.int,
                  max(res.mat.sf12.red.men[,2+i]+
                        1.96*standard.errors.tab.sf12.men[,2+i]))
  min.conf.int<-c(min.conf.int,
                  min(res.mat.sf12.red.men[,2+i]-
                        1.96*standard.errors.tab.sf12.men[,2+i]))
}

plot(seq(from=1,to=6,by=1),
     y=res.mat.sf12.red.men[3,3:8],
     ylim=c(min(min.conf.int),
            max(max.conf.int)),pch=1,
     xlab=' ',ylab=' ',
     xlim=c(0.5,6.5),
     main='SF12, men',xaxt='n')
points(res.mat.sf12.red.men[10,3:8],pch=2,col=3)
lines(c(0,7),c(0,0))
arrows(seq(from=1,to=6,by=1),
       maxes,
       seq(from=1,to=6,by=1), 
       mins,
       code=1,angle=90,col=2)
arrows(seq(from=1,to=6,by=1),
       mins,
       seq(from=1,to=6,by=1),
       maxes,
       code=1,angle=90,col=2)
segments(seq(from=1,to=6,by=1),
         min.conf.int,
         seq(from=1,to=6,by=1),
         max.conf.int)
axis(1,at=seq(from=1,to=6,by=1),
     labels=c('overall','tasks',
              'pace','manner','order',
              'hours'))
#GHQ12 women
maxes<-c()
mins<-c()
max.conf.int<-c()
min.conf.int<-c()
for (i in 1:6){
  maxes<-c(maxes,max(res.mat.ghq12.red.women[,2+i]))
  mins<-c(mins,min(res.mat.ghq12.red.women[,2+i]))
  max.conf.int<-c(max.conf.int,
                  max(res.mat.ghq12.red.women[,2+i]+
                        1.96*standard.errors.tab.women[,2+i]))
  min.conf.int<-c(min.conf.int,
                  min(res.mat.ghq12.red.women[,2+i]-
                        1.96*standard.errors.tab.women[,2+i]))
}

plot(seq(from=1,to=6,by=1),
     y=res.mat.ghq12.red.women[3,3:8],
     ylim=c(min(min.conf.int),
            max(max.conf.int)),pch=1,
     xlim=c(0.5,6.5),
     xlab=' ',ylab=' ',
     main='GHQ12, women',xaxt='n')
points(res.mat.ghq12.red.women[10,3:8],pch=2,col=3)
lines(c(0,7),c(0,0))
arrows(seq(from=1,to=6,by=1),
       maxes,
       seq(from=1,to=6,by=1), 
       mins,
       code=1,angle=90,col=2)
arrows(seq(from=1,to=6,by=1),
       mins,
       seq(from=1,to=6,by=1),
       maxes,
       code=1,angle=90,col=2)
segments(seq(from=1,to=6,by=1),
         min.conf.int,
         seq(from=1,to=6,by=1),
         max.conf.int)
axis(1,at=seq(from=1,to=6,by=1),
     labels=c('overall','tasks',
              'pace','manner','order',
              'hours'))
#SF12 women
maxes<-c()
mins<-c()
max.conf.int<-c()
min.conf.int<-c()
for (i in 1:6){
  maxes<-c(maxes,max(res.mat.sf12.red.women[,2+i]))
  mins<-c(mins,min(res.mat.sf12.red.women[,2+i]))
  max.conf.int<-c(max.conf.int,
                  max(res.mat.sf12.red.women[,2+i]+
                        1.96*standard.errors.tab.sf12.women[,2+i]))
  min.conf.int<-c(min.conf.int,
                  min(res.mat.sf12.red.women[,2+i]-
                        1.96*standard.errors.tab.sf12.women[,2+i]))
}

plot(seq(from=1,to=6,by=1),
     y=res.mat.sf12.red.women[3,3:8],xlim=c(0.5,6.5),
     ylim=c(min(min.conf.int),
            max(max.conf.int)),pch=1,
     xlab=' ',ylab=' ',
     main='SF12, women',xaxt='n')
points(res.mat.sf12.red.women[10,3:8],pch=2,col=3)
lines(c(0,7),c(0,0))
arrows(seq(from=1,to=6,by=1),
       maxes,
       seq(from=1,to=6,by=1), 
       mins,
       code=1,angle=90,col=2)
arrows(seq(from=1,to=6,by=1),
       mins,
       seq(from=1,to=6,by=1),
       maxes,
       code=1,angle=90,col=2)
segments(seq(from=1,to=6,by=1),
         min.conf.int,
         seq(from=1,to=6,by=1),
         max.conf.int)
axis(1,at=seq(from=1,to=6,by=1),
     labels=c('overall','tasks',
              'pace','manner','order',
              'hours'))
dev.off()


########################################
#more robustness tests: exclude all those who ever change jobs
ever.change.jobs<-unique(subset(full.data.set,same_employer==2)[,'pid'])
df.robust<-subset(full.data.set,pid%in%ever.change.jobs==FALSE)

cond.assoc.always.work.ghq12.men<-plm(GHQ12_caseness~
                     autonomy+factor(wave_number)+
                       factor(occ)+
                       factor(education)+factor(marital_stat)+
                       (no_children>0),
                   effect='individual',
                   index=c('pid'),data=subset(df.robust,sex==1))

se.ghq12.men<-coeftest(cond.assoc.always.work.ghq12.men,
             vcovHC(cond.assoc.always.work.ghq12.men, type = 'HC0',cluster='group'))
se.ghq12.men[1,]

cond.assoc.always.work.sf12.men<-plm(SF12~
                                    autonomy+
                        factor(wave_number)+factor(occ)+
                          factor(education)+factor(marital_stat)+
                          (no_children>0),
                                  effect='individual',
                                  index=c('pid'),data=subset(df.robust,sex==1))

se.sf12.men<-coeftest(cond.assoc.always.work.sf12.men,
                   vcovHC(cond.assoc.always.work.sf12.men, type = 'HC0',cluster='group'))
se.sf12.men[1,]

cond.assoc.always.work.ghq12.women<-plm(GHQ12_caseness~
                      autonomy+factor(wave_number)+
                        factor(occ)+
                        factor(education)+factor(marital_stat)+
                        (no_children>0),
                                      effect='individual',
                                      index=c('pid'),data=subset(df.robust,sex==2))

se.ghq12.women<-coeftest(cond.assoc.always.work.ghq12.women,
                       vcovHC(cond.assoc.always.work.ghq12.women, type = 'HC0',cluster='group'))
se.ghq12.women[1,]

cond.assoc.always.work.sf12.women<-plm(SF12~
                                       autonomy+
                                         factor(wave_number)+
                                         factor(occ)+
                                         factor(education)+factor(marital_stat)+
                                         (no_children>0),
                                     effect='individual',
                                     index=c('pid'),data=subset(df.robust,sex==2))

se.sf12.women<-coeftest(cond.assoc.always.work.sf12.women,
                      vcovHC(cond.assoc.always.work.sf12.women, type = 'HC0',cluster='group'))
se.sf12.women[1,]

##########
#Table 14#
##########
stargazer(cond.assoc.always.work.ghq12.men,
          cond.assoc.always.work.sf12.men,
          keep='autonomy')
stargazer(cond.assoc.always.work.ghq12.women,
          cond.assoc.always.work.sf12.women,
          keep='autonomy')

#manually calculate R-squared
1-var(cond.assoc.always.work.ghq12.men$residuals)/
  var(subset(df.robust,sex==1&GHQ12_caseness>=0&
               lagged_GHQ12_caseness>=0)[,'GHQ12_caseness'])
1-var(cond.assoc.always.work.sf12.men$residuals)/
  var(subset(df.robust,sex==1&SF12>=0&
               lagged_SF12)[,'SF12'])
1-var(cond.assoc.always.work.ghq12.women$residuals)/
  var(subset(df.robust,sex==2&GHQ12_caseness>=0&
               lagged_GHQ12_caseness)[,'GHQ12_caseness'])
1-var(cond.assoc.always.work.sf12.women$residuals)/
  var(subset(df.robust,sex==2&SF12>=0&
               lagged_SF12)[,'SF12'])
########################################
#More robustness tests. Change up preferred autonomy index
robust.aut.est<-matrix(nrow=4,ncol=8)
robust.aut.se<-matrix(nrow=4,ncol=8)

for (s in c(1,2)){
for (val in c(1,2,4,5)){
  
  df.this<-subset(full.data.set,sex==s)
  
  aut.this<-as.data.frame(as.numeric((subset(full.data.set,sex==s)[,'wkaut1']+
                          subset(full.data.set,sex==s)[,'wkaut2']+
                          subset(full.data.set,sex==s)[,'wkaut3']+
                          subset(full.data.set,sex==s)[,'wkaut4']+
                          subset(full.data.set,sex==s)[,'wkaut5'])>=val)
  )
  colnames(aut.this)<-'aut_this'
  
  df.this<-cbind(df.this,aut.this)
  
  reg<-lm(GHQ12_caseness~aut_this,data=subset(df.this,sex==s&
                                                lagged_GHQ12_caseness>=0))
  
  robust.aut.est[match(val,c(1,2,4,5)),1+4*(s-1)]<-coef(reg)[2]
  robust.aut.se[match(val,c(1,2,4,5)),1+4*(s-1)]<-
    coeftest(reg, vcov = vcovCL, cluster = ~pid)[2,2]
  
  plm.caseness.fe<-feols(GHQ12_caseness~aut_this+
                factor(education)+factor(occ)+
                factor(marital_stat)+(no_children>0)|pid+wave_number, 
                data=subset(df.this,sex==s),cluster=~pid)
  
  robust.aut.est[match(val,c(1,2,4,5)),2+4*(s-1)]<-coef(plm.caseness.fe)[1]
  robust.aut.se[match(val,c(1,2,4,5)),2+4*(s-1)]<-plm.caseness.fe$se[1]
  
  
  reg<-lm(SF12~aut_this,data=subset(df.this,sex==s&
                                      lagged_SF12>=0))
  
  robust.aut.est[match(val,c(1,2,4,5)),3+4*(s-1)]<-coef(reg)[2]
  robust.aut.se[match(val,c(1,2,4,5)),3+4*(s-1)]<-
    coeftest(reg, vcov = vcovCL, cluster = ~pid)[2,2]
  
  plm.caseness.fe<-feols(SF12~aut_this+
                           factor(education)+factor(occ)+
                           factor(marital_stat)+
                           (no_children>0)|pid+wave_number, 
                         data=subset(df.this,sex==s),cluster=~pid)
  
  robust.aut.est[match(val,c(1,2,4,5)),4+4*(s-1)]<-coef(plm.caseness.fe)[1]
  robust.aut.se[match(val,c(1,2,4,5)),4+4*(s-1)]<-plm.caseness.fe$se[1]
  
  
}
}
  
robustness.test.matrix<-rbind(robust.aut.est[1,],
                              robust.aut.se[1,],
                              robust.aut.est[2,],
                              robust.aut.se[2,],
                              robust.aut.est[3,],
                              robust.aut.se[3,],
                              robust.aut.est[4,],
                              robust.aut.se[4,])

##################
#Tables E1 and E2#
##################
stargazer(robustness.test.matrix[,1:2])
stargazer(robustness.test.matrix[,3:4])
stargazer(robustness.test.matrix[,5:6])
stargazer(robustness.test.matrix[,7:8])

##############
#Event study for low autonomy treatment:
#combinations men and women, SF12 and GHQ12 caseness, treatment=low autonomy, treatment=high autonomy:
#comb = 2 x 2 x 2 =8
#subset by people who are ever untreated
event.study.res<-matrix(ncol=8,nrow=11)
event.study.se<-matrix(ncol=8,nrow=8)

ever.untreat<-unique(subset(full.data.set,autonomy==0)[,'pid'])
ever.treat<-unique(subset(full.data.set,autonomy==1)[,'pid'])
people.whose.autonomy.changes<-ever.treat[ever.treat%in%ever.untreat]

autonomy.dec<-as.data.frame(full.data.set[,'autonomy']-
              full.data.set[,'lagged_autonomy']>0)
autonomy.inc<-as.data.frame(full.data.set[,'autonomy']-
                              full.data.set[,'lagged_autonomy']<0)
colnames(autonomy.dec)<-'autonomy_decrease'
colnames(autonomy.inc)<-'autonomy_increase'
full.data.set<-cbind(full.data.set,autonomy.dec,autonomy.inc)

changes.by.person<-cbind(aggregate(autonomy_decrease~pid,data=full.data.set,FUN=sum),
                         aggregate(autonomy_increase~pid,data=full.data.set,FUN=sum))

sum(changes.by.person[,2])
sum(changes.by.person[,4])
#How many experience an increase in autonomy?
sum(changes.by.person[,4]>0)
#for how many of those people is the increase in autonomy the only change
sum(changes.by.person[,4]==1&changes.by.person[,2]==0)

#How many experience a decrease in autonomy?
sum(changes.by.person[,2]>0)
#for how many of those people is the increase in autonomy the only change
sum(changes.by.person[,4]==0&changes.by.person[,2]==1)

observation.numbers<-matrix(nrow=4,ncol=5)

for (s in c(1,2)){
  for (treat in c(0,1)){
    
    ever.untreat<-unique(subset(full.data.set,autonomy==(1-treat))[,'pid'])
    ever.treat<-unique(subset(full.data.set,autonomy==treat)[,'pid'])
    #last 3 lines of subset new since thesis revision
    event.study.df<-subset(full.data.set,pid%in%ever.untreat&wave_number%in%c(2,4,6,8,10)&
                             sex==s
                             #&GHQ12_caseness>=0&SF12>0&
                             #lagged_GHQ12_caseness>=0&lagged_SF12>0
                             #&pid%in%ever.untreat&pid%in%ever.treat
                             )
    
    observation.numbers[s+treat*2,1]<-length(ever.untreat)
    
    #get when treatment events
    treatment<-event.study.df[,'autonomy']==treat&
      event.study.df[match(interaction(event.study.df[,'pid'],event.study.df[,'wave_number']-2),
                           interaction(event.study.df[,'pid'],event.study.df[,'wave_number'])),
                     'autonomy']==(1-treat)
    treatment<-as.data.frame(treatment)
    colnames(treatment)<-'treated'
    event.study.df<-cbind(event.study.df,treatment)
    
    #find out how many times people are treated then remove people who are treated twice
    no.times.treated<-as.matrix(table(subset(event.study.df,treated==1)[,'pid']))
    double.treated<-rownames(no.times.treated)[no.times.treated==2]
    event.study.df<-subset(event.study.df,(pid%in%double.treated)==FALSE)
    
    observation.numbers[s+treat*2,2]<-length(unique(event.study.df[,'pid']))
    
    #get time since treatment for everyone
    treatment.time<-cbind(subset(event.study.df,treated==1)[,'pid'],subset(event.study.df,treated==1)[,'wave_number'])
    time.since.treatment<-as.data.frame(ifelse(is.na(event.study.df[,'wave_number']-
                                 treatment.time[match(event.study.df[,'pid'],treatment.time[,1]),2]),
                                               -100,
                                       event.study.df[,'wave_number']-
                                         treatment.time[match(event.study.df[,'pid'],treatment.time[,1]),2]))
    #time.since.treatment<-as.data.frame(event.study.df[,'wave_number']-treatment.time[match(event.study.df[,'pid'],treatment.time[,1]),2])
    colnames(time.since.treatment)<-'time_since_treatment'
    treatment.time<-as.data.frame(treatment.time[match(event.study.df[,'pid'],treatment.time[,1]),2])
    colnames(treatment.time)<-'treatment_time'
    event.study.df<-cbind(event.study.df,time.since.treatment,treatment.time)
    
    #find out any non-absorbing states, and remove them.
    nonaborbed.pids<-unique(subset(event.study.df,autonomy==(1-treat)&time_since_treatment>0)[,'pid'])
    event.study.df<-subset(event.study.df,(pid%in%nonaborbed.pids)==FALSE)
    
    observation.numbers[s+treat*2,3]<-length(nonaborbed.pids)
    observation.numbers[s+treat*2,4]<-length(unique(event.study.df[,'pid']))
    
    #fill in the observation numbers
    observation.numbers[s+treat*2,5]<-nrow(event.study.df)
    
    #unique(event.study.df[,'time_since_treatment'])
    #now run the TWFE event study
    twfe.es.ghq12<-plm(GHQ12_caseness~(time_since_treatment==(-100))+
                         (time_since_treatment==(-8))+
                         (time_since_treatment==(-4))+
                         (time_since_treatment==(-6))+
                         (time_since_treatment==(0))+
                         (time_since_treatment==(2))+
                         (time_since_treatment==(4))+
                         (time_since_treatment==(6))+
                         factor(wave_number)+factor(occ)+factor(marital_stat)+
                         factor(education)+factor(age)+
                         (no_children>0),
                         data=subset(event.study.df,lagged_GHQ12_caseness>=0),
                       index='pid')
    twfe.es.ghq12.se<-vcov(twfe.es.ghq12,cluster='pid')
    twfe.es.sf12<-plm(SF12~(time_since_treatment==(-100))+
                        (time_since_treatment==(-8))+
                        (time_since_treatment==(-4))+
                        (time_since_treatment==(-6))+
                        (time_since_treatment==(0))+
                        (time_since_treatment==(2))+
                        (time_since_treatment==(4))+
                        (time_since_treatment==(6))+
                        factor(wave_number)+factor(occ)+factor(marital_stat)+
                        factor(education)+factor(age)+
                        (no_children),
                      data=subset(event.study.df,lagged_SF12>=0),
                      index='pid')
    twfe.es.sf12.se<-vcov(twfe.es.sf12,cluster='pid')
    
    
    hyo.test<-linearHypothesis(twfe.es.ghq12, c("time_since_treatment == (2)TRUE=0",
                                                'time_since_treatment == (4)TRUE=0',
                                                "time_since_treatment == (6)TRUE=0"), 
                               white.adjust = "hc1")
    
    event.study.res[9,1+(s-1)*2+treat*4]<-as.numeric(hyo.test$`Pr(>Chisq)`[2])
    
    
    hyo.test<-linearHypothesis(twfe.es.sf12, c("time_since_treatment == (2)TRUE=0",
                                               'time_since_treatment == (4)TRUE=0',
                                               "time_since_treatment == (6)TRUE=0"), 
                               white.adjust = "hc1")
    
    event.study.res[9,2+(s-1)*2+treat*4]<-as.numeric(hyo.test$`Pr(>Chisq)`[2])
    
    #test pre-trends
    hyo.test<-linearHypothesis(twfe.es.ghq12, c("time_since_treatment == (-8)TRUE=0",
                                      'time_since_treatment == (-4)TRUE=0',
                                      "time_since_treatment == (-6)TRUE=0"), 
                     white.adjust = "hc1")
    
    event.study.res[10,1+(s-1)*2+treat*4]<-as.numeric(hyo.test$`Pr(>Chisq)`[2])
    
    
    hyo.test<-linearHypothesis(twfe.es.sf12, c("time_since_treatment == (-8)TRUE=0",
                                     'time_since_treatment == (-4)TRUE=0',
                                     "time_since_treatment == (-6)TRUE=0"), 
                     white.adjust = "hc1")
    
    event.study.res[10,2+(s-1)*2+treat*4]<-as.numeric(hyo.test$`Pr(>Chisq)`[2])
    
    event.study.res[1:8,1+(s-1)*2+treat*4]<-c(coef(twfe.es.ghq12)[1:3],0,coef(twfe.es.ghq12)[4:7])
    event.study.res[1:8,2+(s-1)*2+treat*4]<-c(coef(twfe.es.sf12)[1:3],0,coef(twfe.es.sf12)[4:7])
    event.study.res[11,1+(s-1)*2+treat*4]<-length(twfe.es.ghq12$residuals)
    event.study.res[11,2+(s-1)*2+treat*4]<-length(twfe.es.sf12$residuals)
    
    event.study.se[1:8,1+(s-1)*2+treat*4]<-c(sqrt(diag(twfe.es.ghq12.se))[1:3],0,
                                             sqrt(diag(twfe.es.ghq12.se))[4:7])
    event.study.se[1:8,2+(s-1)*2+treat*4]<-c(sqrt(diag(twfe.es.sf12.se))[1:3],0,
                                             sqrt(diag(twfe.es.sf12.se))[4:7])
    
  }
}

sex.treat.list<-c('men, treatment = high autonomy','women, treatment = high autonomy',
                  'men, treatment = low autonomy','women, treatment = low autonomy')

###########
#Figure I1#
###########
setEPS(width=8,height=8)
postscript("event_studies_new.eps")
par(mfrow=c(4,2),mar=c(4,3,3,2))

for (val in seq(from=1,to=4,by=1)){
plot(seq(from=-8,to=6,by=2),
     event.study.res[1:8,1+2*(val-1)],type='o',
     ylim=c(min(event.study.res[1:8,1+2*(val-1)]-
                  1.96*event.study.se[1:8,1+2*(val-1)]),
            max(event.study.res[1:8,1+2*(val-1)]+
                  1.96*event.study.se[1:8,1+2*(val-1)])),xlab='Time relative to change',
     ylab=' ',
     main=paste('GHQ12 caseness, ',sex.treat.list[val],paste=''))
lines(seq(from=-8,to=6,by=2),seq(from=0,to=0,length.out=8))
segments(seq(from=-8,to=6,by=2),event.study.res[1:8,1+2*(val-1)]+
           1.96*event.study.se[1:8,1+2*(val-1)], 
         seq(from=-8,to=6,by=2), event.study.res[1:8,1+2*(val-1)]-
           1.96*event.study.se[1:8,1+2*(val-1)],
         lwd = 1.5,col=1)
arrows(seq(from=-8,to=6,by=2),event.study.res[1:8,1+2*(val-1)]+
         1.96*event.study.se[1:8,1+2*(val-1)], 
       seq(from=-8,to=6,by=2), event.study.res[1:8,1+2*(val-1)]-
         1.96*event.study.se[1:8,1+2*(val-1)],
       angle=90,length=0.025,code=3,lwd=1.5,col=1)

plot(seq(from=-8,to=6,by=2),
     event.study.res[1:8,2+2*(val-1)],type='o',
     ylim=c(min(event.study.res[1:8,2+2*(val-1)]-
                  1.96*event.study.se[1:8,2+2*(val-1)]),
            max(event.study.res[1:8,2+2*(val-1)]+
                  1.96*event.study.se[1:8,2+2*(val-1)])),xlab='time relative to change',
     ylab=' ',
     main=paste('SF12, ',sex.treat.list[val],paste=''))
lines(seq(from=-8,to=6,by=2),seq(from=0,to=0,length.out=8))
segments(seq(from=-8,to=6,by=2),event.study.res[1:8,2+2*(val-1)]+
           1.96*event.study.se[1:8,2+2*(val-1)], 
         seq(from=-8,to=6,by=2), event.study.res[1:8,2+2*(val-1)]-
           1.96*event.study.se[1:8,2+2*(val-1)],
         lwd = 1.5,col=1)
arrows(seq(from=-8,to=6,by=2),event.study.res[1:8,2+2*(val-1)]+
         1.96*event.study.se[1:8,2+2*(val-1)], 
       seq(from=-8,to=6,by=2), event.study.res[1:8,2+2*(val-1)]-
         1.96*event.study.se[1:8,2+2*(val-1)],
       angle=90,length=0.025,code=3,lwd=1.5,col=1)

}

dev.off()

####################################################
#Mediating effect of personality
####################################################
#personality exploration
pers.results<-matrix(nrow=15,ncol=4)

#re-centre all personality measures
for (pers in c('agreeableness','neuroticism',
               'conscientiousness','openness','extraversion')){
full.data.set[,pers]<-full.data.set[,pers]-median(full.data.set[,pers])

}

wkaut5.ghq12.women<-feols(GHQ12_caseness~
                wkaut5*((agreeableness)+(extraversion)+
                      (neuroticism)+(conscientiousness)+
                        (openness))+
                  wkaut1*(factor(agreeableness)+factor(extraversion)+
                             factor(neuroticism)+factor(conscientiousness)+
                             factor(openness))+
                wkaut2*(factor(agreeableness)+factor(extraversion)+
                           factor(neuroticism)+factor(conscientiousness)+
                           factor(openness))+
                  wkaut3*(factor(agreeableness)+factor(extraversion)+
                             factor(neuroticism)+factor(conscientiousness)+
                             factor(openness))+
                  wkaut4*(factor(agreeableness)+factor(extraversion)+
                             factor(neuroticism)+factor(conscientiousness)+
                             factor(openness))+
                  factor(occ)+
                  factor(education)+factor(marital_stat)+
                  (no_children>0)|pid+wave_number,
              data=subset(full.data.set,
                  lagged_GHQ12_caseness>=0&
                    sex==2),
              cluster=~pid)

women.results<-rbind(
wkaut5.ghq12.women$coeftable['wkaut5',],
wkaut5.ghq12.women$coeftable['wkaut5:agreeableness',],
wkaut5.ghq12.women$coeftable['wkaut5:extraversion',],
wkaut5.ghq12.women$coeftable['wkaut5:neuroticism',],
wkaut5.ghq12.women$coeftable['wkaut5:openness',],
wkaut5.ghq12.women$coeftable['wkaut5:conscientiousness',])

pers.results[seq(from=1,to=11,by=2),3]<-women.results[,1]
pers.results[seq(from=2,to=12,by=2),3]<-women.results[,2]
pers.results[13,3]<-nrow(subset(full.data.set,GHQ12_caseness>=0&autonomy>=0&
                                                      lagged_GHQ12_caseness>=0&
                                                      sex==2))
pers.results[14,3]<-1-var(wkaut5.ghq12.women$residuals)/var(subset(full.data.set,GHQ12_caseness>=0&
                           lagged_GHQ12_caseness>=0&autonomy>=0&
                             sex==2)[,'GHQ12_caseness'])
pers.results[15,3]<-women.results[,2]<-mean(subset(full.data.set,GHQ12_caseness>=0&
                                                lagged_GHQ12_caseness>=0&autonomy>=0&
                                                sex==2)[,'GHQ12_caseness'])


wkaut5.ghq12.men<-feols(GHQ12_caseness~
                            wkaut5*((agreeableness)+(extraversion)+
                                      (neuroticism)+(conscientiousness)+
                                      (openness))+
                            wkaut1*(factor(agreeableness)+factor(extraversion)+
                                      factor(neuroticism)+factor(conscientiousness)+
                                      factor(openness))+
                            wkaut2*(factor(agreeableness)+factor(extraversion)+
                                      factor(neuroticism)+factor(conscientiousness)+
                                      factor(openness))+
                            wkaut3*(factor(agreeableness)+factor(extraversion)+
                                      factor(neuroticism)+factor(conscientiousness)+
                                      factor(openness))+
                            wkaut4*(factor(agreeableness)+factor(extraversion)+
                                      factor(neuroticism)+factor(conscientiousness)+
                                      factor(openness))+
                            factor(occ)+
                            factor(education)+factor(marital_stat)+
                            (no_children>0)|pid+wave_number,
                          data=subset(full.data.set,
                                      lagged_GHQ12_caseness>=0&
                                        sex==1),
                          cluster=~pid)

men.results<-rbind(
wkaut5.ghq12.men$coeftable['wkaut5',],
wkaut5.ghq12.men$coeftable['wkaut5:agreeableness',],
wkaut5.ghq12.men$coeftable['wkaut5:extraversion',],
wkaut5.ghq12.men$coeftable['wkaut5:neuroticism',],
wkaut5.ghq12.men$coeftable['wkaut5:openness',],
wkaut5.ghq12.men$coeftable['wkaut5:conscientiousness',])

pers.results[seq(from=1,to=11,by=2),1]<-men.results[,1]
pers.results[seq(from=2,to=12,by=2),1]<-men.results[,2]
pers.results[13,1]<-nrow(subset(full.data.set,GHQ12_caseness>=0&
                                  lagged_GHQ12_caseness>=0&autonomy>=0&
                                  sex==1))
pers.results[14,1]<-1-var(wkaut5.ghq12.men$residuals)/var(subset(full.data.set,GHQ12_caseness>=0&
                                                                     lagged_GHQ12_caseness>=0&autonomy>=0&
                                                                     sex==1)[,'GHQ12_caseness'])
pers.results[15,1]<-women.results[,2]<-mean(subset(full.data.set,GHQ12_caseness>=0&autonomy>=0&
                                                     lagged_GHQ12_caseness>=0&
                                                     sex==1)[,'GHQ12_caseness'])

men.and.women.assoc.contr.ghq12<-cbind(c(men.results[1,1],
                                         men.results[2,1]*mean(subset(full.data.set,
                                                                      lagged_GHQ12_caseness>=0&
                                                                        sex==1)[,'agreeableness']),
                                         men.results[3,1]*mean(subset(full.data.set,
                                                                      lagged_GHQ12_caseness>=0&
                                                                        sex==1)[,'extraversion']),
                                         men.results[4,1]*mean(subset(full.data.set,
                                                                      lagged_GHQ12_caseness>=0&
                                                                        sex==1)[,'neuroticism']),
                                         men.results[5,1]*mean(subset(full.data.set,
                                                                      lagged_GHQ12_caseness>=0&
                                                                        sex==1)[,'openness']),
                                         men.results[6,1]*mean(subset(full.data.set,
                                                                      lagged_GHQ12_caseness>=0&
                                                                        sex==1)[,'conscientiousness'])),
                                       c(women.results[1,1],
                                         women.results[2,1]*mean(subset(full.data.set,
                                                                        lagged_GHQ12_caseness>=0&
                                                                          sex==2)[,'agreeableness']),
                                         women.results[3,1]*mean(subset(full.data.set,
                                                                        lagged_GHQ12_caseness>=0&
                                                                          sex==2)[,'extraversion']),
                                         women.results[4,1]*mean(subset(full.data.set,
                                                                        lagged_GHQ12_caseness>=0&
                                                                          sex==2)[,'neuroticism']),
                                         women.results[5,1]*mean(subset(full.data.set,
                                                                        lagged_GHQ12_caseness>=0&
                                                                          sex==2)[,'openness']),
                                         women.results[6,1]*mean(subset(full.data.set,
                                                                        lagged_GHQ12_caseness>=0&
                                                                          sex==2)[,'conscientiousness'])))

wkaut5.ghq12.women<-feols(SF12~
                            wkaut5*((agreeableness)+(extraversion)+
                                      (neuroticism)+(conscientiousness)+
                                      (openness))+
                            wkaut1*(factor(agreeableness)+factor(extraversion)+
                                      factor(neuroticism)+factor(conscientiousness)+
                                      factor(openness))+
                            wkaut2*(factor(agreeableness)+factor(extraversion)+
                                      factor(neuroticism)+factor(conscientiousness)+
                                      factor(openness))+
                            wkaut3*(factor(agreeableness)+factor(extraversion)+
                                      factor(neuroticism)+factor(conscientiousness)+
                                      factor(openness))+
                            wkaut4*(factor(agreeableness)+factor(extraversion)+
                                      factor(neuroticism)+factor(conscientiousness)+
                                      factor(openness))+
                            factor(occ)+
                            factor(education)+factor(marital_stat)+
                            (no_children>0)|pid+wave_number,
                          data=subset(full.data.set,
                                      lagged_SF12>=0&
                                        sex==2),
                          cluster=~pid)

women.results<-rbind(
  wkaut5.ghq12.women$coeftable['wkaut5',],
  wkaut5.ghq12.women$coeftable['wkaut5:agreeableness',],
  wkaut5.ghq12.women$coeftable['wkaut5:extraversion',],
  wkaut5.ghq12.women$coeftable['wkaut5:neuroticism',],
  wkaut5.ghq12.women$coeftable['wkaut5:openness',],
  wkaut5.ghq12.women$coeftable['wkaut5:conscientiousness',])

pers.results[seq(from=1,to=11,by=2),4]<-women.results[,1]
pers.results[seq(from=2,to=12,by=2),4]<-women.results[,2]
pers.results[13,4]<-nrow(subset(full.data.set,SF12>=0&
                                  lagged_SF12>=0&autonomy>=0&
                                  sex==2))
pers.results[14,4]<-1-var(wkaut5.ghq12.women$residuals)/var(subset(full.data.set,SF12>=0&
                                                                     lagged_SF12>=0&autonomy>=0&
                                                                   sex==2)[,'SF12'])
pers.results[15,4]<-women.results[,2]<-mean(subset(full.data.set,SF12>=0&
                                                     lagged_SF12>=0&autonomy>=0&
                                                     sex==2)[,'SF12'])

wkaut5.ghq12.men<-feols(SF12~
                          wkaut5*((agreeableness)+(extraversion)+
                                    (neuroticism)+(conscientiousness)+
                                    (openness))+
                          wkaut1*(factor(agreeableness)+factor(extraversion)+
                                    factor(neuroticism)+factor(conscientiousness)+
                                    factor(openness))+
                          wkaut2*(factor(agreeableness)+factor(extraversion)+
                                    factor(neuroticism)+factor(conscientiousness)+
                                    factor(openness))+
                          wkaut3*(factor(agreeableness)+factor(extraversion)+
                                    factor(neuroticism)+factor(conscientiousness)+
                                    factor(openness))+
                          wkaut4*(factor(agreeableness)+factor(extraversion)+
                                    factor(neuroticism)+factor(conscientiousness)+
                                    factor(openness))+
                          factor(occ)+
                          factor(education)+factor(marital_stat)+
                          (no_children>0)|pid+wave_number,
                        data=subset(full.data.set,
                                    lagged_GHQ12_caseness>=0&
                                      sex==1),
                        cluster=~pid)

men.results<-rbind(
  wkaut5.ghq12.men$coeftable['wkaut5',],
  wkaut5.ghq12.men$coeftable['wkaut5:agreeableness',],
  wkaut5.ghq12.men$coeftable['wkaut5:extraversion',],
  wkaut5.ghq12.men$coeftable['wkaut5:neuroticism',],
  wkaut5.ghq12.men$coeftable['wkaut5:openness',],
  wkaut5.ghq12.men$coeftable['wkaut5:conscientiousness',])

pers.results[seq(from=1,to=11,by=2),2]<-men.results[,1]
pers.results[seq(from=2,to=12,by=2),2]<-men.results[,2]
pers.results[13,2]<-nrow(subset(full.data.set,SF12>=0&
                                  lagged_SF12>=0&autonomy>=0&
                                  sex==1))
pers.results[14,2]<-1-var(wkaut5.ghq12.men$residuals)/var(subset(full.data.set,SF12>=0&
                                                                     lagged_SF12>=0&autonomy>=0&
                                                                     sex==1)[,'SF12'])
pers.results[15,2]<-men.results[,2]<-mean(subset(full.data.set,SF12>=0&
                                                     lagged_SF12>=0&autonomy>=0&
                                                     sex==1)[,'SF12'])

##################
#Table 12 panel A#
##################
stargazer(pers.results)

men.and.women.assoc.contr.sf12<-cbind(c(men.results[1,1],
    men.results[2,1]*mean(subset(full.data.set,
                           lagged_GHQ12_caseness>=0&
                             sex==1)[,'agreeableness']),
    men.results[3,1]*mean(subset(full.data.set,
                                 lagged_GHQ12_caseness>=0&
                                   sex==1)[,'extraversion']),
    men.results[4,1]*mean(subset(full.data.set,
                                 lagged_GHQ12_caseness>=0&
                                   sex==1)[,'neuroticism']),
    men.results[5,1]*mean(subset(full.data.set,
                                 lagged_GHQ12_caseness>=0&
                                   sex==1)[,'openness']),
    men.results[6,1]*mean(subset(full.data.set,
                                 lagged_GHQ12_caseness>=0&
                                   sex==1)[,'conscientiousness'])),
    c(women.results[1,1],
      women.results[2,1]*mean(subset(full.data.set,
                                   lagged_GHQ12_caseness>=0&
                                     sex==2)[,'agreeableness']),
      women.results[3,1]*mean(subset(full.data.set,
                                   lagged_GHQ12_caseness>=0&
                                     sex==2)[,'extraversion']),
      women.results[4,1]*mean(subset(full.data.set,
                                   lagged_GHQ12_caseness>=0&
                                     sex==2)[,'neuroticism']),
      women.results[5,1]*mean(subset(full.data.set,
                                   lagged_GHQ12_caseness>=0&
                                     sex==2)[,'openness']),
      women.results[6,1]*mean(subset(full.data.set,
                                   lagged_GHQ12_caseness>=0&
                                     sex==2)[,'conscientiousness'])))

men.and.women.assoc.contr<-cbind(men.and.women.assoc.contr.ghq12[,1],men.and.women.assoc.contr.sf12[,1],
      men.and.women.assoc.contr.ghq12[,2],men.and.women.assoc.contr.sf12[,2])


##################
#Table 12 panel B#
##################
stargazer(men.and.women.assoc.contr)
sum(men.and.women.assoc.contr[,1])
sum(men.and.women.assoc.contr[,2])
sum(men.and.women.assoc.contr[,3])
sum(men.and.women.assoc.contr[,4])

####################################################
#Robustness test: run a PCA and use this
pca.wkaut<-prcomp(~wkaut1+wkaut2+wkaut3+wkaut4+wkaut5,
                  data = full.data.set)
equal.weights<-
seq(from=1,
    to=1,
    length.out=5)
###########
#Figure F1#
###########
setEPS(width=8,height=(16/3))
postscript("PCA_results.eps")
par(mar=c(3,6,2,2),mfrow=c(1,1))
barplot(rbind(pca.wkaut$rotation[,1]*5/sum(pca.wkaut$rotation[,1]),
              equal.weights),main='PC1 factor loadings',
        beside=TRUE,col=c(1,2),horiz=TRUE,
        names=c('Tasks',
                'Pace',
                'Manner',
                'Task Order',
                'Work hours'),las=2)
legend('bottomleft',c('PCA','Equal weights'),fill=c(1,2),bg='white')
dev.off()

full.data.set.pca<-cbind(
  subset(full.data.set,wkaut1>=0&
           wkaut2>=0&
           wkaut3>=0&
           wkaut4>=0&
           wkaut5>=0),pca.wkaut$x[,1])
colnames(full.data.set.pca)[ncol(full.data.set.pca)]<-
  'autonomy_pca'
cor(full.data.set.pca[,'autonomy'],
    full.data.set.pca[,'autonomy_pca'])
full.data.set.pca[,'autonomy_pca']<-
  sqrt(var(full.data.set.pca[,'autonomy']))/
  sqrt(var(full.data.set.pca[,'autonomy_pca']))*
  full.data.set.pca[,'autonomy_pca']

pca.results<-matrix(ncol=4,nrow=20)
for (s in c(1,2)){
  for (mh in c('GHQ12_caseness','SF12')){
   
    if(mh=='GHQ12_caseness'){
    mydf<-subset(full.data.set.pca,lagged_GHQ12_caseness>=0&GHQ12_caseness>=0)
    }
    
    if(mh=='SF12'){
      mydf<-subset(full.data.set.pca,lagged_SF12>=0&SF12>=0)
    }
    
    formula<-paste(mh,'~autonomy_pca+wave_number',sep='')
    this.reg<-lm(as.formula(formula),
                 subset(mydf,sex==s))
    se<-coeftest(this.reg,
        vcovHC(this.reg, type = 'HC0',cluster=~pid))
    
    pca.results[1+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),1]<-
      coef(this.reg)['autonomy_pca']
    pca.results[2+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),1]<-
      se['autonomy_pca',2]
    
    pca.results[3+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),1]<-
      nrow(subset(mydf,sex==s))
      
    pca.results[4+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),1]<-
      1-var(this.reg$residuals)/var(subset(mydf,sex==s)[,mh])
      
    pca.results[5+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),1]<-
      se['autonomy_pca',2]<-mean(subset(mydf,sex==s)[,mh])
    
    formula<-paste(mh,'~autonomy_pca+wave_number+factor(occ)+
                  factor(education)+factor(marital_stat)+
                  (no_children>0)',sep='')
    this.reg<-lm(as.formula(formula),
                 subset(mydf,sex==s))
    se<-coeftest(this.reg,
                 vcovHC(this.reg, type = 'HC0',cluster=~pid))
    
    pca.results[1+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),2]<-
      coef(this.reg)['autonomy_pca']
    pca.results[2+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),2]<-
      se['autonomy_pca',2]
    
    pca.results[3+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),2]<-
      nrow(subset(mydf,sex==s))
    
    pca.results[4+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),2]<-
      1-var(this.reg$residuals)/var(subset(mydf,sex==s)[,mh])
    
    pca.results[5+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),2]<-
      se['autonomy_pca',2]<-mean(subset(mydf,sex==s)[,mh])
    
    
    formula<-paste(mh,'~autonomy_pca|pid+wave_number',sep='')
    this.reg<-feols(as.formula(formula),
                 subset(mydf,sex==s))
    se<-coeftest(this.reg,
                 vcovHC(this.reg, type = 'HC0',cluster=~pid))
    
    pca.results[1+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),
                3]<-
      coef(this.reg)['autonomy_pca']
    pca.results[2+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),
                3]<-
      se['autonomy_pca',2]
    
    pca.results[3+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),3]<-
      nrow(subset(mydf,sex==s))
    
    pca.results[4+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),3]<-
      1-var(this.reg$residuals)/var(subset(mydf,sex==s)[,mh])
    
    pca.results[5+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),3]<-
      se['autonomy_pca',2]<-mean(subset(mydf,sex==s)[,mh])
    
    
    formula<-paste(mh,'~autonomy_pca+factor(occ)+
                  factor(education)+factor(marital_stat)+
                  (no_children>0)|pid+wave_number',sep='')
    this.reg<-feols(as.formula(formula),
                    subset(mydf,sex==s),cluster=~pid)
    
    pca.results[1+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),
                4]<-
      coef(this.reg)['autonomy_pca']
    pca.results[2+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),
                4]<-
      this.reg$coeftable['autonomy_pca',2]
     
    pca.results[3+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),4]<-
      nrow(subset(mydf,sex==s))
    
    pca.results[4+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),4]<-
      1-var(this.reg$residuals)/var(subset(mydf,sex==s)[,mh])
    
    pca.results[5+5*(match(mh,c('GHQ12_caseness','SF12'))-1)+10*(s-1),4]<-
      se['autonomy_pca',2]<-mean(subset(mydf,sex==s)[,mh])
    
  }
}

##########
#Table F1#
##########
stargazer(pca.results)

results.incl.no.pers<-return_baseline_results(full.data.set.keep.no.pers)
results.table.inc.no.pers<-construct_results_table(results.incl.no.pers)

##########
#Table H1#
##########
stargazer(results.table.inc.no.pers[1:14,1:8])
stargazer(results.table.inc.no.pers[1:14,9:16])
##########
#Table H2#
##########
stargazer(results.table.inc.no.pers[15:28,1:8])
stargazer(results.table.inc.no.pers[15:28,9:16])

results.incl.non.workers<-return_baseline_results(full.data.set.incl.non.workers)
results.table.inc.non.workers<-construct_results_table(results.incl.non.workers)

##########
#Table G1#
##########
stargazer(results.table.inc.non.workers[1:14,1:8])
stargazer(results.table.inc.non.workers[1:14,9:16])
##########
#Table G2#
##########
stargazer(results.table.inc.non.workers[15:28,1:8])
stargazer(results.table.inc.non.workers[15:28,9:16])

full.data.set.diff.aut.def<-
  full.data.set[,colnames(full.data.set)[colnames(full.data.set)%in%
                                           c('autonomy','wkaut1','wkaut2','wkaut3',
                                            'wkaut4','wkaut5')==FALSE]]
colnames(full.data.set.diff.aut.def)[
  match(c("no_autonomy_wkaut1","no_autonomy_wkaut2","no_autonomy_wkaut3",
          "no_autonomy_wkaut4","no_autonomy_wkaut5"),
        colnames(full.data.set.diff.aut.def))]<-paste('wkaut',
                                                      seq(from=1,to=5,by=1),sep='')

#Repeat the analysis but with "no autonomy" defining autonomy types
autonomy.r1.definition<-as.data.frame(round((full.data.set.diff.aut.def[,'wkaut1']+
                                             full.data.set.diff.aut.def[,'wkaut2']+
                                             full.data.set.diff.aut.def[,'wkaut3']+
                                             full.data.set.diff.aut.def[,'wkaut4']+
                                             full.data.set.diff.aut.def[,'wkaut5'])/5))
colnames(autonomy.r1.definition)<-'autonomy'
full.data.set.diff.aut.def<-cbind(full.data.set.diff.aut.def,autonomy.r1.definition)

results.diff.aut.def<-return_baseline_results(full.data.set.diff.aut.def)
results.table.diff.aut.def<-construct_results_table(results.diff.aut.def)

##########
#Table D1#
##########
stargazer(results.table.diff.aut.def[1:14,1:8])
stargazer(results.table.diff.aut.def[1:14,9:16])
##########
#Table D2#
##########
stargazer(results.table.diff.aut.def[15:28,1:8])
stargazer(results.table.diff.aut.def[15:28,9:16])

ft.pt.sex.table<-table(subset(full.data.set,full_time>0)[,'sex'],
      subset(full.data.set,full_time>0)[,'full_time'])
ft.pt.sex.table[1,]<-ft.pt.sex.table[1,]/sum(ft.pt.sex.table[1,])
ft.pt.sex.table[2,]<-ft.pt.sex.table[2,]/sum(ft.pt.sex.table[2,])

#VIF analysis
vifs<-matrix(nrow=5,ncol=4)
for (mh in c('GHQ12_caseness','SF12')){
  for (s in c(1,2)){
formula<-paste(mh,'~wkaut1+wkaut2+
                   wkaut3+wkaut4+
                   wkaut5+factor(wave_number)',sep='')
col<-1+(match(mh,c('GHQ12_caseness','SF12'))-1)+
     2*(match(s,c(1,2))-1)
vifs[1:5,col]<-vif(lm(as.formula(formula),
                subset(full.data.set,sex==s)))[1:5,1]
}
}

rownames(vifs)<-paste('wkaut',seq(from=1,to=5,by=1),sep='')
colnames(vifs)<-c('GHQ12 caseness, men',
                 'SF12, men',
                 'GHQ12 caseness, women',
                 'SF12, women')

##########
#Table B1#
##########
stargazer(vifs)

###################################
#Interactions with having children#
###################################

women.spec.interacted.with.children<-
  feols(GHQ12_caseness~wkaut1*(no_children>0)+
          wkaut2*(no_children>0)+
          wkaut3*(no_children>0)+
          wkaut4*(no_children>0)+
          wkaut5*(no_children>0)+
          factor(education)+factor(marital_stat)+
          (no_children>0)+factor(occ)+factor(age)|pid+wave_number,
        data=subset(full.data.set,lagged_GHQ12_caseness>=0&sex==2&
                      full_time>=0),
        cluster=~pid)
women.spec.interacted.with.children$coeftable

women.spec.interacted.with.children.sf12<-
  feols(SF12~wkaut1*(no_children>0)+
          wkaut2*(no_children>0)+
          wkaut3*(no_children>0)+
          wkaut4*(no_children>0)+
          wkaut5*(no_children>0)+
          factor(education)+factor(marital_stat)+
          (no_children>0)+factor(occ)+factor(age)|pid+wave_number,
        data=subset(full.data.set,lagged_SF12>=0&sex==2&
                      full_time>=0),
        cluster=~pid)
women.spec.interacted.with.children.sf12$coeftable

men.spec.interacted.with.children<-
  feols(GHQ12_caseness~wkaut1*(no_children>0)+
          wkaut2*(no_children>0)+
          wkaut3*(no_children>0)+
          wkaut4*(no_children>0)+
          wkaut5*(no_children>0)+
          factor(education)+factor(marital_stat)+
          (no_children>0)+factor(occ)+factor(age)|pid+wave_number,
        data=subset(full.data.set,lagged_GHQ12_caseness>=0&sex==1&
                      full_time>=0),
        cluster=~pid)
men.spec.interacted.with.children$coeftable

men.spec.interacted.with.children.sf12<-
  feols(SF12~wkaut1*(no_children>0)+
          wkaut2*(no_children>0)+
          wkaut3*(no_children>0)+
          wkaut4*(no_children>0)+
          wkaut5*(no_children>0)+
          factor(education)+factor(marital_stat)+
          (no_children>0)+factor(occ)+factor(age)|pid+wave_number,
        data=subset(full.data.set,lagged_SF12>=0&sex==1&
                      full_time>=0),
        cluster=~pid)
men.spec.interacted.with.children.sf12$coeftable

#######################################
#Interactions with full-time/part-time#
#######################################

women.interaction.ft<-feols(GHQ12_caseness~wkaut1*factor(full_time)+
                            wkaut2*factor(full_time)+
                            wkaut3*factor(full_time)+
                            wkaut4*factor(full_time)+
                            wkaut5*factor(full_time)+
                            factor(education)+factor(marital_stat)+
                            (no_children>0)+factor(occ)+factor(age)|pid+wave_number,
                          data=subset(full.data.set,lagged_GHQ12_caseness>=0&sex==2&
                                        full_time>=0),
                          cluster=~pid)

women.interaction.ft$coeftable['wkaut1:factor(full_time)2',]
women.interaction.ft$coeftable['factor(full_time)2:wkaut2',]
women.interaction.ft$coeftable['factor(full_time)2:wkaut3',]
women.interaction.ft$coeftable['factor(full_time)2:wkaut4',]
women.interaction.ft$coeftable['factor(full_time)2:wkaut5',]

women.interaction.ft.sf12<-feols(SF12~wkaut1*factor(full_time)+
                                 wkaut2*factor(full_time)+
                                 wkaut3*factor(full_time)+
                                 wkaut4*factor(full_time)+
                                 wkaut5*factor(full_time)+
                                 factor(education)+factor(marital_stat)+
                                 (no_children>0)+factor(occ)+factor(age)|pid+wave_number,
                               data=subset(full.data.set,lagged_SF12>=0&sex==2&full_time>=0),
                               cluster=~pid)

women.interaction.ft.sf12$coeftable['wkaut1:factor(full_time)2',]
women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut2',]
women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut3',]
women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut4',]
women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut5',]

women.interaction.ft.index<-feols(GHQ12_caseness~autonomy*factor(full_time)+
                              factor(education)+factor(marital_stat)+
                              (no_children>0)+factor(occ)+factor(age)|pid+wave_number,
                            data=subset(full.data.set,lagged_GHQ12_caseness>=0&sex==2&
                                          full_time>=0),
                            cluster=~pid)

women.interaction.ft.sf12.index<-feols(SF12~autonomy*factor(full_time)+
                              factor(education)+factor(marital_stat)+
                              (no_children>0)+factor(occ)+factor(age)|pid+wave_number,
                            data=subset(full.data.set,lagged_SF12>=0&sex==2&
                                          full_time>=0),
                            cluster=~pid)

wk.autonomy.by.ft<-cbind(rbind(
women.interaction.ft.index$coeftable['autonomy',1],
women.interaction.ft.index$coeftable['autonomy',2],
women.interaction.ft.index$coeftable['autonomy:factor(full_time)2',1],
women.interaction.ft.index$coeftable['autonomy:factor(full_time)2',2],
nrow(subset(full.data.set,GHQ12_caseness>=0&full_time>=0&
              lagged_GHQ12_caseness>=0&autonomy>=0&sex==2)),
var(women.interaction.ft.index$fitted.values)/
  var(subset(full.data.set,GHQ12_caseness>=0&full_time>=0&
               lagged_GHQ12_caseness>=0&autonomy>=0&sex==2)[,'GHQ12_caseness']),
mean(subset(full.data.set,GHQ12_caseness>=0&full_time>=0&
              lagged_GHQ12_caseness>=0&autonomy>=0&sex==2)[,'GHQ12_caseness'])),
rbind(
women.interaction.ft.sf12.index$coeftable['autonomy',1],
women.interaction.ft.sf12.index$coeftable['autonomy',2],
women.interaction.ft.sf12.index$coeftable['autonomy:factor(full_time)2',1],
women.interaction.ft.sf12.index$coeftable['autonomy:factor(full_time)2',2],
nrow(subset(full.data.set,SF12>=0&full_time>=0&
              lagged_SF12>=0&autonomy>=0&sex==2)),
var(women.interaction.ft.sf12.index$fitted.values)/
  var(subset(full.data.set,SF12>=0&full_time>=0&
               lagged_SF12>=0&autonomy>=0&sex==2)[,'SF12']),
mean(subset(full.data.set,SF12>=0&full_time>=0&
              lagged_SF12>=0&autonomy>=0&sex==2)[,'SF12'])))

inidividual.aut.by.ft<-cbind(
rbind(
  women.interaction.ft$coeftable['wkaut1',1],
  women.interaction.ft$coeftable['wkaut1',2],
  women.interaction.ft$coeftable['wkaut1:factor(full_time)2',1],
  women.interaction.ft$coeftable['wkaut1:factor(full_time)2',2],
  women.interaction.ft$coeftable['wkaut2',1],
  women.interaction.ft$coeftable['wkaut2',2],
  women.interaction.ft$coeftable['factor(full_time)2:wkaut2',1],
  women.interaction.ft$coeftable['factor(full_time)2:wkaut2',2],
  women.interaction.ft$coeftable['wkaut3',1],
  women.interaction.ft$coeftable['wkaut3',2],
  women.interaction.ft$coeftable['factor(full_time)2:wkaut3',1],
  women.interaction.ft$coeftable['factor(full_time)2:wkaut3',2],
  women.interaction.ft$coeftable['wkaut4',1],
  women.interaction.ft$coeftable['wkaut4',2],
  women.interaction.ft$coeftable['factor(full_time)2:wkaut4',1],
  women.interaction.ft$coeftable['factor(full_time)2:wkaut4',2],
  women.interaction.ft$coeftable['wkaut5',1],
  women.interaction.ft$coeftable['wkaut5',2],
  women.interaction.ft$coeftable['factor(full_time)2:wkaut5',1],
  women.interaction.ft$coeftable['factor(full_time)2:wkaut5',2],
  nrow(subset(full.data.set,GHQ12_caseness>=0&full_time>=0&
           lagged_GHQ12_caseness>=0&autonomy>=0&sex==2)),
  var(women.interaction.ft$fitted.values)/
    var(subset(full.data.set,GHQ12_caseness>=0&full_time>=0&
                lagged_GHQ12_caseness>=0&autonomy>=0&sex==2)[,'GHQ12_caseness']),
  mean(subset(full.data.set,GHQ12_caseness>=0&full_time>=0&
                lagged_GHQ12_caseness>=0&autonomy>=0&sex==2)[,'GHQ12_caseness'])),
rbind(
  women.interaction.ft.sf12$coeftable['wkaut1',1],
  women.interaction.ft.sf12$coeftable['wkaut1',2],
  women.interaction.ft.sf12$coeftable['wkaut1:factor(full_time)2',1],
  women.interaction.ft.sf12$coeftable['wkaut1:factor(full_time)2',2],
  women.interaction.ft.sf12$coeftable['wkaut2',1],
  women.interaction.ft.sf12$coeftable['wkaut2',2],
  women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut2',1],
  women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut2',2],
  women.interaction.ft.sf12$coeftable['wkaut3',1],
  women.interaction.ft.sf12$coeftable['wkaut3',2],
  women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut3',1],
  women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut3',2],
  women.interaction.ft.sf12$coeftable['wkaut4',1],
  women.interaction.ft.sf12$coeftable['wkaut4',2],
  women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut4',1],
  women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut4',2],
  women.interaction.ft.sf12$coeftable['wkaut5',1],
  women.interaction.ft.sf12$coeftable['wkaut5',2],
  women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut5',1],
  women.interaction.ft.sf12$coeftable['factor(full_time)2:wkaut5',2],
  nrow(subset(full.data.set,SF12>=0&full_time>=0&
                lagged_SF12>=0&autonomy>=0&sex==2)),
  var(women.interaction.ft.sf12$fitted.values)/
    var(subset(full.data.set,SF12>=0&full_time>=0&
                 lagged_SF12>=0&autonomy>=0&sex==2)[,'SF12']),
  mean(subset(full.data.set,SF12>=0&full_time>=0&
                lagged_SF12>=0&autonomy>=0&sex==2)[,'SF12']))
)

##########
#Table 11#
##########
stargazer(cbind(wk.autonomy.by.ft[,1],inidividual.aut.by.ft[,1],
                wk.autonomy.by.ft[,2],inidividual.aut.by.ft[,2]))

########################################
#Same with interactions with occupation#
########################################
occ.interaction.coefficients<-matrix(ncol=4,nrow=21)
within.occ.coeffs<-matrix(ncol=4,nrow=36)

for (s in c(1,2)){
  for (mh in c('GHQ12_caseness','SF12')){
   
    my.df<-full.data.set
    colnames(my.df)[match(paste('lagged_',mh,sep=''),colnames(my.df))]<-'lagged_var'
    my.df<-subset(my.df,lagged_var>=0)
    
    formula<-as.formula(paste(mh,'~wkaut1*factor(one_dig_occ)+
                      wkaut2*factor(one_dig_occ)+
                      wkaut3*factor(one_dig_occ)+
                      wkaut4*factor(one_dig_occ)+
                      wkaut5*factor(one_dig_occ)+
                      factor(education)+factor(marital_stat)+
                      (no_children>0)+factor(occ)+factor(age)|pid+wave_number',
                      sep=''))
    
    interaction.ft<-feols(formula,
                              data=subset(my.df,sex==s),
                              cluster=~pid)
    
    col.no<-match(mh,c('GHQ12_caseness','SF12'))+2*(s-1)
    
    unique.occupations<-seq(from=2,to=9,by=1)[is.na(match(paste('factor(one_dig_occ)',
                seq(from=2,to=9,by=1),':wkaut5',sep=''),
          rownames(interaction.ft$coeftable)))==FALSE]
    
    occ.interaction.coefficients[1,col.no]<-interaction.ft$coeftable['wkaut5',1]
    occ.interaction.coefficients[2,col.no]<-interaction.ft$coeftable['wkaut5',2]
    
    occ.interaction.coefficients[3+2*(unique.occupations-2),col.no]<-
      interaction.ft$coeftable[paste('factor(one_dig_occ)',
      unique.occupations,':wkaut5',sep=''),1]
    occ.interaction.coefficients[4+2*(unique.occupations-2),col.no]<-
      interaction.ft$coeftable[paste('factor(one_dig_occ)',
                                     unique.occupations,':wkaut5',sep=''),2]
    
    occ.interaction.coefficients[19,col.no]<-var(interaction.ft$fitted.values)/
      var(my.df[,mh][is.na(my.df[,mh])==FALSE])
    occ.interaction.coefficients[20,col.no]<-length(my.df[,mh][is.na(my.df[,mh])==FALSE])
    occ.interaction.coefficients[21,col.no]<-mean(my.df[,mh][is.na(my.df[,mh])==FALSE])
   
    my.df[,'one_dig_occ']<-as.numeric(my.df[,'one_dig_occ'])
    
    for (occ in sort(unique(my.df[,'one_dig_occ']))){
      
      my.df1<-my.df
      my.df1[,'one_dig_occ']<-ifelse(my.df1[,'one_dig_occ']==occ,
                                     my.df1[,'one_dig_occ'],NA)
      my.df1<-subset(my.df1,sex==s&
               is.na(one_dig_occ)==FALSE)
      
      formula<-as.formula(paste(mh,'~wkaut1+
                      wkaut2+
                      wkaut3+
                      wkaut4+
                      wkaut5+
                      factor(education)+factor(marital_stat)+
                      (no_children>0)+factor(occ)+factor(age)|pid+wave_number',
                                sep=''))
      
      within.occ.ft<-feols(formula,
                            data=subset(my.df1,sex==s),
                            cluster=~pid)
      
      if(is.na(match('wkaut5',rownames(within.occ.ft$coeftable)))==FALSE){
      
      within.occ.coeffs[1+4*(occ-1),col.no]<-within.occ.ft$coeftable['wkaut5',1]
      within.occ.coeffs[2+4*(occ-1),col.no]<-within.occ.ft$coeftable['wkaut5',2]
            within.occ.coeffs[3+4*(occ-1),col.no]<-
        mean(subset(my.df,sex==s&is.na(one_dig_occ)==FALSE)[,mh][is.na(subset(my.df,sex==s&is.na(one_dig_occ)==FALSE)[,mh])==FALSE])

    }
    }
     
  }
}

within.occ.coeffs[seq(from=4,to=36,by=4),1]<-
  table(subset(full.data.set,sex==1&one_dig_occ>0)[,'one_dig_occ'])/
  nrow(subset(full.data.set,sex==1&one_dig_occ>0))
within.occ.coeffs[seq(from=4,to=36,by=4),3]<-
  table(subset(full.data.set,sex==2&one_dig_occ>0)[,'one_dig_occ'])/
  nrow(subset(full.data.set,sex==2&one_dig_occ>0))

##########
#Table 13#
##########
stargazer(within.occ.coeffs)

##########################################
#Is education perfectly explained by pid?#
##########################################
max.educ<-aggregate(education~pid,FUN=max,data=full.data.set)
min.educ<-aggregate(education~pid,FUN=min,data=full.data.set)

educ.max.min<-cbind(max.educ,min.educ)
share.of.people.who.never.change.educ<-
  sum((educ.max.min[,4]-educ.max.min[,2])==0)/nrow(educ.max.min)


###############
#IPW estimates#
###############

attrit.next.period<-
  as.data.frame(
    is.na(match(interaction(full.data.set[,'pid'],full.data.set[,'wave_number']+2),
                interaction(full.data.set[,'pid'],full.data.set[,'wave_number']))))
colnames(attrit.next.period)<-'attrit_next_period'
full.data.set<-cbind(full.data.set,attrit.next.period)

p.each.person.attrits<-aggregate(attrit_next_period~pid,
                      data=subset(full.data.set,wave_number<9),FUN=max)

table(subset(full.data.set,wave_number<9)[,'attrit_next_period'])/
  nrow(subset(full.data.set,wave_number<9))

attrition.model <- glm(attrit_next_period ~ factor(age)+
                         factor(sex)+factor(education)+
                         factor(marital_stat)+labor_income+
                         (no_children>0)+
                         factor(agreeableness)+
                         factor(conscientiousness)+
                         factor(extraversion)+
                         factor(neuroticism)+
                         factor(openness)+
                         factor(one_dig_occ), 
                       family = binomial(link = "probit"), 
                       data = subset(full.data.set,wave_number<9))

model.data<-as.data.frame(cbind(attrition.model$fitted.values,
                  attrition.model$data))

p.attrited<-as.data.frame(
  model.data[match(interaction(full.data.set[,'pid'],full.data.set[,'wave_number']-2),
                      interaction(model.data[,'pid'],model.data[,'wave_number'])),
                      'attrition.model$fitted.values'])                         

colnames(p.attrited)<-'p_attrited'
full.data.set<-cbind(full.data.set,p.attrited)

#re-run preferred spec with and without IPW
ipw.results.matrix<-matrix(ncol=8,nrow=16)
for (s in c(1,2)){
  for (var in c('GHQ12_caseness','SF12')){
    
    my.df<-subset(full.data.set,sex==s&
                    autonomy>=0&
                    education>=0&
                    age>0&
                    marital_stat>=0&
                    labor_income>=0&
                    occ>=0&
                    no_children>=0&
                    is.na(p_attrited)==FALSE)
    
    if(var=='GHQ12_caseness'){
      my.df<-subset(my.df,lagged_GHQ12_caseness>=0)
    }
    if(var=='SF12'){
      my.df<-subset(my.df,lagged_SF12>=0)
    }
     
       aut.conts<-c('autonomy','wkaut1+wkaut2+wkaut3+wkaut4+wkaut5')
    
        lagged.conts<-c('',paste('+factor(occ)',
                  '+factor(education)+factor(marital_stat)+factor(age)+(no_children>0)',
                             sep=''))
    
        conts<-lagged.conts[2]
      
        aut.cont<-aut.conts[1]  
        
        formula<-as.formula(paste(var,'~',aut.cont,conts,'|pid+wave_number',sep=''))
        
        fe.est<-feols(formula, data=subset(my.df,p_attrited>=0), cluster=~pid)
        
        ipw.results.matrix[1,1+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
        fe.est$coeftable['autonomy',1]  
        
        ipw.results.matrix[2,1+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          fe.est$coeftable['autonomy',2]  
        
        ipw.results.matrix[3,1+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          1-var(fe.est$residuals)/var(my.df[,var][is.na(my.df[,var])==FALSE])
      
        fe.est<-feols(formula, data=subset(my.df,p_attrited>=0), cluster=~pid,
                      weights=(1/(1-subset(my.df,p_attrited>=0)[,'p_attrited'])))
        
        ipw.results.matrix[1,2+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          fe.est$coeftable['autonomy',1]  
        
        ipw.results.matrix[2,2+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          fe.est$coeftable['autonomy',2]  
        
        ipw.results.matrix[3,2+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          1-var(fe.est$residuals)/var(my.df[,var][is.na(my.df[,var])==FALSE])
        
        #And individual autonomy components
        
        aut.cont<-aut.conts[2]  
        
        formula<-as.formula(paste(var,'~',aut.cont,conts,'|pid+wave_number',sep=''))
        
        fe.est<-feols(formula, data=subset(my.df,p_attrited>=0), cluster=~pid)
        
        ipw.results.matrix[seq(from=4,to=12,by=2),
                  1+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          fe.est$coeftable[paste('wkaut',seq(from=1,to=5,by=1),sep=''),1]  
        
        ipw.results.matrix[seq(from=5,to=13,by=2),
                           1+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          fe.est$coeftable[paste('wkaut',seq(from=1,to=5,by=1),sep=''),2]
        
        ipw.results.matrix[14,1+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          1-var(fe.est$residuals)/var(my.df[,var][is.na(my.df[,var])==FALSE])
        
        fe.est<-feols(formula, data=subset(my.df,p_attrited>=0), cluster=~pid,
                      weights=(1/(1-subset(my.df,p_attrited>=0)[,'p_attrited'])))
        
        ipw.results.matrix[seq(from=4,to=12,by=2),
                           2+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          fe.est$coeftable[paste('wkaut',seq(from=1,to=5,by=1),sep=''),1]  
        
        ipw.results.matrix[seq(from=5,to=13,by=2),
                           2+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          fe.est$coeftable[paste('wkaut',seq(from=1,to=5,by=1),sep=''),2]
        
        ipw.results.matrix[14,2+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          1-var(fe.est$residuals)/var(my.df[,var][is.na(my.df[,var])==FALSE])        
        
        ipw.results.matrix[15,1+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          length(my.df[,var][is.na(my.df[,var])==FALSE])      
        
        ipw.results.matrix[15,2+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          length(my.df[,var][is.na(my.df[,var])==FALSE])        
        
        ipw.results.matrix[16,1+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          sum(ifelse(is.na(my.df[,var]/my.df[,'p_attrited']),0,
                 my.df[,var]))/
          sum(ifelse(is.na(my.df[,var]/my.df[,'p_attrited']),0,
                     1))
          
        
        ipw.results.matrix[16,2+2*(match(var,c('GHQ12_caseness','SF12'))-1)+
                             4*(match(s,c(1,2))-1)]<-
          sum(ifelse(is.na(my.df[,var]/(1-my.df[,'p_attrited'])),0,
                     my.df[,var]/(1-my.df[,'p_attrited'])))/
          sum(ifelse(is.na(my.df[,var]/my.df[,'p_attrited']),0,
                     1/(1-my.df[,'p_attrited'])))        
        

      }
}

#########
#Table 9#
#########
stargazer(ipw.results.matrix)
