rm(list=ls())
library(plm)
set.seed(811072144)

#####################
#Define repositories#
#####################

data.repository<-'C:\\Users\\zvh514\\OneDrive - University of York\\Documents\\Autonomy_and_mental_health\\data'
setwd(data.repository)

my_plm<-function(y,x,z,fe,data){
  
  y.aggregated<-aggregate(data[,y]~data[,fe],FUN=mean)
  
  data[,y]<-
    data[,y]-
    y.aggregated[match(data[,fe],y.aggregated[,1]),2]
  
  for (i in seq(from=1,to=length(x),by=1)){
    
    x.i.aggregated<-aggregate(data[,x[i]]~data[,fe],FUN=mean)
    
    data[,x[i]]<-
    data[,x[i]]-
      x.i.aggregated[match(data[,fe],x.i.aggregated[,1]),2]
      
  }
  
  
  for (i in seq(from=1,to=length(z),by=1)){
    
    for (j in unique(data[,z[i]])){
      
      z.ij.aggregated<-aggregate((data[,z[i]]==j)~data[,fe],FUN=mean)
      
      
    }
 
    data<-cbind(data,
      data[,z[i]]-
        z.ij.aggregated[match(data[,fe],z.ij.aggregated[,1]),2]
    )
    colnames(data)[ncol(data)]<-paste(z[i],j,sep='')
  }
    
  lm(data[,y]~data[,x])

}
  
plm(autonomy~factor(education)+factor(occ)+
      factor(marital_stat)+(no_children>0)+
      factor(wave_number),
    effect='individual',
    index=c('pid'),data=subset(df.draw,sex==s))

#setwd('C:\\Users\\jms41081\\Documents\\mental_health_and_autonomy\\processed_data')
full.data.set<-read.csv('autonomy_mh_data_set.CSV')

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
  full.data.set[,paste('wkaut',val,sep='')]<-
    ifelse(full.data.set[,paste('wkaut',val,sep='')]<0,
           NA,round(full.data.set[,paste('wkaut',val,sep='')]/4))
}

autonomy<-as.data.frame(round((full.data.set[,'wkaut1']+
                                 full.data.set[,'wkaut2']+full.data.set[,'wkaut3']+
                                 full.data.set[,'wkaut4']+full.data.set[,'wkaut5'])/5))
colnames(autonomy)<-'autonomy'
full.data.set<-cbind(full.data.set,autonomy)

#lagged autonomy
lagged.autonomy<-as.data.frame(
  full.data.set[match(interaction(full.data.set[,'pid'],full.data.set[,'wave_number']-2),
                      interaction(full.data.set[,'pid'],full.data.set[,'wave_number'])),'autonomy']
)
colnames(lagged.autonomy)<-'lagged_autonomy'
full.data.set<-cbind(full.data.set,lagged.autonomy)
#########################
#Convert to log income
full.data.set[,'labor_income']<-ifelse(full.data.set[,'labor_income']>0,
                                       log(full.data.set[,'labor_income']),NA)

#created lagged mental health variables
for (var in c('GHQ12_caseness','SF12')){
  
  lagged.mh<-as.data.frame(full.data.set[match(interaction(full.data.set[,'pid'],
                                                           full.data.set[,'wave_number']-1),
                                               interaction(full.data.set[,'pid'],
                                                           full.data.set[,'wave_number'])),var])
  
  colnames(lagged.mh)<-paste('lagged_',var,sep='')
  
  full.data.set<-cbind(full.data.set,lagged.mh)
  
  
  
}

#start by removing all those who are not continuously in work
ever.not.working<-unique(subset(full.data.set,jb_stat!=2)[,'pid'])
full.data.set.incl.non.workers<-full.data.set
full.data.set<-subset(full.data.set,pid%in%ever.not.working==FALSE)

#Remove all nulls
full.data.set<-subset(full.data.set,is.na(openness)==FALSE&
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
wkaut.controls<-matrix(ncol=5,nrow=4)
wkaut.controls[,1]<-seq(from=2,to=5,by=1)
wkaut.controls[,2]<-c(1,seq(from=3,to=5,by=1))
wkaut.controls[,3]<-c(seq(from=1,to=2,by=1),seq(from=4,to=5,by=1))
wkaut.controls[,4]<-c(seq(from=1,to=3,by=1),5)
wkaut.controls[,5]<-seq(from=1,to=4,by=1)

res.mat.ghq12<-matrix(ncol=12,nrow=12)
res.mat.SF12<-matrix(ncol=12,nrow=12)
res.mat.ghq12.rsq<-matrix(ncol=4,nrow=12)
res.mat.SF12.rsq<-matrix(ncol=4,nrow=12)

delta.list<-c(seq(from=-1.3,to=-0.9,length.out=3),
              seq(from=0.9,to=1.3,length.out=3))


#store the results as draws
ndraws<-1000

draws.ghq12<-matrix(nrow=ndraws,ncol=24)
draws.sf12<-matrix(nrow=ndraws,ncol=24)
draws.ghq12.disagg<-matrix(nrow=ndraws,ncol=120)
draws.sf12.disagg<-matrix(nrow=ndraws,ncol=120)
draws.nrow<-matrix(nrow=ndraws,ncol=2)

pid.list<-unique(full.data.set[,'pid'])

#for each draw, select a sample
for (i.draw in seq(from=1,to=ndraws,by=1)){
 
pid.draw<-sample(pid.list,length(pid.list),replace=TRUE)  
  
df.draw<-subset(full.data.set,pid%in%pid.draw)
draws.nrow[i.draw,1]<-length(pid.draw)
draws.nrow[i.draw,2]<-nrow(df.draw)

#Conditional effects with changing controls
res.cond.eff<-matrix(ncol=28*2,nrow=2)

for (s in c(1,2)){
  
  for (var in c('GHQ12_caseness','SF12')){
    
    my.df<-subset(df.draw,sex==s&
                    education>=0&
                    age>0&
                    marital_stat>=0&
                    labor_income>=0&
                    occ>=0&
                    no_children>=0)  
    
    if(var=='GHQ12_caseness'){
      my.df<-subset(my.df,lagged_GHQ12_caseness>=0)
    }
    if(var=='SF12'){
      my.df<-subset(my.df,lagged_SF12>=0)
    }
    
    controls.list<-c('',paste('factor(age)+factor(marital_stat)+factor(education)+factor(agreeableness)+factor(conscientiousness)+factor(extraversion)+factor(neuroticism)+factor(openness)+
        factor(occ)+labor_income+(no_children>0)',sep=''))
    
   val<-1
      
      formula.this<-paste(var,'~',paste('autonomy+',
                                        controls.list[val],'+factor(wave_number)',sep=''))
      csness.reg<-lm(formula.this,data=my.df)
      se <- summary(csness.reg)$coefficients
      res.cond.eff[val,1+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28]<-se[2,1]
      res.cond.eff[val,2+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28]<-se[2,2]
      res.cond.eff[val,3+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28]<-summary(csness.reg)$r.squared
      
      formula.this<-paste(var,'~',
                          paste('wkaut1+wkaut2+wkaut3+wkaut4+wkaut5+',
                                controls.list[val],'+factor(wave_number)',sep=''))
      csness.reg.disag<-lm(formula.this,data=my.df)
      se <- se <- summary(csness.reg.disag)$coefficients
      res.cond.eff[val,(4+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28):(8+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28)]<-se[2:6,1]
      res.cond.eff[val,(9+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28):(13+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28)]<-se[2:6,2]
      res.cond.eff[val,14+14*(match(var,c('GHQ12_caseness','SF12'))-1)+(s-1)*28]<-summary(csness.reg.disag)$r.squared
      
  }
  
  aut.conts<-c('autonomy','wkaut1+wkaut2+wkaut3+wkaut4+wkaut5')
  
  for (var in c('GHQ12_caseness','SF12')){
    
    lagged.conts<-paste('+factor(occ)',
                             '+factor(education)+factor(marital_stat)+factor(age)+
      (no_children>0)',sep='')
      
      for (aut.cont in aut.conts){  
        
        #Now for the remaining ones, I need a plm function
        formula<-as.formula(paste(var,'~',aut.cont,lagged.conts,sep=''))
        
        plm.caseness.fe<-plm(formula, index=c('pid','wave_number'),data=my.df)
        
        se<-summary(plm.caseness.fe)$coefficients
        
        row<-2
        
        col.shifts<-14*(match(var,c('GHQ12_caseness','SF12'))-1)
        var.shift<-(match(aut.cont,aut.conts)-1)*4
        se.shift<-1+(aut.cont=='wkaut1+wkaut2+wkaut3+wkaut4+wkaut5')*4
        
        col.first<-1+col.shifts+(match(aut.cont,aut.conts)-1)*3+(s-1)*28
        col.last<-col.first+var.shift
        
        df<-my.df
        
        colnames(df)[match(var,colnames(df))]<-'var'
        
        res.cond.eff[row,col.first:col.last]<-as.matrix(se[1:(1+(match(aut.cont,aut.conts)-1)*4),1])
        res.cond.eff[row,(se.shift+col.first):(se.shift+col.last)]<-as.matrix(se[1:(1+(match(aut.cont,aut.conts)-1)*4),2])
        res.cond.eff[row,(se.shift+col.last+1)]<-1-(var(plm.caseness.fe$residuals))/
          (var(subset(df,var>=0)[,'var']))
        
      }
    }
  }


###########################################################
#Do the Oster correction

#options are:
#  GHQ12 or SF12
#  autonomy, wkaut1, wkaut2, wkaut3, wkaut4, wkaut5
#  delta=-1.3, delta=-1, delta=1, delta=1.3
#  max r.squared ratio 1.1, 1.3
#  men and women
#  So 2 x 6 x 4 x 2 x 2= 192

for (s in c(1,2)){
  
  for (delta in delta.list){
    
    #and for each max r.sq ratio
    for (rsq.ratio in seq(from=1.1,to=1.3,by=0.2)){
      
      #get the row
      row<-(match(delta,delta.list)-1)*2+
        match(rsq.ratio,seq(from=1.1,to=1.3,by=0.2))
      
      
      #######################################
      #Run short reg and basic reg
      sigma_y<-var(subset(df.draw,GHQ12_caseness>=0&sex==s)[,'GHQ12_caseness'])
      sigma_X<-var(subset(df.draw,autonomy>=0&sex==s)[,'autonomy'])
      reg.x<-plm(autonomy~factor(education)+factor(occ)+
                   factor(marital_stat)+(no_children>0)+
                   factor(wave_number),
                 effect='individual',
                 index=c('pid'),data=subset(df.draw,sex==s))
      tau_x<-var(reg.x$residuals)
      R_short<-res.cond.eff[2,3+(s-1)*28]
      R_basic<-res.cond.eff[1,3+(s-1)*28]
      beta_short<-res.cond.eff[2,1+(s-1)*28]
      beta_basic<-res.cond.eff[1,1+(s-1)*28]
      R_max<-min(res.cond.eff[2,3+(s-1)*28]*rsq.ratio,1)
      res.mat.ghq12.rsq[row,1+2*(s-1)]<-R_max
      
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
      #sol.this<-ifelse(length(sol.this)>=1,sol.this,beta_short)
      draws.ghq12[i.draw,match(delta,delta.list)+6*(match(rsq.ratio,seq(from=1.1,to=1.3,by=0.2))-1)
                  +12*(s-1)]<-beta_short-
        as.numeric(sol.this[match(min(abs(as.numeric(sol.this))),
                                  abs(as.numeric(sol.this)))])
      
      delta*(beta_basic-beta_short)*(R_max-R_short)/(R_short-R_basic)
      
      
      #######################################
      #same for SF12
      sigma_y<-var(subset(df.draw,SF12>=0&sex==s)[,'SF12'])
      sigma_X<-var(subset(df.draw,autonomy>=0&sex==s)[,'autonomy'])
      reg.x<-plm(autonomy~factor(education)+factor(occ)+
                   factor(marital_stat)+(no_children>0)+factor(wave_number),
                 effect='individual',
                 index=c('pid'),data=subset(df.draw,sex==s))
      tau_x<-var(reg.x$residuals)
      R_short<-res.cond.eff[2,17+(s-1)*28]
      R_basic<-res.cond.eff[1,17+(s-1)*28]
      beta_short<-res.cond.eff[2,15+(s-1)*28]
      beta_basic<-res.cond.eff[1,15+(s-1)*28]
      R_max<-min(R_short*rsq.ratio,1)
      res.mat.SF12.rsq[row,1+2*(s-1)]<-R_max
      
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
      #sol.this<-ifelse(length(sol.this)>=1,sol.this,beta_short)
      draws.sf12[i.draw,match(delta,delta.list)+6*(match(rsq.ratio,seq(from=1.1,to=1.3,by=0.2))-1)
                  +12*(s-1)]<-beta_short-
        as.numeric(sol.this[match(min(abs(as.numeric(sol.this))),
                                  abs(as.numeric(sol.this)))])
      
      
      ###################################
      #wkaut individually
      sigma_y<-var(subset(df.draw,GHQ12_caseness>=0&sex==s)[,'GHQ12_caseness'])
      R_short<-res.cond.eff[2,14+(s-1)*28]
      R_basic<-res.cond.eff[1,14+(s-1)*28]
      R_max<-min(R_short*rsq.ratio,1)
      res.mat.ghq12.rsq[row,2+2*(s-1)]<-R_max
      
      #for each autonomy variable
      count.to.five<-seq(from=1,to=5,by=1)
      for (val in count.to.five){
        
        sigma_X<-var(subset(df.draw,autonomy>=0&sex==s)[,paste('wkaut',val,sep='')])
        reg.x<-plm(paste('wkaut',val,'~','wkaut',wkaut.controls[1,val],
                         '+wkaut',wkaut.controls[2,val],
                         '+wkaut',wkaut.controls[3,val],
                         '+wkaut',wkaut.controls[4,val],
                         '+factor(education)+factor(occ)+',
                         'factor(marital_stat)+(no_children>0)+factor(wave_number)',
                         sep=''), effect='individual',
                   index=c('pid'),data=subset(df.draw,sex==s))
        tau_x<-var(reg.x$residuals)
        beta_short<-res.cond.eff[2,3+val+(s-1)*28]
        beta_basic<-res.cond.eff[1,3+val+(s-1)*28]
        
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
        #sol.this<-ifelse(length(sol.this)>=1,sol.this,beta_short)
        #oster.res[d.count,col.adjust+1]<-as.numeric(sol.this[match(min(abs(as.numeric(sol.this)-beta_short)),abs(as.numeric(sol.this)-beta_short))])
        draws.ghq12.disagg[i.draw,
            match(delta,delta.list)+6*(match(rsq.ratio,seq(from=1.1,to=1.3,by=0.2))-1)
                  +60*(s-1)+12*(val-1)]<-beta_short-
          as.numeric(sol.this[match(min(abs(as.numeric(sol.this))),
                                    abs(as.numeric(sol.this)))])
        
      }
      
      
      ###################################
      #wkaut individually for SF12
      sigma_y<-var(subset(df.draw,SF12>=0&sex==s)[,'SF12'])
      R_short<-res.cond.eff[2,28+(s-1)*28]
      R_basic<-res.cond.eff[1,28+(s-1)*28]
      R_max<-min(R_short*rsq.ratio,1)
      res.mat.SF12.rsq[row,2]<-R_max
      
      #for each autonomy variable
      count.to.five<-seq(from=1,to=5,by=1)
      for (val in count.to.five){
        
        sigma_X<-var(subset(df.draw,autonomy>=0&sex==s)[,paste('wkaut',val,sep='')])
        reg.x<-plm(paste('wkaut',val,'~','wkaut',wkaut.controls[1,val],
                         '+wkaut',wkaut.controls[2,val],
                         '+wkaut',wkaut.controls[3,val],
                         '+wkaut',wkaut.controls[4,val],
                         '+factor(education)+factor(occ)+',
                         'factor(marital_stat)+(no_children>0)+factor(wave_number)',
                         sep=''), effect='individual',
                   index=c('pid'),data=subset(df.draw,sex==s))
        tau_x<-var(reg.x$residuals)
        beta_short<-res.cond.eff[2,17+val+(s-1)*28]
        beta_basic<-res.cond.eff[1,17+val+(s-1)*28]
        
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
        #sol.this<-ifelse(length(sol.this)>=1,sol.this,beta_short)
        #oster.res[d.count,col.adjust+1]<-as.numeric(sol.this[match(min(abs(as.numeric(sol.this)-beta_short)),abs(as.numeric(sol.this)-beta_short))])
        draws.sf12.disagg[i.draw,
                           match(delta,delta.list)+6*(match(rsq.ratio,seq(from=1.1,to=1.3,by=0.2))-1)
                           +60*(s-1)+12*(val-1)]<-beta_short-
          as.numeric(sol.this[match(min(abs(as.numeric(sol.this))),
                                    abs(as.numeric(sol.this)))])
        
      }
    }
  }
}

}

write.csv(draws.ghq12,'res_mat_ghq12_draws.CSV')
write.csv(draws.sf12,'res_mat_sf12_draws.CSV')
write.csv(draws.ghq12.disagg,'res_mat_ghq12_draws_disagg.CSV')
write.csv(draws.sf12.disagg,'res_mat_sf12_draws_disagg.CSV')
