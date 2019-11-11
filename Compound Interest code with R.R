compoundmodel<-function(FV,PV,i,t,m){ 
  x=menu(c("nominal rate", "discount rate", "effective rate"), title="The given interest is ?") 
  if(x==1){ 
    i<- (1 + (i/m))^(m)-1 
    compoundmodel1(FV,PV,i,t,m) 
  } 
  else if(x==2){ 
    i<- (1 + (i))^(m)-1 
    compoundmodel1(FV,PV,i,t,m) 
  } 
  else if(x==3){ 
    compoundmodel1(FV,PV,i,t,m) 
  } 
} 
compoundmodel1<-function(FV,PV,i,t,m){ 
  if (missing(PV)) {
    PV<- FV*(1+i)^(-t)
  }   
  else if (missing(i)) { 
    i<-(FV/PV)^(1/t) - 1   
  }   
  else if (missing(t)) {
    t<- log(FV/PV)/log(1+i)   
  }   
  else if (missing(FV)) {   
    FV<- PV*(1+i)^t   
  } 
  print('This model converted any given type of i to effective interest rate before use') 
  print('These are all the Values of variables associated with this model') 
  list("Future Value" =FV, "Present Value"= PV, "Effective Interest rate" = i, "Time" = t, "frequent of compounding" = m) 
  ##### t is the period of compound, m is the frequent of compounding per period. 
  ##### And of course FV is Future Value, i is the interest given in question 
  #####compoundmodel(FV=2431.174,i=0.02,t=5,m=12) 