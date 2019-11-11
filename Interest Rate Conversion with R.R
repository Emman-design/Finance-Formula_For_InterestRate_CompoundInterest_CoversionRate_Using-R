rateconvertmodel<-function(a,n){
  x=menu(c("effective interest rate i","nominal interest rate im", "effective discount rate d", "nominal 
           discount rate dm", "force of interest z"), title="Choose the given interest rate in your question?") 
  if(x==1){
    i=a 
    im = n*((1+i)^(1/n)-1) 
    d = i/(1+i) 
    dm = n*(1-(1-d)^(1/n)) 
    z = log(1+i) 
  } 
  else if(x==2){
    im = a 
    i = (1+(im/n))^n -1 
    d = i/(1+i) 
    dm = n*(1-(1-d)^(1/n)) 
    z = log(1+i) 
  }
  else if(x==3){
    d = a 
    i = d/1-d 
    im = n*((1+i)^(1/n)-1) 
    dm = n*(1-(1-d)^(1/n)) 
    z = log(1+i) } 
  else if(x==4){
    dm = a 
    d = 1-(1-(dm/n))^n 
    i = d/1-d 
    im = n*((1+i)^(1/n)-1) 
    z = log(1+i) 
  } 
  else if(x==5){
    z = a 
    i = exp(z)-1 
    im = n*((1+i)^(1/n)-1) 
    d = i/(1+i) 
    dm = n*(1-(1-d)^(1/n)) 
  } 
  list("effective interest rate" =i , "nominal interest rate" = im, "effective discount rate" =d, "nominal discount rate" = dm, "force of interest" = z)
  #### Copy and paste the below example to see how this works 
  ####This model takes in any given rate and convert it  
  ####to other form of rates by given the number of compound per year 
  ####rateconvertmodel(0.066,12) 
} 
