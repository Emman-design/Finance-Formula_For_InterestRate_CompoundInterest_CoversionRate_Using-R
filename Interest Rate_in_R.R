library(ILSR)
#Copy and Run this function, an example of how to use it is included at the end of the program
Interestmodel<-function(FV,PV,r,t){
  if (missing(PV)) {
    PV<-FV/(1+r*t)
  }
  else if (missing(FV)) {
    FV<-PV*(1+r*t)
  }
  else if (missing(r)) {
    r<-((FV/PV)-1)/t
  }
  else if (missing(t)) {
    t<-((FV/PV)-1)/r
  }
  Interest<- PV*r*t
  list(FV=FV, PV= PV, r = r, t= t, Interest= Interest)
  #######How to use is below
  #######Interestmodel(FV=1250,r=0.05,t=5)
}
