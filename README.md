# Le-Monde-puzzle-1134



    If one replaces a pair (a,b) of integers with the pair (g,s) of their greatest common denominator and smallest common multiple, how long at most before the sequence ends. Same question when considering a collection of five integers where two are selected by the pair (g,s) of their greatest common denominator and smallest common multiple.

The first question is straightforward as s is a multiple of s. So the sequence ends at most after one run. For five, run of a brute force R search return 9 as “the” solution (even though the true maximum is 10, as illustrated by the quintuplet (16,24,36,54,81):

    ogcd <- function(x,y){r<-x%%y
      return(ifelse(r,ogcd(y,r),y))}

    oscm<-function(x,y) x*y/ogcd(x,y)

    divemul<-function(a,b) return(c(oscm(a,b),ogcd(a,b)))

    for (t in 1:1e5){
    ini=sample(1:1e2,5)
    i=0;per=ker=sample(ini,2)
    nez=divemul(per[1],per[2])
    while(!max(nez%in%per)){
     ini=c(ini[!ini%in%per],nez)
     per=sample(ini,2)
     ker=rbind(ker,per)
     nez=divemul(per[1],per[2])
     i=i+1}
     sol=max(sol,i)}

