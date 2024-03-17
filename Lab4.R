#n is the number of trials
#p is the probability of success in each trial
#m3 is skewness (1st column)
#m4 is kurtosis (2nd column)
m34binomial = function(n,p){
  q = 1-p
  den = n*p*q
  m3 = (q-p)/sqrt(den)
  m4 = 3 + (1-6*p*q)/den
  cbind(m3, m4)
}


#r: experiment continues until r successes 
#p is the probability of success in each trial
#m3 is skewness (1st column)
#m4 is kurtosis (2nd column)
m34negbinomial = function(r,p){
  q = 1-p
  qr=q*r
  m3 = (1+q)/sqrt(qr)
  m4 = 3 + 6/r + p^2/qr
  cbind(m3, m4)
}

#computes the distance between Binom(n,p) and Hyper(b, t-b, n) with b = t*p
#plots the two pmfs for points around the mean = n*p
#blue dots are the binomial pmf and orange dots are the hypergeometric pmf
distBinomHyper = function(n,p,t){
  x=0:n
  b = floor(t*p)
  pr1=dbinom(x,n,p)
  pr2=dhyper(x,b,t-b,n)
  D = 0.5*sum(abs(pr1-pr2))
  #define the x values around the mean plus minus 5 st. deviations of the 
  #binomial distribution
  i.u=x < n*p + 5*sqrt(n*p*(1-p))
  i.l=x > n*p - 5*sqrt(n*p*(1-p))
  i.plot=i.u & i.l
  #round the distance down to first 3 digits after decimal point
  D1=floor(1000*D)/1000
  #plot the two pmf-s for x values around the mean plus minus 5 st. dev
  #plot(x[i.plot],pr2[i.plot],type="p",col="orange",xlab="x", ylab="pmf", main=paste("D = ", D1), cex.main=1,lwd=2,cex=.5,ylim=c(0,max(c(pr1,pr2))))
  plot(x[i.plot],pr2[i.plot],type="p",col="orange",xlab="x", ylab="pmf", main=paste("n= ", n, " p= ", p, "  t= ", t, " D= ", D), cex.main=1,lwd=2,cex=.5,ylim=c(0,max(c(pr1,pr2))))
  lines(x[i.plot],pr1[i.plot],type="p",col="blue",lwd=2,cex=.5)
  D
}


#1a
ns = c(1, 10, 50, 100, 250, 500)
ps = c(0.1, 0.3, 0.5, 0.7, 0.9)
create_distribution_table = function(ns,ps,m,func){
  values = c()
  for (n in ns) {
    values = append(values, func(n, ps)[,m-2])
  }
  d_matrix = matrix(values, ncol=5, byrow =TRUE)
  colnames(d_matrix) = ps
  rownames(d_matrix) = ns
  return(as.table(d_matrix))
}

print("== Skewness binomial ==")
create_distribution_table(ns,ps,3,m34binomial)


print("== Kurtosis binomial ==")
create_distribution_table(ns,ps,4,m34binomial)

#1b
#we notice that for skewness its mirrored on 0.5 but negative. as in the skewness of 1-p for any given n is equal to
# the negative of the skewness of p. For kurtosis, the kurtosis of 1-p is equal to that of p. (but why?) From this we notice
# that it somewhat of a downward (negative) slope (maybe cubic?) and for higher n the slope is more shallow
# For Kurtosis, values near p=0 and 1 are higher than p=0.5 so it seems to be more a positive quadratic curve

#1c
print("== Skewness negative binomial ==")
create_distribution_table(ns,ps,3,m34negbinomial)


print("== Kurtosis negative binomial ==")
create_distribution_table(ns,ps,4,m34negbinomial)

#1d
# across rows we notice that the value gets larger and get larger faster as p gets closer to one so in may resemble an 
# exponential curve.This is the case for both skewness and kurtosis.
# again as n gets larger the slope is gentler


#2a
ps = c(0.1, 0.3, 0.5, 0.7, 0.9)
n = c(500, 1000, 3000)
create_distBinormHyper_table = function(n,ps){
  values = c()
  ts = c(n+100, n+500, n+1500, n+2000, n+10000, 16000000)
  for (t in ts) {
    for (p in ps)
    values = append(values, distBinomHyper(n,p,t))
    #plot.new()
  }
  d_matrix = matrix(values, ncol=5, byrow =TRUE)
  colnames(d_matrix) = ps
  rownames(d_matrix) = ts
  return(as.table(d_matrix))

}
create_distBinormHyper_table(500, ps)
create_distBinormHyper_table(1000, ps)
create_distBinormHyper_table(3000, ps)

#2b the curve resembels a normal distribution with 















