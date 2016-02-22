
rgamma.prior<-function(N,rscale=sqrt(2)/2,shape=3,lo=-Inf,up=Inf){
  scale = abs(rscale)/qgamma(.5,shape=shape)
  if(sign(lo)==sign(up) | !sign(lo) | !sign(up)){
    if(sign(up))
      samps = qgamma(runif(N,pgamma(lo,scale=scale,shape=shape),pgamma(up,scale=scale,shape=shape)),scale=scale,shape=shape)
    if(sign(lo)==-1)
      samps = -qgamma(runif(N,pgamma(abs(up),scale=scale,shape=shape),pgamma(abs(lo),scale=scale,shape=shape)),scale=scale,shape=shape)
  }else{
    aNeg = pgamma(abs(lo),scale=scale,shape=shape)
    aPos = pgamma(abs(up),scale=scale,shape=shape)
    probPos = aPos / (aNeg + aPos)
    sgn = rbinom(N,1,probPos)
    samps = (sgn*2-1)*qgamma(runif(N,0,pgamma(abs(c(lo,up))[sgn+1],scale=scale,shape=shape)),scale=scale,shape=shape)
  }
  return(samps)
}

dgamma.prior<-Vectorize(function(x,rscale=sqrt(2)/2,shape=3,lo=-Inf,up=Inf,return.log=FALSE)
{
  if(lo>=up) stop("Invalid limits.")
  rscale = abs(rscale)
  if(x<lo | x>up) return(log(!return.log)) 
  scale = rscale/qgamma(.5,shape=shape)
  if(sign(lo)==sign(up) | !sign(lo) | !sign(up)){
    log.normalize = log(abs(pgamma(abs(up), scale = scale, shape = shape) - pgamma(abs(lo), scale = scale, shape = shape)))
  }else{
    log.normalize = log(pgamma(abs(up), scale = scale, shape = shape) + pgamma(abs(lo), scale = scale, shape = shape))
  }
  log.dens = dgamma(abs(x), scale = scale, shape = shape, log = TRUE) - log.normalize 
  if(return.log){
    return(log.dens)
  }else{
    return(exp(log.dens))
  }
},"x")


dnorm.prior<-Vectorize(function(x,rscale=sqrt(2)/2,location=0,lo=-Inf,up=Inf,return.log=FALSE)
{
  dt.prior(x,rscale,location,Inf,lo,up,return.log)
},"x")

dt.prior<-Vectorize(function(x,rscale=sqrt(2)/2,location=0,df=1,lo=-Inf,up=Inf,return.log=FALSE)
{
  if(lo>=up) stop("Invalid limits.")
  rscale = abs(rscale)
  if(x<lo | x>up) return(log(!return.log)) 
  scale = rscale/qt(.75,df=df)
  log.normalize = log(pt( (up - location)/scale, df) - pt( (lo - location)/scale, df))
  log.dens = dt( (x - location)/scale, df, log = TRUE) - log(scale) - log.normalize
  if(return.log){
    return(log.dens)
  }else{
    return(exp(log.dens))
  }
},"x")


dy.gamma = Vectorize(function(t,N,rscale=sqrt(2)/2,shape=3,lo=-Inf,up=Inf,return.log=FALSE){
  integrate(function(delta,...)
    suppressWarnings(exp(dt(t,N-1,ncp = delta*sqrt(N),log=TRUE) + dgamma.prior(delta,rscale,shape,lo,up,TRUE))),
  lower=lo,upper=up,rscale=rscale,shape=shape,N=N,t=t,lo=lo,up=up)[[1]]
},"t")

dd.gamma = function(delta,N,rscale=sqrt(2)/2,shape=3,lo=-Inf,up=Inf,return.log=FALSE){
  dy.gamma(delta*sqrt(N),N,rscale,shape,lo,up,return.log)*sqrt(N)
}

dy.t = Vectorize(function(t,N,rscale=sqrt(2)/2,location=0,df=1,lo=-Inf,up=Inf,return.log=FALSE){
  integrate(function(delta,...)
    suppressWarnings(exp(dt(t,N-1,ncp = delta*sqrt(N),log=TRUE) + dt.prior(delta,rscale,location,df,lo,up,TRUE))),
    lower=lo,upper=up,rscale=rscale,location=location,df=df,N=N,t=t,lo=lo,up=up)[[1]]
},"t")

dd.t = function(delta,N,rscale=sqrt(2)/2,location=0,df=1,lo=-Inf,up=Inf,return.log=FALSE){
  dy.t(delta*sqrt(N),N,rscale,location,df,lo,up,return.log)*sqrt(N)
}

#tt = seq(-5,30,len=50)/sqrt(50)
#plot(tt,dd.t(tt,50,lo=0),ty='l')

#dd = rcauchy(10000,scale=sqrt(2)/2)
#ts = rt(10000,49,dd*sqrt(50))
#hist(ts/sqrt(50),freq=FALSE)
#tt = seq(-5,30,len=50)/sqrt(50)
#lines(tt,dd.t(tt,50,lo=0),ty='l',col="red")


