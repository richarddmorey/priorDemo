require(lattice)
require(stats)
require(shiny)
require("RColorBrewer")
require("SVGAnnotation")
require("XML")
require(grImport)
require(shinyjs)
require("gridSVG")

options(shiny.usecairo=FALSE)

source('utility.R')



shinyServer(function(input,output,session) {

  getLimits = function(){
    if(input$whichRange == "Full") return(c(-Inf,Inf))
    if(input$whichRange == "Positive") return(c(0,Inf))
    if(input$whichRange == "Negative") return(c(-Inf,0))
    if(input$whichRange == "Custom") return(as.numeric(c(input$range.lower,input$range.upper)))
  }

  getLimits.latex = function(){
    if(input$whichRange == "Full") return(c("-\\infty","\\infty"))
    if(input$whichRange == "Positive") return(c("0","\\infty"))
    if(input$whichRange == "Negative") return(c("-\\infty","0"))
    if(input$whichRange == "Custom") lims = c(input$range.lower,input$range.upper)
    if(lims[1]=="-Inf") lims[1] = "-\\infty"
    if(lims[2]=="Inf" | lims[2]=="+Inf") lims[2] = "\\infty"
    return(lims)
  }


  output$statString <- reactive({
    N = as.numeric(input$data.N)
    if(input$whichData=="t statistic"){
      t.stat = as.numeric(input$data.stat)
      eff.size = t.stat / sqrt(N)
      return(paste("Cohen's d: ",round(eff.size,3),sep=""))
    }else{
      eff.size = as.numeric(input$data.stat)
      t.stat = eff.size * sqrt(N)   
      return(paste("t statistic: ",round(t.stat,3),sep=""))   
    }
  })

  output$bfString <- reactive({
    prior.lims = getLimits()
    N = as.numeric(input$data.N)
    if(input$whichData=="t statistic"){
      t.stat = as.numeric(input$data.stat)
      eff.size = t.stat / sqrt(N)
    }else{
      eff.size = as.numeric(input$data.stat)
      t.stat = eff.size * sqrt(N)   
    }
    if(input$whichPrior == "point"){
      alt.marg.like = dt(t.stat,df=N-1,ncp=as.numeric(input$point.delta)*sqrt(N))
    }else if(input$whichPrior == "normal"){
      mu = as.numeric(input$normal.mu)
      rscale = as.numeric(input$normal.r)
      alt.marg.like = dy.t(t.stat,N=N,rscale,location=mu,df=Inf, lo=prior.lims[1],up=prior.lims[2])
    }else if(input$whichPrior == "cauchy"){
      mu = as.numeric(input$normal.mu)
      rscale = as.numeric(input$normal.r)
      alt.marg.like = dy.t(t.stat,N=N,rscale,location=mu,df=1, lo=prior.lims[1],up=prior.lims[2])
    }else if(input$whichPrior == "gamma"){
      shape = as.numeric(input$gamma.shape)
      rscale = as.numeric(input$gamma.r)
      alt.marg.like = dy.gamma(t.stat,N=N,rscale,shape,lo=prior.lims[1],up=prior.lims[2])
    }
    B01 = dt(t.stat,N-1)/alt.marg.like
    if(B01>1){
      return(paste("B01: ",round(B01,3),sep=""))
    }else{
      return(paste("B10: ",round(1/B01,3),sep=""))
    }
  })

  output$prior.out <- renderUI({
    prior.lims = getLimits.latex()

    if(input$whichPrior == "point"){
      model.string = paste("\\delta = ",input$point.delta,sep="")
    }else if(input$whichPrior == "normal"){
      mu = round(as.numeric(input$normal.mu),3)
      rscale = as.numeric(input$normal.r)
      sigma = round(rscale/qnorm(.75,0,1),3)
      model.string = paste("\\delta\\sim \\mbox{Normal}(\\mu=",mu,",\\sigma=",sigma,"), ",prior.lims[1],"\\leq\\delta<",prior.lims[2],sep="")
    }else if(input$whichPrior == "cauchy"){
      mu = round(as.numeric(input$normal.mu),3)
      rscale = round(as.numeric(input$normal.r),3)
      model.string = paste("\\delta\\sim \\mbox{Cauchy}(\\mbox{location}=",mu,", \\mbox{scale}=",rscale,"), ",prior.lims[1],"\\leq\\delta<",prior.lims[2],sep="")
    }else if(input$whichPrior == "gamma"){
      shape = as.numeric(input$gamma.shape)
      rscale = as.numeric(input$gamma.r)
      scale = round(abs(rscale)/qgamma(.5,shape=shape),3)
      shape = round(shape,3)
      model.string = paste("\\delta\\sim \\mbox{double gamma}(\\mbox{shape}=",shape,", \\mbox{scale}=",scale,"), ",prior.lims[1],"\\leq\\delta<",prior.lims[2],sep="")
    }
    str1 = paste("The current alternative model is:
      $$
      \\begin{eqnarray*}
      ",model.string,"
      \\end{eqnarray*}
      $$")
    withMathJax(helpText(str1))
  })

  

  output$svg.grid <- renderText({
    #from lattice package documentation
    
    nullCol = "red"
    altCol = "blue"  
    
    prior.lims = getLimits()
    N = as.numeric(input$data.N)
    if(input$whichData=="t statistic"){
      t.stat = as.numeric(input$data.stat)
      eff.size = t.stat / sqrt(N)
    }else{
      eff.size = as.numeric(input$data.stat)
      t.stat = eff.size * sqrt(N)      
    }


    doc = svgPlot( {
      
      par(mfrow=c(2,1),mar=c(4,4,1,.1),mgp=c(2.5,1,0))

      if(input$whichPrior == "point"){
        
        ### Plot 1
        plot(c(0,input$point.delta),c(1.15,1.15),ty='h',col=c(nullCol,altCol),
          axes=FALSE, ylab="Prior density",xlab="True effect size",ylim=c(0,1.3))
        axis(1,at=c(0,input$point.delta))
        points(c(0,input$point.delta),c(1.15,1.15),,col=c(nullCol,altCol),cex=2,pch=19)
        box()
        abline(h=0,col="gray")

        ### Plot 2
        alt.Rng = qt(c(.005,.995),df=N-1,ncp=as.numeric(input$point.delta)*sqrt(N))
        null.Rng = qt(c(.005,.995),df=N-1)
        Rng = range(c(alt.Rng,null.Rng,t.stat))

        xx = seq(Rng[1],Rng[2],len=200)
        if(input$whichData=="t statistic"){
          plot(xx,dt(xx,N-1),col=nullCol,ty='l',axes=FALSE,ylab="Prior pred. density",xlab="t statistic")
          lines(xx,dt(xx,N-1,ncp=as.numeric(input$point.delta)*sqrt(N)),col=altCol)
        }else{
         plot(xx/sqrt(N),dt(xx,N-1)*sqrt(N),col=nullCol,ty='l',axes=FALSE,ylab="Prior pred. density",xlab="Cohen's d")
         lines(xx/sqrt(N),dt(xx,N-1,ncp=as.numeric(input$point.delta)*sqrt(N))*sqrt(N),col=altCol)
        }
        box()
        abline(h=0,col="gray")
        abline(v=as.numeric(input$data.stat),col="black",lwd=2,lty=2)
        axis(1)

      }else if(input$whichPrior == "normal"){
        
        ### Plot 1
        mu = as.numeric(input$normal.mu)
        rscale = as.numeric(input$normal.r)
        sigma = rscale/qnorm(.75,0,1)
        samps = qnorm(runif(100000,pnorm(prior.lims[1],mu,sigma),pnorm(prior.lims[2],mu,sigma)),mu,sigma)
        plt.lims = quantile(samps,c(.005,.995))
        xx = seq(plt.lims[1]-.1*diff(plt.lims),plt.lims[2]+.1*diff(plt.lims),len=100)
        yy = dnorm.prior(xx,rscale,location=mu,lo=prior.lims[1],up=prior.lims[2],return.log=FALSE)

        if(min(plt.lims)>0) plt.lims[1] = 0
        if(max(plt.lims)<0) plt.lims[2] = 0

        plot(xx,yy,ty='l',col=altCol,
          axes=FALSE, ylab="Prior density",xlab="True effect size",ylim=c(0,max(yy)*1.3),xlim=plt.lims)
        segments(0,max(yy)*1.15,0,0,col=nullCol)
        points(0,max(yy)*1.15,col=nullCol,cex=2,pch=19)
        axis(1)
        box()
        abline(h=0,col="gray")

        ### Plot 2
        alt.Rng = quantile(rt(samps,N-1,ncp=samps*sqrt(N)),c(.005,.995))
        null.Rng = qt(c(.005,.995),df=N-1)
        Rng = range(c(alt.Rng,null.Rng,t.stat))

        xx = seq(Rng[1],Rng[2],len=200)
        if(input$whichData=="t statistic"){
          plot(xx,dt(xx,N-1),col=nullCol,ty='l',axes=FALSE,ylab="Prior pred. density",xlab="t statistic")
          lines(xx,dy.t(xx,N,rscale,location=mu,df=Inf,lo=prior.lims[1],up=prior.lims[2],return.log=FALSE),col=altCol)
        }else{
         plot(xx/sqrt(N),dt(xx,N-1)*sqrt(N),col=nullCol,ty='l',axes=FALSE,ylab="Prior pred. density",xlab="Cohen's d")
         lines(xx/sqrt(N),dy.t(xx,N,rscale,location=mu,df=Inf,lo=prior.lims[1],up=prior.lims[2],return.log=FALSE)*sqrt(N),col=altCol)
        }


        box()
        abline(h=0,col="gray")
        abline(v=as.numeric(input$data.stat),col="black",lwd=2,lty=2)
        axis(1)

      }else if(input$whichPrior == "cauchy"){
        
        ### Plot 1
        mu = as.numeric(input$cauchy.mu)
        rscale = as.numeric(input$cauchy.r)
        sigma = rscale
        samps = qcauchy(runif(100000,pcauchy(prior.lims[1],mu,sigma),pcauchy(prior.lims[2],mu,sigma)),mu,sigma)
        plt.lims = quantile(samps,c(.05,.95))
        xx = seq(plt.lims[1]-.1*diff(plt.lims),plt.lims[2]+.1*diff(plt.lims),len=200)
        yy = dt.prior(xx,rscale,location=mu,df=1,lo=prior.lims[1],up=prior.lims[2],return.log=FALSE)

        if(min(plt.lims)>0) plt.lims[1] = 0
        if(max(plt.lims)<0) plt.lims[2] = 0

        plot(xx,yy,ty='l',col=altCol,
          axes=FALSE, ylab="Prior density",xlab="True effect size",ylim=c(0,max(yy)*1.3),xlim=plt.lims)
        segments(0,max(yy)*1.15,0,0,col=nullCol)
        points(0,max(yy)*1.15,col=nullCol,cex=2,pch=19)
        axis(1)
        box()
        abline(h=0,col="gray")

        ### Plot 2
        alt.Rng = quantile(rt(samps,N-1,ncp=samps*sqrt(N)),c(.05,.95))
        null.Rng = qt(c(.005,.995),df=N-1)
        Rng = range(c(alt.Rng,null.Rng,t.stat))

        xx = seq(Rng[1],Rng[2],len=200)
        if(input$whichData=="t statistic"){
          plot(xx,dt(xx,N-1),col=nullCol,ty='l',axes=FALSE,ylab="Prior pred. density",xlab="t statistic")
          lines(xx,dy.t(xx,N,rscale,location=mu,df=1,lo=prior.lims[1],up=prior.lims[2],return.log=FALSE),col=altCol)
        }else{
         plot(xx/sqrt(N),dt(xx,N-1)*sqrt(N),col=nullCol,ty='l',axes=FALSE,ylab="Prior pred. density",xlab="Cohen's d")
         lines(xx/sqrt(N),dy.t(xx,N,rscale,location=mu,df=1,lo=prior.lims[1],up=prior.lims[2],return.log=FALSE)*sqrt(N),col=altCol)
        }


        box()
        abline(h=0,col="gray")
        abline(v=as.numeric(input$data.stat),col="black",lwd=2,lty=2)
        axis(1)


      }else if(input$whichPrior == "gamma"){

        ### Plot 1
        shape = as.numeric(input$gamma.shape)
        rscale = as.numeric(input$gamma.r)
        sigma = rscale/qgamma(.5,shape=shape)
        samps = rgamma.prior(100000,rscale,shape,lo=prior.lims[1],up=prior.lims[2])
        plt.lims = quantile(samps,c(.005,.995))
        xx = seq(plt.lims[1]-.1*diff(plt.lims),plt.lims[2]+.1*diff(plt.lims),len=200)
        yy = dgamma.prior(xx,rscale,shape=shape,lo=prior.lims[1],up=prior.lims[2],return.log=FALSE)

        if(min(plt.lims)>0) plt.lims[1] = 0
        if(max(plt.lims)<0) plt.lims[2] = 0

        plot(xx,yy,ty='l',col=altCol,
          axes=FALSE, ylab="Prior density",xlab="True effect size",ylim=c(0,max(yy)*1.3),xlim=plt.lims)
        segments(0,max(yy)*1.15,0,0,col=nullCol)
        points(0,max(yy)*1.15,col=nullCol,cex=2,pch=19)
        axis(1)
        box()
        abline(h=0,col="gray")

        ### Plot 2
        alt.Rng = quantile(rt(samps,N-1,ncp=samps*sqrt(N)),c(.005,.995))
        null.Rng = qt(c(.005,.995),df=N-1)
        Rng = range(c(alt.Rng,null.Rng,t.stat))

        xx = seq(Rng[1],Rng[2],len=200)
        if(input$whichData=="t statistic"){
          plot(xx,dt(xx,N-1),col=nullCol,ty='l',axes=FALSE,ylab="Prior pred. density",xlab="t statistic")
          lines(xx,dy.gamma(xx,N,rscale,shape=shape,lo=prior.lims[1],up=prior.lims[2],return.log=FALSE),col=altCol)
        }else{
         plot(xx/sqrt(N),dt(xx,N-1)*sqrt(N),col=nullCol,ty='l',axes=FALSE,ylab="Prior pred. density",xlab="Cohen's d")
         lines(xx/sqrt(N),dy.gamma(xx,N,rscale,shape=shape,lo=prior.lims[1],up=prior.lims[2],return.log=FALSE)*sqrt(N),col=altCol)
        }


        box()
        abline(h=0,col="gray")
        abline(v=as.numeric(input$data.stat),col="black",lwd=2,lty=2)
        axis(1)
      }



    }, height = 5, width = 6, pointsize = 10)  
    
    tempsvg <- tempfile(fileext=".svg")
    on.exit(unlink(tempsvg))
    saveXML(doc, tempsvg)
    svgoutput <- readLines(con = tempsvg, n=-1)
    svgoutput
  }) 



})