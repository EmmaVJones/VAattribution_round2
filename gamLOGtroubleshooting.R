tolerance <- function(spdata, envdata,  sp.siteid="Sample.ID", species="GENUS", 
                      sp.abndid="RA", env.siteid="Sample.ID", xvar="cond", 
                      cutoff = 20, cutoff2 = 10, region = "all", lim ="GAM",
                      coord = NULL, mtype = 3, dense.N = 201, cast = TRUE,  
                      plot.pdf = F, add.map=F,statename = NULL,  add.lab = F, 
                      add.abund = T, ylab1 = "Capture Probability",  
                      ylab2 = "Relative Abundance", 
                      main = "Capture Probability of Macroinvertebrate Taxon Along Conductivity Gradient", 
                      mar = c(5,4,3, 4), 
                      xlabs=expression(paste("Conductivity ( ", mu, "S/cm)")), 
                      log.x = TRUE, base = 10, plus = F, rounder = 2, taus = c(50,95), 
                      nbin = 60,
                      emmaLOG = FALSE) {
  
  print(xvar)
  dfenv <- envdata[!is.na(envdata[,xvar]), c(env.siteid, xvar, coord)] 
  if(cast) { 
    df.sp <- data.frame(SITEID = spdata[, sp.siteid], species = spdata[, species],
                        abund = spdata[, sp.abndid]) 
    df.sp <- subset(df.sp, !is.na(species) & !is.na(abund))
    df.tmp <- merge(df.sp, dfenv, by.x = "SITEID", by.y = env.siteid )
    ss <- reshape::cast(df.tmp, SITEID ~ species, sum, value = "abund") 
    names(ss)[1] <- sp.siteid
  } else ss <- spdata
  #    print(names(ss))
  df1 <- merge(ss, dfenv, by.x = sp.siteid, by.y = env.siteid)   # merged crosstab abundance file
  nc <- ncol(df1)
  taxa.count <- apply(df1[,(2:ncol(ss))] > 0, 2, sum)
  #    print((taxa.count))
  tnames <- names(taxa.count)[taxa.count >= cutoff2] 
  ntaxa <- length(tnames)                           # length of taxa list for taxonomic level i
  
  df2 <- df1 <- df1[order(df1[,xvar]),]
  
  df2[2:(nc-ncol(dfenv) + 1)] <- apply(df2[2:(nc-ncol(dfenv) + 1 )] > 0, 2, as.numeric)                 # p/a file
  
  stress <- df1[, xvar]
  stress.u <- sort(unique(stress))
  
  xrange <- range(stress)
  xnew <- seq(from = xrange[1], to = xrange[2], length = 100)
  
  ###### calculate weight for cdf 
  
  cutp <- seq(from = min(stress), to = max(stress), length = nbin)
  cutm <- 0.5*(cutp[-1] + cutp[-nbin])
  
  df2$cutf <- cut(df2[,xvar], cutp, include.lowest = T)
  
  wt <- 1/table(df2$cutf)
  wt[is.infinite(wt)] <- NA
  wt <- wt/sum(wt, na.rm = T)
  dftemp <- data.frame(cutf = names(wt), wt = as.vector(wt))
  df2 <- merge(df2, dftemp, by = "cutf")[-1]
  df2 <- df2[order(df2[,xvar]),]  
  df1 <- df1[order(df1[,xvar]),] 
  
  #### end 
  ##### function to compute area under the curve
  auc <- function(xrange, mod, dense.N) {
    x <- seq(min(xrange), max(xrange), length = dense.N - 1)
    s.area <-rep(NA, dense.N - 1)
    y <- predict(mod, newdata = data.frame(dose = x), type = "response")
    for (index in 1:dense.N-1) {
      s.area[index] <- (y[index] + y[index + 1])/2*(x[index+1]-x[index])
    }
    tsum <- sum(s.area, na.rm=T)         # all area
    jj=1
    csum = sum(s.area[1:jj])              # cum area
    xc95 <- yc95 <- rep(NA, length(taus))
    for(ii in 1:length(taus)) {
      while(csum < taus[ii]/100*tsum) {
        jj = jj + 1
        csum <- sum(s.area[1:jj], na.rm=T)
      }
      if(jj == 1) {
        xc95[ii] <- x[jj]
        yc95[ii] <- y[jj]
      } else {
        xc95[ii] <- (x[jj]+ x[jj-1])/2  
        yc95[ii] <- (y[jj]+ y[jj-1])/2
      }
    }
    return(c(xc95))
    #      print(xc95)
  } 
  # plot pdf option
  #if(plot.pdf) {
  #  pdf(file = paste("data/", region, ".taxon.gam.pdf",sep=""),
  #      width = 9, height = 6.5, pointsize = 12)
  #  par(mfrow = c(2, 3), pty = "m", mar =mar, oma=c(1.5, 0.5, 2,0.5) )
  #  totpage <- ceiling((length(tnames)+1)/6)
  #  
  #  if(add.map) {
  #    simpleCap <- function(x) {
  #      s <- strsplit(x, " ")[[1]]
  #      paste(toupper(substring(s, 1,1)), substring(s, 2),
  #            sep="", collapse=" ")
  #    }
  #    med = 1
  #    while(is.na(df1[med, coord[1]])) med = 1 + med
  #    
  #    if(is.null(statename)) {
  #      statename <<- map.where("state", df1[med, coord[1]], df1[med, coord[2]])
  #      statename <- simpleCap(statename)
  #    } 
  #    if(add.lab) {
  #      map.text("state", region = statename, mar = mar)
  #    } else {
  #      map("state", regions = statename, mar = mar)
  #    }
  #    
  #    text(df1[,coord[1]],df1[,coord[2]],".")
  #    #       mtext(paste("Ecoregion",region), side =3,line = 0.5, cex=1.5)  
  #  }
  #}
  
  varnames <- c("N", "min_ob", paste(taus, "th_ob",sep="_"), "max_ob", "Opt_WA",
                "Tol_WA", paste("CDF", taus,"th","Abund", sep ="_"),
                paste("CDF", taus,"th","PA", sep ="_"),paste("CDF_wt", taus,"th",sep ="_"), 
                paste("LRM", taus,"th", sep ="_"),paste("QLRM", taus,"th", sep ="_"),
                "Opt_qlrm", "Tol_qlrm", paste("GAM",taus, "th", sep="_"), "ROC")
  
  optsave <- matrix(NA, ncol = length(taus)*7 + 8, nrow  = ntaxa)
  colnames(optsave) <- varnames
  rownames(optsave) <- tnames
  
  for (i in 1:ntaxa) {
    isel <- match(tnames[i], names(df1))       # selected taxa i
    #    print(df1[,isel])
    print(tnames[i])
    ### (0) observed range
    samplen <- length(df2[df2[,isel]>0,xvar])          
    limits <- quantile(df2[df2[,isel]>0,xvar], prob = c(0, taus/100, 1))       
    
    ## (1)  Weighted averaging 
    WA <- sum(df1[,isel]*df1[,xvar])/sum(df1[,isel]) 
    tol <- sqrt(sum(df1[,isel] * (df1[,xvar]- WA)^2) /sum(df1[,isel])) 
    #    WTA <- sum(df1[,isel]*df1[,xvar]*df2$wt)/sum(df1[,isel]*df2$wt)     # sample weighted weighted average         
    
    ### (2)  cdf or cumsum no weighting  both abundance based and p/a based
    csum1 <-cumsum(df1[,isel])/sum(df1[,isel])          # cumlative vectors devided total abundance 
    #      print(sum(df1[,isel]))
    ic1 <- rep(1, length(taus))
    for(ii in 1:length(taus)) {
      #  print(csum1)
      if(is.finite(csum1[ic1[ii]])) {
        while(csum1[ic1[ii]] < taus[ii]/100) ic1[ii] <- ic1[ii] + 1
      } else csum1 <- NA
    }
    #### (3) cdf p/a        
    csum2 <-cumsum(df2[,isel])/sum(df2[,isel])    # cumlative vectors devided total abundance 
    ic2 <- rep(1, length(taus))
    for(ii in 1:length(taus)) {
      while(csum1[ic2[ii]] < taus[ii]/100) ic2[ii]<- ic2[ii] + 1
    }
    
    ##### (4) cdf weighted 
    pres <- df2[, isel] > 0
    sel <- df2[pres,]
    eout2 <- rep(NA, length(taus))
    for(ii in 1:length(taus)) {
      eout2[ii] <- Hmisc::wtd.quantile(sel[,xvar], sel$wt, normwt = TRUE, prob = taus[ii]/100)  
    }
    
    ######### (5)(6)(7) logistic regression model          
    resp <<- df2[, isel] 
    dose <<- df2[, xvar]
    lrm1 <- glm(resp ~ dose, family="binomial")
    lrm2 <- glm(resp ~ dose + I(dose^2), family="binomial")
    #### u = -b1/(2b2)  t = 1/sqrt(-2b2) for lrm2  
    lrm2.opt <- (-lrm2$coef[2]/(2*lrm2$coef[3])  )
    lrm2.tol <- 1/sqrt(-2*as.vector(lrm2$coef[3]))
    s=mgcv:::s
    
    lrm3 <- try(mgcv::gam(resp ~ s(dose,k = 3), family= "binomial"),TRUE)
    
    if(isTRUE(class(lrm3)=="try-error")) { next } else{  ############################ ERROR HANDLING INSERTED BY EVJ 02062018
      
      if(mtype == 1) model <- lrm1
      if(mtype == 2) model <- lrm2
      if(mtype == 3) model <- lrm3
      
      # Compute mean predicted probability of occurrence
      predlk <- predict(model, type = "link")
      predout <- exp(predlk)/(1 + exp(predlk))
      x <- predout[pres]
      y <- predout[!pres]
      rocmat <- matrix(NA, nrow = length(x), ncol = length(y))
      for (j in 1:length(x)) {
        rocmat[j,] <- as.numeric(x[j] > y)
      }
      # Summarize all comparisons to compute area under ROC
      roc <- sum(rocmat)/(length(x)*length(y))
      #        wilcox <- wilcox.test(x, y, conf.int = FALSE)
      predresp <- predict(model, newdata = data.frame(dose=xnew), type="link", se.fit=T)
      
      # Compute upper and lower 90% confidence limits
      up.bound.link <- predresp$fit + qnorm(0.95)*predresp$se.fit
      low.bound.link <- predresp$fit + qnorm(0.05)*predresp$se.fit
      mean.resp.link <- predresp$fit
      
      # Convert from logit transformed values to probability.
      up.bound <- exp(up.bound.link)/(1+exp(up.bound.link))
      low.bound <- exp(low.bound.link)/(1+exp(low.bound.link))
      mean.resp <- exp(mean.resp.link)/(1+exp(mean.resp.link))
      
      #### 5,6,7 full range
      ######## find modeled xc95 full data range
      lrm1.95f <- auc(xrange = xrange, mod = lrm1, dense.N = dense.N)
      lrm2.95f <- auc(xrange = xrange, mod = lrm2, dense.N = dense.N)
      lrm3.95f <- auc(xrange = xrange, mod = lrm3, dense.N = dense.N) 
      #        print(lrm3.95f)
      ###### Save results 
      
      optsave[i,] <- c(samplen, round(c(limits, WA, tol, df1[ic1,xvar],
                                        df1[ic2,xvar], eout2, lrm1.95f,lrm2.95f, lrm2.opt, lrm2.tol, lrm3.95f),rounder), roc)
      
      #### The following code calcualte probabilities of occurrence
      binf <- cut(df2[,xvar], cutp, include.lowest = T)
      bvals <- tapply(df2[, isel] > 0, binf, mean, na.rm =T)
      
      if(plot.pdf) {
        ## Special Plot for log scale bc I can't get the original log scale to work properly 05212018
        if(emmaLOG==TRUE){
          #file_name <- paste('FinalDataAndPlots/Fish/',lim,'/',xvar,'/',tnames[i],'.jpg',sep='')
          #jpeg(file_name)
          par(mar =mar, oma=c(1.5, 0.5, 2,0.5) )
          typ <- ifelse(samplen >= cutoff, "l", "n")
          if(xvar== "wshdImpPCT"){xrange[1] <- c(0.5)} # can't have 0 for log x scale ########################################
          plot(xnew, mean.resp, axes = TRUE, type = typ, xlab = "", ylab = "", xlim= xrange,
               ylim = range(low.bound, up.bound, bvals, na.rm =T), col=1, log='x')
          points(xnew, low.bound, type="l", col=1, lty="dashed")
          points(xnew, up.bound, type="l", col=1, lty="dashed")
          abline(v= lrm3.95f, lty= "dashed", col="red")
          y1range <- range(low.bound, up.bound, bvals, na.rm =T)
          y2range <- range(df1[, isel])
          if(add.abund) {
            y2new <- (df1[,isel] - min(y2range))*diff(y1range)/diff(y2range)+ min(y1range)    # convert y2 to y1 scale
            y1new.at <- (axTicks(2) - min(y1range))*diff(y2range)/diff(y1range)+ min(df1[, isel])    # convert to y2 scale
            y2.lab <- round(y1new.at, 3)    # add labels at original y2 scale
            axis(4, at = axTicks(2), labels = y2.lab, tcl =-0.4, lwd.ticks = 1)    # major ticks with labels
            points(dose, y2new, pch = 21, col = 1, cex = 0.3, bg = "lightgray")
            mtext(ylab2, side = 4, line = 2.5, col=1 , cex = 0.9)
          }    else {
            points(cutm, bvals, pch=21, col= 1, cex = 0.7, bg ="gray")                      # probability
          }
          mtext(xlabs, side = 1, line = 2.3, cex = 0.9)
          mtext(ylab1, side = 2, line = 2.3, cex = 0.9, col=1)
          mtext(bquote(italic(.(tnames[i]))), side = 3, line = 0.5)
          dev.off()
        }else{
          file_name <- paste('FinalDataAndPlots/Fish/',lim,'/',xvar,'/',tnames[i],'.jpg',sep='')
          jpeg(file_name)
          par(mar =mar, oma=c(1.5, 0.5, 2,0.5) )
          typ <- ifelse(samplen >= cutoff, "l", "n")
          plot(xnew, mean.resp, axes = FALSE, type = typ, xlab = "", ylab = "", xlim= xrange,
               ylim = range(low.bound, up.bound, bvals, na.rm =T), col=1)
          
          if(samplen >= cutoff) {
            points(xnew, low.bound, type="l", col=1, lty="dashed")
            points(xnew, up.bound, type="l", col=1, lty="dashed")
            if(!is.na(lim)) {  #  plot limits
              if(lim =="GAM") { 
                abline(v= lrm3.95f, lty= "dashed", col="red")
              } else if(lim =="CDF"){
                abline(v= eout2, lty= "dashed", col="blue")
              }
            }
          }   
          max.pow <- ceiling(max(xrange)); min.pow <- floor(min(xrange))  ## add x range for log formation    
          
          if(log.x) {
            at0 <- min.pow:max.pow              # add ticks at log10 = even     
            lab1 <- base^at0          # could be base = exp(1)
            axis(1, at = log(lab1 + plus, base), labels = lab1, lwd.ticks = 1)    # major ticks with labels
            xtick <- at0 <= max(df1[,xvar]) & at0 >= min(df1[,xvar])          # major labels
            
            if(sum(xtick) <5) {
              axis(1, at = log((1:10 * rep(lab1[-1]/10, each = 10)) + plus, base ), tcl = -0.4, 
                   labels = FALSE, lwd.ticks = 0.8)  # minor ticks            xtick <- at0 <= max(x) & axis.at >= xmin          # major labels
            }
            ### if only two or fewer major lables, then add tick labels at 2 and 5 log
            if(sum(xtick)==2) {
              at1 <- log(3 * rep(lab1[-1]/10, each = 1) + plus, base)
              axis(1, at = at1, tcl = -0.3, lwd.ticks= 1, labels = (3 * rep(lab1[-1]/10, each = 1)),
                   tcl = - 0.4)
            }   else if (sum(xtick)<2) {
              at2 <- log(c(2, 5) * rep(lab1[-1]/10, each = 2) + plus, base)
              axis(1, at = at2, tcl = -0.3, lwd.ticks= 1, labels = (c(2, 5) * 
                                                                      rep(lab1[-1]/10, each = 2)))
            }            
          } else  {
            axis(1, at = pretty(xrange), labels = pretty(xrange), lwd.ticks = 1, las = 1)    # major ticks with labels
          }
          axis(2)
          y1range <- range(low.bound, up.bound, bvals, na.rm =T)
          y2range <- range(df1[, isel])
          if(add.abund) {
            y2new <- (df1[,isel] - min(y2range))*diff(y1range)/diff(y2range)+ min(y1range)    # convert y2 to y1 scale
            y1new.at <- (axTicks(2) - min(y1range))*diff(y2range)/diff(y1range)+ min(df1[, isel])    # convert to y2 scale
            y2.lab <- round(y1new.at, 3)    # add labels at original y2 scale
            axis(4, at = axTicks(2), labels = y2.lab, tcl =-0.4, lwd.ticks = 1)    # major ticks with labels
            points(dose, y2new, pch = 21, col = 1, cex = 0.3, bg = "lightgray")
            mtext(ylab2, side = 4, line = 2.5, col=1 , cex = 0.9)
          }    else {
            points(cutm, bvals, pch=21, col= 1, cex = 0.7, bg ="gray")                      # probability
          }
          
          mtext(xlabs, side = 1, line = 2.3, cex = 0.9)
          mtext(ylab1, side = 2, line = 2.3, cex = 0.9, col=1)
          mtext(bquote(italic(.(tnames[i]))), side = 3, line = 0.5)
          box() 
          
          #if((i+5)/6-round((i+5)/6)==0) {
          #  title(main= main, outer=TRUE, cex.main= 1.8) 
          #  mtext(paste("Page", (i+5)/6, "of", totpage), outer = TRUE, side =1)
          #}
          
          dev.off()
        }
        
        
      }
      
    }
  }
  if(plot.pdf) graphics.off()
  return(optout <- data.frame(tnames, optsave))
}



parameters <- c("DO","pH","SpCond","LRBS","TP","TN","wshdImpPCT","TotHab",'TotHab_wMD','Sf','Cl')#,"MetalCCU")
parametersToAnalyze <- which( colnames(envData) %in% parameters)


z <- tolerance(spdata=gather(fishTaxa,GENUS,RA,-UID),
               envdata = select(envData,UID,names(envData)[i]),
               sp.siteid="UID" ,species="GENUS",sp.abndid="RA",env.siteid="UID",
               xvar=as.character(names(envData)[i]),
               cutoff = 10,
               cutoff2 = 10,
               region = "all",lim ="GAM",coord = NULL,mtype = 3,dense.N = 201,
               cast = TRUE  ,plot.pdf = T,add.map=F,statename = NULL,
               add.lab = F,add.abund = T,
               ylab1 = "Capture Probability",ylab2 = "Relative Abundance",
               main = "Capture Probability of Fish Taxon Along Conductivity Gradient",
               mar = c(5,4,3, 4),
               xlabs=prettyXlabel(names(envData)[i]),
               log.x = FALSE,#ifelse(i %in% c(16,19),TRUE,FALSE),
               base = 10,plus = F,rounder = 2,taus = c(50,95),nbin = 60,
               emmaLOG = FALSE)
#saveRDS(z,paste('FinalDataAndPlots/Fish/GAM/',as.character(names(envData)[i]),'/',
#                as.character(names(envData)[i]),'.RDS',sep=''))

