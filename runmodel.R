runmodel = function(data,pop,si,prior_r0,prior_rho,nchains,nit,nwarmup,nthin,n.eoo,w.eoo,...) {
  # initialize
  require(rstan);require(dplyr);require(rmarkdown)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  load("compiled_tsir.Rdata")
  ResampleThreshold = 1.05
  
  # manage data
  data2 = mutate(data,
                 POP=pop,
                 NCASES_M1=ifelse(!is.na(lag(NCASES,1)),lag(NCASES,1),0),
                 CUM_NCASES=cumsum(NCASES_M1),
                 INC=NCASES/POP,
                 CUMINC=cumsum(NCASES)/POP,
                 TOT_NCASES=sum(NCASES),
                 AR=TOT_NCASES/POP)
  
  # discretize serial interval
  if(si$type=="Gamma") {
    wmax = floor(si$mean+si$sd*4)
    w = pgamma(1:wmax+.5,shape=si$shape,rate=si$rate)-pgamma(1:wmax-.5,shape=si$shape,rate=si$rate)
    w = w/sum(w)
  }
  if(si$type=="Normal") {
    wmax = floor(si$mean+si$sd*4)
    w = pnorm(1:wmax+.5,mean=si$mean,sd=si$sd)-pnorm(1:wmax-.5,mean=si$mean,sd=si$sd)
    w = w/sum(w)
  }
  if(si$type=="Uniform") {
    wmax = ceiling(si$max)
    w = punif(1:wmax+.5,min=si$min,max=si$max)-punif(1:wmax-.5,min=si$min,max=si$max)
    w = w/sum(w)
  }
  
  # prepare data for stan
  dl = list(W=dim(data2)[1] ,
            O_t=data2$NCASES,
            sumO_t=data2$CUM_NCASES,
            pop=unique(data2$POP),
            siL=length(w),
            siW=w,
            P_R0_type=NA,
            P_rho_type=NA,
            P_R0=c(NA,NA),
            P_rho=c(NA,NA),
            pW=104
  )
  
  # select model and priors
  if(prior_r0$type=="Exponential") {
    dl$P_R0 = c(prior_r0$rate,0)
    dl$P_R0_type = 1
  }
  if(prior_r0$type=="Uniform") {
    dl$P_R0_type = 2
    dl$P_R0 = c(prior_r0$min,prior_r0$max)
  }
  if(prior_r0$type=="Normal") {
    dl$P_R0_type = 3
    dl$P_R0 = c(prior_r0$mean,prior_r0$sd)
  }
  if(prior_r0$type=="Gamma") {
    dl$P_R0_type = 4
    dl$P_R0 = c(prior_r0$mean,prior_r0$sd)
  }
  if(prior_rho$type=="Beta") {
    dl$P_rho_type = 1
    dl$P_rho = c(prior_rho$shape1,prior_rho$shape2)
  }
  if(prior_rho$type=="Uniform") {
    dl$P_rho_type = 2
    dl$P_rho = c(prior_rho$min,prior_rho$max)
  }
  if(prior_rho$type=="Gamma") {
    dl$P_rho_type = 3
    dl$P_rho = c(prior_rho$mean,prior_rho$sd)
  }

  # run model
  S_ = sampling(object=M_,
                data=dl,
                chains=nchains,
                iter=nit,
                warmup=nwarmup,
                thin=nthin,
                algorithm="NUTS")
  
  # check convergence (with Rhat<1.05) and resample with more iterations if not converged
  maxrhat <- max(summary(S_,pars=c("R0","rho","phi"))[[1]][,"Rhat"])
  nresample = 0
  while(maxrhat>ResampleThreshold & nresample<6) {
    S_ = sampling(object=M_,
                  data=dl,
                  chains=nchains,
                  iter=nit+(nresample/10)*nit,
                  warmup=nwarmup+(nresample/10)*nwarmup,
                  thin=nthin,
                  sample_file="sampling",
                  diagnostic_file="diagnostic",
                  algorithm="NUTS")
    maxrhat <-  max(summary(S_,pars=c("R0","rho","phi"))[[1]][,"Rhat"])
    nresample = nresample + 1
  }
  
  # prepare output
  res = list()
  res$data = data
  res$dl = dl
  res$pop = pop
  res$si = si
  res$prior_r0 = prior_r0
  res$prior_rho = prior_rho
  res$nchains = nchains
  res$nit = nit
  res$nwarmup = nwarmup
  res$nthin = nthin
  res$n.eoo = n.eoo
  res$w.eoo = w.eoo
  
  # diagnostics
  res$trace1 = stan_trace(S_,pars=c("R0"),inc_warmup=FALSE) + 
    labs(y=expression(R[0]))
  res$trace2 = stan_trace(S_,pars=c("rho"),inc_warmup=FALSE) + 
    labs(y=expression(rho))
  res$trace3 = stan_trace(S_,pars=c("phi"),inc_warmup=FALSE) + 
    labs(y=expression(phi))
  res$acf1 = stan_ac(S_,pars=c("R0")) 
  res$acf2 = stan_ac(S_,pars=c("rho"))
  res$acf3 = stan_ac(S_,pars=c("phi"))
  
  # extract parameters of interest
  res$R_summarypars = summary(S_,pars=c("R0","rho","phi"))[[1]]
  res$R_summarypred = summary(S_,pars=c("ptotlp","ptotoverall","pattackrate"))[[1]]
  res$R_dens_r0 = density(rstan::extract(S_,pars="R0")[[1]])
  res$R_dens_rho = density(rstan::extract(S_,pars="rho")[[1]],from=0,to=1)
  res$R_dens_phi = density(rstan::extract(S_,pars="phi")[[1]])
  res$R_fit = cbind(as.data.frame(summary(S_,pars="pred_lp")[[1]]),NWEEK=1:dl$W)
  res$R_resid = cbind(as.data.frame(summary(S_,pars="resid_lp")[[1]]),NWEEK=1:dl$W)
  
  # compute predicted epidemic curve
  tmppred = rstan::extract(S_,pars="pO")[[1]]
  tmpdate = 1:ncol(tmppred) + dim(res$R_fit)[1]
  tmpmean = apply(tmppred,2,mean)
  tmpdelta = abs(sweep(tmppred,2,tmpmean))
  tmprank = apply(tmpdelta,2,rank,ties.method="random")
  tmprankmax = rank(apply(tmprank,1,max),ties.method="random")
  tmpkeep95 = tmppred[which(tmprankmax<=length(tmprankmax)*0.95),]
  tmpkeep50 = tmppred[which(tmprankmax<=length(tmprankmax)*0.5),]
  tmpint95 = t(apply(tmpkeep95,2,function(x) c(min(x),max(x))))
  tmpint50 = t(apply(tmpkeep50,2,function(x) c(min(x),max(x))))
  res$R_pred = data.frame(mean=tmpmean,se_mean=NA,
                          sd=NA,`2.5%`=tmpint95[,1],
                          `25%`=tmpint50[,1],`50%`=apply(tmppred,2,median),`75%`=tmpint50[,2],`97.5%`=tmpint95[,2],
                          n_eff=NA,Rhat=NA,NWEEK=tmpdate)
  res$R_pred = filter(res$R_pred,X97.5.>0)
  names(res$R_pred) = c("mean","se_mean","sd","2.5%","25%","50%","75%","97.5%","n_eff","Rhat","NWEEK")

  # compute time predictions
  tmpmax = do.call("cbind",rstan::extract(S_,pars=c("lp","pO"))) %>%
    apply(.,1,max)
  res$R_max = round(c(mean=mean(tmpmax),se_mean=sd(tmpmax)/length(tmpmax),sd=sd(tmpmax),quantile(tmpmax,probs=c(0.025,0.25,0.5,0.75,0.975)),n_eff=NA,Rhat=NA),2)
  
  tmpwhichmax = do.call("cbind",rstan::extract(S_,pars=c("lp","pO"))) %>%
    apply(.,1,which.max) 
  res$R_peak = round(c(mean=mean(tmpwhichmax),se_mean=sd(tmpwhichmax)/length(tmpwhichmax),sd=sd(tmpwhichmax),quantile(tmpwhichmax,probs=c(0.025,0.25,0.5,0.75,0.975)),n_eff=NA,Rhat=NA),2)
  
  tmpstart = do.call("cbind",rstan::extract(S_,pars=c("lp","pO"))) %>%
    apply(.,1,function(x) which(cumsum(x>n.eoo)==w.eoo)) %>%
    unlist()
  res$R_start = round(c(mean=mean(tmpstart),se_mean=sd(tmpstart)/length(tmpstart),sd=sd(tmpstart),quantile(tmpstart,probs=c(0.025,0.25,0.5,0.75,0.975)),n_eff=NA,Rhat=NA),2)
  
  tmpend = do.call("cbind",rstan::extract(S_,pars=c("lp","pO")))
  tmpend = unlist(lapply(1:dim(tmpend)[[1]], function(i) {
    y = tmpend[i,]
    y[1:which.max(y)] <- max(y)
    return(which(cumsum(y<n.eoo)==w.eoo))
  }))
  res$R_end = round(c(mean=mean(tmpend),se_mean=sd(tmpend)/length(tmpend),sd=sd(tmpend),quantile(tmpend,probs=c(0.025,0.25,0.5,0.75,0.975)),n_eff=NA,Rhat=NA),2)
  fit = res
  save(fit,file="fit.Rdata")
  return(res)
}