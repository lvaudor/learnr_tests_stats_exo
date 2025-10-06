to_bins=function(mydf,var,qmin,qmax,Nbins){
  mydf=ungroup(mydf)
  df_basis=bind_cols(id=1:(Nbins-1),
                     Xmin=seq(qmin,qmax,length.out=Nbins)[1:(Nbins-1)],
                     Xmax=seq(qmin,qmax,length.out=Nbins)[2:Nbins]) 
  df_result=df_basis %>% 
    mutate(Y=map2_int(.x=Xmin,.y=Xmax,~length(which(mydf[[var]]>.x & mydf[[var]]<=.y)))) %>% 
    mutate(Y=Y/max(Y))
  result=bind_rows(df_result %>% select(id,Xmin, Y) %>% mutate(or=1, X=Xmin),
                   df_result %>% select(id, Xmax, Y) %>% mutate(or=2, X=Xmax)) %>% 
   arrange(id,X,or) %>% 
   select(id, X,Y)
  
  return(result)
}

repeat_identical=function(mydf,frame){
  mydf=map(as.list(frame),
           ~bind_cols(frame=rep(.,nrow(mydf)),
                      mydf)) %>% 
    bind_rows()
  return(mydf)
}

N=10; mu=2; sigma=1;
nIter=10000;nImages=20;
show=c(TRUE,TRUE,FALSE,FALSE);
model="lnorm";
animate=FALSE

distribution_of_mean=function(N,
                              mu,
                              sigma,
                              nIter=300,
                              nImages=30,
                              model="norm",
                              show=c(TRUE,TRUE,TRUE,TRUE),
                              animate=TRUE){
  dfun=get(paste0("d",model))
  qfun=get(paste0("q",model))
  rfun=get(paste0("r",model))
  qmin=qfun(0.01,mu,sigma)
  qmax=qfun(0.99,mu,sigma)
  xd=seq(qmin,qmax,length.out=300)
  #####
  dfX=tibble(frame=1:nIter) %>%
    group_by(frame) %>% 
    nest() %>% 
    mutate(data=map(frame,~tibble(X=rfun(N,mu,sigma)))) %>% 
    tidyr::unnest(cols=c("data")) %>% 
    group_by(frame) %>% 
    mutate(Xbar=mean(X)) %>% 
    ungroup()
  #####
  dfXplot=dfX %>% 
    filter(frame<=nImages) %>% 
    to_bins(var="X",qmin=qmin,qmax=qmax,Nbins=20) %>% 
    repeat_identical(fram=1:nImages)
  #####  
  dfXbar=dfX %>% 
    select(frame,Xbar) %>% 
    unique()  
    
  dfXTheo=tibble(xd=xd,
                 yd=dfun(xd,mu,sigma)) %>% 
    repeat_identical(frame=1:nImages) %>% 
    mutate(yd=yd/max(yd))
  #######
  dfX=filter(dfX,frame<=nImages)
  xlarge=rfun(100000,mu,sigma)
  meantheo=mean(xlarge)
  sdtheo=sd(xlarge)
  dfXbarTheo=tibble(xd=xd,
                    yd=dnorm(xd,meantheo,sdtheo/sqrt(N))) %>% 
   # repeat_identical(frame=1:nImages) %>% 
    mutate(yd=yd/max(yd))
  dfXbar=to_bins(dfXbar,var="Xbar", qmin=qmin, qmax=qmax,Nbins=20)
  #########
  panim=ggplot(dfX,aes(x=X))+
    scale_x_continuous(limits=c(qmin,qmax))+
    scale_y_continuous(labels=NULL)+
    ggtitle(bquote(paste(X," de loi ",.(model)(mu==.(mu),sigma==.(sigma))," avec ",N==.(N))))
  
  if(show[1]){
    panim=panim+
      geom_ribbon(data=dfXTheo,
                  aes(x=xd,ymin=0,ymax=yd),
                  fill="forestgreen", alpha=0.25)
  }
  
  if(show[2]){
    if(animate){
      panim=panim+
        geom_ribbon(data=dfXplot,
                    aes(x=X,ymax=Y,ymin=0),
                    col="forestgreen", alpha=0.25)+
        geom_vline(aes(xintercept=Xbar), col="#A03090")
    }else{
      panim=panim+
        geom_ribbon(data=dfXplot %>% filter(frame==1),
                    aes(x=X,ymax=Y,ymin=0),
                    col="forestgreen", alpha=0.25)+
        geom_vline(data=dfXplot %>% filter(frame==1),
                   aes(xintercept=Xbar), col="#A03090")
    }
  }
  
  if(show[3]){
    panim=panim+
      geom_ribbon(data=dfXbarTheo,
                  aes(x=xd,ymin=0,ymax=yd),
                  fill="#A03090", alpha=0.25)
  }
  if(show[4]){
    panim=panim+
      geom_ribbon(data=dfXbar,
                  aes(x=X,ymax=Y, ymin=0),
                  col="#A03090", alpha=0.25
      )
  }
  result=panim
  return(result)
}
