ivgls <-
function(Depend,Endo,IVs,Exo,lags,iter) {
    
    ## Set number of iterations
    if(missing(iter)) {
        iter=100
    } else {
        iter=iter
    }
    
    ## Determine whether there are sufficient instrumental variables
    if(!is.null(ncol(Endo)) && !is.null(ncol(IVs))) {
        try(if(ncol(Endo)>ncol(IVs)) stop("Error: insufficient instrumental variables"))
    }
    if(!is.null(ncol(Endo)) && is.null(ncol(IVs))) {
        try(if(ncol(Endo)>1) stop("Error: insufficient instrumental variables"))
    }
    
    ## Set names exogenous variable(s)
    if(!missing(Exo)) {
        if(!is.null(colnames(Exo))) {
            names_Exo=colnames(Exo) 
        } else {
            if(!is.null(ncol(Exo))) {
                names_Exo=rep("a",ncol(Exo))
                for(i in 1:ncol(Exo)) {names_Exo[i]=paste("Exo",i)}  
            } else {
                names_Exo="Exo" 
            }
        }
        if(!is.null(ncol(Exo)) && ncol(Exo)!=0) {
            Exo=matrix(Exo,ncol=ncol(Exo))
        } else {
            Exo=matrix(Exo)
        }
    }
    
    ## Set names endogenous variable(s)
    if(!is.null(colnames(Endo))) {
        names_Endo=colnames(Endo) 
    } else {
        if(is.null(ncol(Endo))) {
            names_Endo=deparse(substitute(Endo))
        } else{
            names_Endo=rep("a",ncol(Endo))
            for(i in 1:ncol(Exo)) {names_Endo[i]=paste("Endo",i)}  
        }
    }
    
    ## Set name dependent variable
    if(!is.null(colnames(Depend))) {
        name_Depend=colnames(Depend) 
    } else {
        name_Depend=deparse(substitute(Depend))
    }
    
    ## Define dependent variable ; all observations
    Depend=matrix(Depend)
    
    ## Define endogenous variable(s)
    if(!is.null(ncol(Endo))) {
        Endo=matrix(Endo,ncol=ncol(Endo))
    } else {
        Endo=matrix(Endo)
    }
    
    ## Define instrumental variable(s)
    if(!is.null(ncol(IVs))) {
        IVs=matrix(IVs,ncol=ncol(IVs))
    } else {
        IVs=matrix(IVs)
    }
    
    ## Number of observations
    n=nrow(Depend)
    
    ## End, lags taken into account
    end=n-lags
    
    ## Begin, lags taken into account
    begin=lags+1
    
    ## Check whether lags>observations (including dummies)
    for(i in 1:ncol(Endo)) {
        for(j in 1:nrow(Endo)) {
            if(nrow(Endo)<=lags || abs(min(ifelse(length(rle(c(Endo[,i]))$lengths[rle(c(Endo[,i]))$values==1])>=1
                                                  ,rle(c(Endo[,i]))$lengths[rle(c(Endo[,i]))$values==1],1000)))<=lags) {
                stop("Error: insufficient observations")
            }
        }
    }
    
    ## Check whether lags>observations (including dummies)
    if(!missing(Exo)) {
        for(i in 1:ncol(Exo)) {
            if(abs(min(ifelse(length(rle(c(Exo[,i]))$lengths[rle(c(Exo[,i]))$values==1])>=1
                              ,rle(c(Exo[,i]))$lengths[rle(c(Exo[,i]))$values==1],1000)))<=lags) {
                stop("Error: insufficient observations")
            }
        }
    }
    
    ## Independent variables: constant, endogenous independent variables and
    ## exogenous independent variables (optional) ; all observations
    if(missing(Exo)) {
        Indep=cbind(c(rep(1,n)),Endo)
    } else {
        Indep=cbind(c(rep(1,n)),Endo,Exo)
    }
    
    ## Independent variables ; observations without lags
    X=Indep[begin:n,]
    
    ## Number of variables
    m=ncol(X)
    
    ## Dependent variable ; observations without lags
    Y=matrix(Depend[begin:n,])
    
    ## Instrumental variables: constant, exogenous variables,
    ## instruments, lagged dependent variables Y
    if(missing(Exo)) {
        IV=cbind(c(rep(1,end)),IVs[begin:n,])
    } else {
        IV=cbind(c(rep(1,end)),Exo[begin:n,],IVs[begin:n,])
    }
    
    for(i in lags:1) {
        IV=cbind(IV,Depend[i:(n-(begin-i)),])
    }
    
    ## Weighting matrix
    W=IV%*%solve(t(IV)%*%IV)%*%t(IV)
    
    ## Consistent estimator for beta
    beta=solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%Y
    
    ## Residuals
    eps=Y-X%*%beta
    
    ## Matrix with (lagged) residuals
    epsmat=matrix(0,nrow=end-lags,ncol=begin)
    
    ## Today's residuals
    epsmat[,1]=eps[begin:end]
    
    ## Add lagged residuals to the matrix
    for(i in 2:begin) {
        epsmat[,i]=eps[(begin-i+1):(end-i+1)]
    }
    
    ## Correlation coefficients
    rho=solve(t(epsmat[,2:begin])%*%epsmat[,2:begin])%*%t(epsmat[,2:begin])%*%epsmat[,1]
    
    ## Instrument matrix: original instruments, lags of the exogenous variables,
    ## lags of the endogenous variables
    Instr=cbind(IV,Indep[1:(n-(begin-1)),-1])
    if(lags>1) {
        for(i in lags:2) {
            Instr=cbind(Instr,Indep[i:(n-(begin-i)),-1])
        }
    }
    
    ## New weighting matrix
    Wstar=Instr%*%solve(t(Instr)%*%Instr)%*%t(Instr)
    
    ## EGLS correction
    X_=X-as.numeric(rho[1])*Indep[lags:(n-1),]
    Y_=Y-as.numeric(rho[1])*Depend[lags:(n-1),]
    
    ## EGLS correction if lags>1
    if(lags>1) {
        for(i in 2:lags) {
            X_=X_-as.numeric(rho[i])*Indep[(lags-i+1):(n-i),]
            Y_=Y_-as.numeric(rho[i])*Depend[(lags-i+1):(n-i),]
        }
    }
    
    ## Restore intercept after EGLS correction
    X_[,1]=c(rep(1,end))
    
    ## Create matrix to store autocorrelation coefficients
    rho_=matrix(0,nrow=iter,ncol=lags)
    rho_[1,]=rho
    
    ## 2SLS on EGLS corrected data
    beta_=solve(t(X_)%*%Wstar%*%X_)%*%t(Wstar%*%X_)%*%Y_
    
    ## Correct intercept
    beta_[1]=beta_[1]/(1-sum(rho))
    
    ## Create matrix to store 2SLS coefficients
    beta_it=matrix(0,nrow = iter,ncol=dim(X_)[2])
    beta_it[1,]=t(beta_)
    
    ## Iterative process to compute the 2SLS coefficients
    for(t in 2:iter) {
        eps=Y-X%*%beta_it[(t-1),]
        epsmat=matrix(0,nrow=(n-lags),ncol=begin)
        epsmat[,1]=eps
        
        for(i in 2:begin) {
            epsmat[,i]=Depend[(begin-i+1):(n-i+1),]-Indep[(begin-i+1):(n-i+1),]%*%beta_it[(t-1),]
        }
        
        rho_[t,]=solve(t(epsmat[,2:begin])%*%epsmat[,2:begin])%*%t(epsmat[,2:begin])%*%epsmat[,1]
        
        X_=X-as.numeric(rho_[t,1])*Indep[lags:(n-1),]
        Y_=Y-as.numeric(rho_[t,1])*Depend[lags:(n-1),]
        
        if(lags>1) {
            for(i in 2:lags) {
                X_=X_-as.numeric(rho_[t,i])*Indep[(lags-i+1):(n-i),]
                Y_=Y_-as.numeric(rho_[t,i])*Depend[(lags-i+1):(n-i),]
            }
        }
        
        ## Store to calculate the variance
        Xvar=X_
        
        X_[,1]=c(rep(1,end))
        
        beta_it[t,]=solve(t(X_)%*%Wstar%*%X_)%*%t(Wstar%*%X_)%*%Y_
        beta_it[t,1]=beta_it[t,1]/(1-sum(rho_[t,]))
    }
    betaFinal=beta_it[iter,]
    
    ## Residuals to be returned, after normalization
    res=Y-X%*%betaFinal
    resmat=matrix(0,nrow=(n-lags),ncol=begin)
    resmat[,1]=res
    
    for(i in 2:begin) {
        resmat[,i]=Depend[(begin-i+1):(n-i+1),]-Indep[(begin-i+1):(n-i+1),]%*%betaFinal
    }
    
    ## 'Normalized' residuals (taking into accout the autocorrelation structure)
    eps=res-matrix(resmat[,2:begin],ncol=lags)%*%rho_[iter,]
    
    ## Residual variance
    sigma2=as.numeric(1/(n-m-lags)*t(eps)%*%eps)
    
    ## Sum of squared residuals
    RSSur=t(eps)%*%eps
    
    ## Fitted values
    fitted_Y=X%*%betaFinal+matrix(resmat[,2:begin],ncol=lags)%*%rho_[iter,]
    
    ## Calculating the standard error of the coefficients 
    ## (including correlation coefficients)
    G=cbind(Xvar,matrix(resmat[,2:begin],ncol=lags))
    secoef=sqrt(diag(sigma2*solve(t(G)%*%Wstar%*%G)))
    
    ## Coefficients, including the correlation coeffs
    coef=c(betaFinal,rho_[iter,])
    
    ## T-statistics of the coefficients
    t_stat_beta=coef/secoef
    
    ## P-values
    p_value=rep(NULL,length(t_stat_beta))
    for(i in 1:length(t_stat_beta)) {
        p_value[i]=ifelse(t_stat_beta[i]>0,2*(1-pt(t_stat_beta[i],(n-m))),2*pt(t_stat_beta[i],(n-m)))
    }
    
    ## Significance sign for p-values
    r=rep("",length(coef))
    for(i in 1:length(p_value)) {
        if(p_value[i]<=0.01) {
            r[i]="***"
        }
        if(p_value[i]>0.01 && p_value[i]<=0.05) {
            r[i]="**"
        }
        if(p_value[i]>0.05 && p_value[i]<=0.10) {
            r[i]="*"
        }
    }
    
    ## R-squared and adjusted R-squared
    N=diag(end)-(1/end)*rep(1,end)%*%t(rep(1,end))
    R2=1-sigma2/(t(Y)%*%N%*%Y/(n-length(coef)))
    adjR2=1-(1-R2)*(end-1)/(end-m-lags)
    
    ## Names of correlation coefficients
    rho_names=rep("a",lags)
    rho_names[1]=paste("rho","1")
    if(lags>1) {
        for(i in 2:lags) {
            rho_names[i]=paste("rho",i)
        }
    }
    
    ## Names of independent variable(s)
    if(missing(Exo)) {
        name=c("(Intercept)",names_Endo,rho_names)
        #names(coef)=c("intercept",names_Endo,rho_names)
    } else {
        name=c("(Intercept)",names_Endo,names_Exo,rho_names)
        #names(coef)=c("intercept",names_Endo,names_Exo,rho_names)
    }
    
    obs=n
    
    ## Output list
    outputFile=list(coef,t_stat_beta,secoef,p_value,R2,adjR2,sqrt(sigma2),eps,fitted_Y,name,obs)
    
    ## Preparing output and printing
    coef=sprintf("%.4f",round(coef,4))
    t_stat_beta=sprintf("%.4f",round(t_stat_beta,4))
    p_value=sprintf("%.4f",round(p_value,4))
    R2=sprintf("%.4f",round(R2,4))
    adjR2=sprintf("%.4f",round(adjR2,4))
    secoef=sprintf("%.4f",round(secoef,4))
    sigma=sprintf("%.4f",round(sqrt(sigma2),4))
    sigmahat=sprintf("%.4f",round(sigma2,4))
    tabel=matrix("",nrow=length(coef),ncol=5)
    tabel[,2:5]=cbind(coef,secoef,t_stat_beta,p_value)
    tabel[,1]=name
    tabel=cbind(tabel,r)
    colnames(tabel)=c("","Estimate","Std. error","t value","Pr(>|t|)","")
    
    ## Function to print the results
    printing=function() {
        cat(paste("Dependent variable:",name_Depend),sep = "\n")
        cat(sep="\n")
        cat("Coefficients:",sep = "\n")
        prmatrix(tabel,quote = FALSE,right = TRUE,
                 rowlab=rep("",nrow(tabel)))
        cat("----",sep = "\n")
        cat("Sign. codes: 0.01 '***' 0.05 '**' 0.10 '*' ",sep = "\n")
        cat("\n")
        cat(paste("Residual standard error:",sigma,"on",(n-m-lags)
                  ,"degrees of freedom"),sep = "\n")
        cat(paste("Multiple R-squared:"),R2,", Adjusted R-squared:",
            adjR2)
    }
    
    ## Output to be returned
    outputFile=c(printing,outputFile)
    names(outputFile)=c("summary","coefficients","t.stat","se.coef","p.value","R.squared","adj.R.squared","sigma","residuals","fitted.values","name","observations")
    
    return(outputFile)
}
