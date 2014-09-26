#misc R functions


library(gdata)

tabpct=function(row, col, rowlab="N", tex="False"){
	t1=table(row, col)
	ptot1=round((apply(t1, 2, sum))/sum(apply(t1, 2, sum)), 4)*100
	pt1=round(prop.table(t1, 1), 4)*100
	count1=apply(t1, 2, sum)
	#Total=c(count1[1], ptot1[1], count1[2], ptot1[2])
	Total=c((interleave(t(count1), t(ptot1))))
 #tab1=cbind(t1[,1], pt1[,1], t1[,2], pt1[, 2])
  tab1=t(interleave(t(t1), t(pt1)))
	tab1=rbind(tab1, Total)
  xcols=dim(tab1)[2]/2
  ns=c(rep(rowlab, xcols))
  if(tex=="False"){pcts=c(rep("%", xcols))}
  if(tex=="True") {pcts=c(rep("\\%", xcols))}
  xnames=c(interleave(t(ns), t(pcts)))
  colnames(tab1)=xnames
 	#if(tex=="False"){colnames(tab1)=c(rowlab1, "%", rowlab2, "%")}

  #if(tex=="True") {colnames(tab1)=c(rowlab1, "\\%", rowlab2, "\\%")}

 	tab1
	return(tab1)
	}



 tabpct.oneway=function(col, rowlab="N", tex="False"){
	t1=table(col)
	count1=sum(t1)
	pt1=round(prop.table(t1), 4)*100
	Total=c(count1[1], 100) 
	tab1=cbind(t1, pt1) 
	tab1=rbind(tab1, Total)
	if(tex=="False") {colnames(tab1)=c(rowlab, "%")}
	if(tex=="True") {colnames(tab1)=c(rowlab, "\\%")}
	tab1
	return(tab1)
	}



tabpct.par=function(row, col, rownames=NULL, colnames=NULL) {
 	t1=table(row, col)
	rsums=apply(t1,1, sum)
	t1=cbind(t1, rsums)
	csums=apply(t1,2, sum)
	csums.pct=paste(csums, "(100)", sep="")

	pcts=round(prop.table(t1, margin=2), 3)*100
	
	pct.par=paste("(", pcts,  ")", sep="")
	dat.pct=paste(t1, pct.par, sep="")
	dat.pct=matrix(dat.pct, nrow=dim(t1)[1], ncol=dim(t1)[2])
	dat.pct=rbind(dat.pct, csums.pct)
	if (!is.null(rownames)) { 
		dimnames(dat.pct)[[1]]=rownames
		}
		else {
		dimnames(dat.pct)[[1]]=NULL
		}
	if (!is.null(colnames)) {
		 dimnames(dat.pct)[[2]]=colnames
		} 
		else {
		dimnames(dat.pct)[[2]]=NULL
		}
		
	return(dat.pct)
	}
	
describe=function(x) {
	n <- length(na.omit(x))
	xbar <- mean(x, na.rm=T)
	md <- median(x, na.rm=T)
	s2 <- var(x, na.rm=T)
	s <- sqrt(var(x, na.rm=T))
	se <- s/sqrt(n)
	xmin <- min(x, na.rm=T)
	xmax <- max(x, na.rm=T)
	rng <- xmax - xmin
	cbind(n, xbar, md, s2, s, se, xmin, xmax, rng)
}

glmORs=function(x, dps=3){
  ors=exp(x$coefficients)
  ses=summary(x)$coefficients[, 2]
  pvals=summary(x)$coefficients[, 4]
  orslo=exp(x$coefficients-(1.96*ses))
  orsup=exp(x$coefficients+(1.96*ses))
  dat=cbind(round(ors, dps), round(orslo, dps),  round(orsup, dps), round(pvals, dps))
  dat=dat[-1,] 
  colnames(dat)=c("ORs", "95LB", "95UB", "p")
    dat
}
  

dropvars=function(dat, x) {
xtmp <- names(dat) %in% c(x)
dtmp=dat[!xtmp]
}

