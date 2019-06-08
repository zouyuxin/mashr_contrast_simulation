plotMotif = function(bestmotif){
  layout(matrix(1:2,ncol=2))
  R = length(bestmotif$W)
  K = dim(bestmotif$W[[1]])[1]
  q = matrix(0,nrow=K,ncol=R)
  for(r in 1:R){
    q[,r] = 1-bestmotif$W[[r]][,1]
  }
  u = 1:R
  v = 1:K
  image(u,v,t(q),
        col=gray(seq(from=1,to=0,by=-0.1)),xlab="Bacterial infection",yaxt = "n",
        ylab="Expression pattern",main="pattern")
  axis(2,at=1:length(v))
  axis(1,at=1:length(u))
  for(i in 1:(length(u)+1)){
    abline(v=(i-0.5))
  }
  for(i in 1:(length(v)+1)) {
    abline(h=(i-0.5))
  }

  Ng=10000
  if(is.null(bestmotif$clustlike)!=TRUE)
    Ng=nrow(bestmotif$clustlike)
  genecount=floor(bestmotif$pi*Ng)
  NK=K
  plot(0,0.7,pch=".",xlim=c(0,1.2),ylim=c(0.75,NK+0.25),
       frame.plot=FALSE,axes=FALSE,xlab="Number of genes",ylab="", main="frequency")
  segments(0,0.7,bestmotif$pi[1],0.7)
  rect(0,1:NK-0.3,bestmotif$pi,1:NK+0.3,
       col="dark grey")
  mtext(1:NK,at=1:NK,side=2,cex=0.8)
  text(bestmotif$pi+0.15,1:NK,
       labels=floor(bestmotif$pi*Ng))
}

plotMotif.mean = function(bestmotif, mixsd){
  layout(matrix(1:2,ncol=2))
  R = length(bestmotif$W)
  K = dim(bestmotif$W[[1]])[1]
  q = matrix(0,nrow=K,ncol=R)
  for(r in 1:R){
    q[,r] = bestmotif$W[[r]]%*%mixsd
  }
  u = 1:R
  v = 1:K
  image(u,v,t(q),
        col=gray(seq(from=1,to=0,by=-0.1)),xlab="Bacterial infection",yaxt = "n",
        ylab="Expression pattern",main="pattern")
  axis(2,at=1:length(v))
  axis(1,at=1:length(u))
  for(i in 1:(length(u)+1)){
    abline(v=(i-0.5))
  }
  for(i in 1:(length(v)+1)) {
    abline(h=(i-0.5))
  }

  Ng=10000
  if(is.null(bestmotif$clustlike)!=TRUE)
    Ng=nrow(bestmotif$clustlike)
  genecount=floor(bestmotif$pi*Ng)
  NK=K
  plot(0,0.7,pch=".",xlim=c(0,1.2),ylim=c(0.75,NK+0.25),
       frame.plot=FALSE,axes=FALSE,xlab="Number of genes",ylab="", main="frequency")
  segments(0,0.7,bestmotif$pi[1],0.7)
  rect(0,1:NK-0.3,bestmotif$pi,1:NK+0.3,
       col="dark grey")
  mtext(1:NK,at=1:NK,side=2,cex=0.8)
  text(bestmotif$pi+0.15,1:NK,
       labels=floor(bestmotif$pi*Ng))
}
