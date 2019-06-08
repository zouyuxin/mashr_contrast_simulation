#' Create simulation with signal
#' @param nsamp number of samples of each type
#' @param ncond number of conditions
#' @param err_sd the standard deviation of the errors
#' @details The simulation consists of equal numbers of four different types of deviations: null, equal among conditions, present only in first condition, independent across conditions
#' @export
sim.mean.sig = function(nsamp = 100, ncond = 5, err_sd=sqrt(0.5)){
  # generate scalar
  Cs = rnorm(nsamp, 10)
  C = matrix(rep(Cs,10), nrow=nsamp, ncol=10)
  # 90% null
  nsamp.alt = ceiling(0.1*nsamp)
  D.zero = matrix(0, nrow=nsamp-nsamp.alt, ncol=10)
  # 10% alt
  nsamp.all = floor(nsamp.alt)
  # generate delta
  D.all = matrix(0,nrow=nsamp.all, ncol=10)
  d1 = rnorm(nsamp.all,sd=2)
  D.all[,1:ncond] = matrix(rep(d1, ncond), nrow=nsamp.all, ncol=ncond)

  D = rbind(D.zero, D.all)

  C = C + D

  Shat = matrix(err_sd, nrow=nrow(C), ncol=ncol(C))
  E = matrix(rnorm(length(Shat), mean=0, sd=Shat), nrow=nrow(C),ncol=ncol(C))
  Chat = C+E
  row_ids = paste0("sample_", 1:nrow(C))
  col_ids = paste0("condition_", 1:ncol(C))
  rownames(C) = row_ids
  colnames(C) = col_ids
  rownames(Chat) = row_ids
  colnames(Chat) = col_ids
  rownames(Shat) = row_ids
  colnames(Shat) = col_ids
  return(list(C=C,Chat=Chat,Shat=Shat))
}
