sim.flash = function(nsamp=100, err_sd=1){
  ncond=6
  B.0 = matrix(0, nrow = nsamp, ncol = ncond)
  B.1 = B.0; B.2 = B.0; B.3 = B.0; B.4 = B.0; B.5 = B.0
  b = rnorm(nsamp)
  B.1[,c(1,3)] = cbind(b, b)
  B.2[,2] = b
  # b3 = rnorm(nsamp)
  B.3[,c(4,6)] = cbind(b,b)

  B.4[,c(1,2,3)] = cbind(b,b,b)
  B.5[,c(2,4,6)] = cbind(b, b, b)

  B = rbind(B.0, B.1, B.2, B.3, B.4, B.5)

  Shat = matrix(err_sd, nrow = nrow(B), ncol = ncol(B))
  E = matrix(rnorm(length(Shat), mean = 0, sd = Shat), nrow = nrow(B),
             ncol = ncol(B))
  Bhat = B + E

  row_ids = paste0("effect_", 1:nrow(B))
  col_ids = paste0("condition_", 1:ncol(B))
  rownames(B) = row_ids
  colnames(B) = col_ids
  rownames(Bhat) = row_ids
  colnames(Bhat) = col_ids
  rownames(Shat) = row_ids
  colnames(Shat) = col_ids
  return(list(B = B, Bhat = Bhat, Shat = Shat))
}
