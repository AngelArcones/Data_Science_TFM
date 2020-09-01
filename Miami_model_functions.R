NPP_temp <- function(temp){
  3000/(1+exp(1.315-0.119*temp))
}

NPP_prec <- function(prec){
  3000*(1-exp(-0.000664*prec))
}