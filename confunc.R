#linear contrasts for multiply imputed predictive margins

confunc<-function(contrasts,stat){
  
  exprlist <- list(contrast = contrasts)
  datalist<-as.list(stat) 
  varmat<-as.matrix(attr(stat,"var"))
  
  
  dexprlist <- lapply(exprlist, function(expr) deriv(expr, 
                                                     
                                                     names(datalist))[[1]])
  
  values <- lapply(dexprlist, function(dexpr) eval(do.call(substitute, 
                                                           
                                                           list(dexpr, datalist))))
  jac <- do.call(rbind, lapply(values, function(value) attr(value, 
                                                            
                                                            "gradient")))
  
  var <- jac %*% varmat %*% t(jac)
  
  values <- do.call(c, values)
  
  dimnames(var) <- list(names(values), names(values))
  
  attr(values, "var") <- var
  
  
  
  values
}
