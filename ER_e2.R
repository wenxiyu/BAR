

ER = function(t, n0, n1, N, theta1, theta2, delta_l, 
              delta_u, trup, seed = 12,drop = 0){
  
  set.seed(seed)
  narm = length(trup)
  # record the results over t simulations
  p_summary = matrix(nrow = t, ncol = narm) # respnose rate
  n_summary = matrix(nrow = t, ncol = narm) # sample size
  x_summary = matrix(nrow = t, ncol = narm) # cured patinets
  s_summary = matrix(nrow = t, ncol = narm) # declaring success
  d_summary = matrix(nrow = t, ncol = narm) # temporary suspension
  
  for(u in 1:t){
    
    n = c(rep(0, narm)) 
    x = c(rep(0, narm))
    pp = c(rep(1/narm, narm))
    p = matrix(ncol = narm, nrow = N + n0)
    
    # early enrollment
    for(cc in 1:n0){
      n = n + 1  # every group increases one patient
      tempt = rbinom(narm, 1, trup)
      x = x + tempt
      p[cc, ] = x/n
    }
    # AR: every round enrolls n1 new patients, total N rounds
    for(aa in 1:N){ 
      for(bb in 1:n1){
        tempt = sample(1:narm, 1, replace = T, prob = pp)
        n[tempt] = n[tempt] + 1
        temp1 = rbinom(1, 1, trup[tempt])
        x[tempt] = temp1 + x[tempt]
      }
      p[cc+aa, ] = x/n
    }
    
    n_summary[u, ] = n
    x_summary[u, ] = x
    p_summary[u, ] = x / n
    
    # declaring success  
    for (i in 1:narm){
      tempt = length(which(p[, i] > theta1)) / nrow(p)
      if (tempt > delta_u) 
        s_summary[u, i] = 1
    }
  }
  
  
  for (i in 1:narm){
    print(paste("response rate",i))
    print(colMeans(p_summary)[i],digits = 2)
    print(quantile(p_summary[ , i],c(0.50, 0.025, 0.975)),digits = 2)
    
    print("sample size")
    print(colMeans(n_summary)[i],digits = 2)
    print(quantile(n_summary[ , i],c(0.50, 0.025, 0.975)),digits = 2)
    
    print("declaring success")
    print(colSums(s_summary/100,na.rm = TRUE))
    
    if(drop){
      print("dropping arm")
      print(colMeans(d_summary, na.rm = TRUE)[i], digits = 2)
      print(quantile(d_summary[ , i], c(0.50,0.025,0.975),
                     na.rm = TRUE),digits = 2)
    }
    print("===================================")
  }
  
  results = list()
  results[[1]] = p_summary
  results[[2]] = n_summary
  results[[3]] = x_summary
  results[[4]] = s_summary
  results[[5]] = d_summary
  return(results)
}





