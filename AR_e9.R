
AR = function(t, n0, n1, N, a1, a2, theta1, theta2, 
              delta_l, delta_u, drop, trup, seed = 12){

set.seed(seed)
narm = length(trup)
# record the results over t simulations
p_summary = matrix(nrow = t, ncol = narm) # respnose rate
n_summary = matrix(nrow = t, ncol = narm) # sample size
x_summary = matrix(nrow = t, ncol = narm) # cured patinets
s_summary = matrix(nrow = t, ncol = narm) # declaring success
d_summary = matrix(nrow = t, ncol = narm) # temporary suspension
e = matrix(nrow = N, ncol = narm)  # early stopping rule

for(u in 1:t){

  n = c(rep(0, narm)) 
  x = c(rep(0, narm))
  p = c(rep(1/narm, narm))

  # early enrollment
  for(cc in 1:n0){
    n = n + 1  # every group increases one patient
    tempt = rbinom(narm, 1, trup)
    x = x + tempt
  }
 # AR: every round enrolls n1 new patients, total N rounds
  for(aa in 1:N){ 
    a2 = a1 + x
    b2 = a1 + b1 + n
    p = a2 / b2
    
    if (drop){
      for (i in 1:5){
        e[i] = 1 - pbeta(theta2, a2[i], b2[i])
        if (e[i] < delta_l) # suspend this tx
        {
        p[i] = 0
        d_summary[u,i] = n[i]
        }
      }
      if (sum(p) == 0)
        p = rep(1/narm, narm)
    }
    
    for(bb in 1:n1){
      tempt = sample(1:narm, 1, replace = T, prob = p)
      n[tempt] = n[tempt] + 1
      temp1 = rbinom(1, 1, trup[tempt])
      x[tempt] = temp1 + x[tempt]
    }
  }
  
  n_summary[u, ] = n
  x_summary[u, ] = x
  p_summary[u, ] = x / n
 
   # declaring success  
  for (i in 1:narm){
    a2 = a1 + x
    b2 = a1 + b1 + n
    e[i] = 1 - pbeta(theta1, a2[i], b2[i])
    if (e[i] > delta_u) 
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










