
# whether there is temporary suspension
drop = 1  
# true response rate of each arm
trup = c(0.3, 0.05, 0.05, 0.05, 0.05)
# number of simulations
t = 1000   
# early enrollment: make sure each treatment has n0 patients before AR starts
n0 = 2  
# enroll n1 patients at a time (per month)
n1 = 2  
# continously enroll for N times (month)
N = 25
# narm = length (trup)
# thus total # of patients = n0 * narm + n1 * N

# a1,b1 is the hyperparamerts of the prior distribution pj~gamma(a1,b1)
# a2,b2 is the posterior distribution pj|data~gamma(a2,b2)
a1 = 0.01
b1 = 0.01

# Declaring Success: P(pj > theta1) > delta_u
# Temporary Suspension: P(pj > theta2) < delta_l
theta1 = 0.05
theta2 = 0.3
delta_l = 0.05
delta_u = 0.8


# call the function
ar_results = AR(t, n0, n1, N, a1, a2, theta1,theta2, 
                delta_l, delta_u, drop, trup)

# call the function
er_results = ER(t, n0, n1, N,  theta1, theta2, delta_l, 
                delta_u, trup)

# boxplot
bp(er_p = er_results[[1]], ar_p = ar_results[[1]], c("Response Rate"))
bp(er_p = er_results[[2]], ar_p = ar_results[[2]], c("Sample Size"))

# 2*2 table
declaring_success(er_s_summary = er_results[[4]], 
                  ar_s_summary = ar_results[[4]])
