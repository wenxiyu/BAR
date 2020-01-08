

 
  declaring_success= function(er_s_summary,ar_s_summary){
  ar_twobytwo = twobytwo(ar_s_summary)
  er_twobytwo = twobytwo(er_s_summary)
  print("Adaptive Randomization")
  print(ar_twobytwo)
  print_success(ar_twobytwo)
  print("==========================")
  print("Equal Randomization")
  print(er_twobytwo)
  print_success(er_twobytwo)
 }
 
print_success = function(ar_twobytwo){
  type1 = ar_twobytwo[2, 1]/ rowSums(ar_twobytwo)[2]
  type2 = ar_twobytwo[1, 2]/ rowSums(ar_twobytwo)[1]
  print(paste("Type I error is",type1))
  print(paste("Type II error is",type2))
  print(paste("Power is", 1-type2))
  
}

 twobytwo = function(s_summary){
   results = matrix(ncol = 2, nrow = 2)
   results[1, 1] = colSums(s_summary, na.rm = TRUE)[1]
   results[1, 2] = nrow(s_summary) - results[1, 1]
   results[2, 1] = sum(colSums(s_summary, na.rm = TRUE)[-1])
   results[2, 2] = nrow(s_summary) * (ncol(s_summary) - 1) -
                    results[2, 1]
   colnames(results) = c("Success", "Not Declared as Success")
   rownames(results) = c("Arm 1", "Other Arms")
   return (results)
 }