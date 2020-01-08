# Boxplot
# er_p: response rate/sample size summary of ER, t*narm
# ar_p: response rate/sample size summary of AR, t*narm
# name: y_axis title, chooose either Response Rate or Sample Size

bp = function(er_p, ar_p, name){
er_p = as.data.frame(er_p)
ar_p = as.data.frame(ar_p)

narm = ncol(er_p)
t = nrow(er_p)

bp_p_1 = data.frame(as.vector(as.matrix(er_p)),
                rep(c(seq(1, narm, 1)), each = t),
                rep(0, t*narm))
colnames(bp_p_1) = c("p", "group", "AR")
bp_p_2 = data.frame(as.vector(as.matrix(ar_p)),
                  rep(c(seq(1, narm, 1)),each = t),
                  rep(1, t*narm))
colnames(bp_p_2) = c("p","group","AR")
bp_p = rbind(bp_p_1, bp_p_2)
rm(bp_p_1, bp_p_2)

myplot = boxplot(p ~ AR*group , data = bp_p  , boxwex = 0.4 , 
               ylab = name,
               main = "" , col = c("grey" , "darkseagreen2") ,  xaxt = "n")
axis(1, at = seq(1.5 , 10 , 2), labels = c(c(seq(1, 5, 1))), 
         tick = FALSE , cex = 0.3)
for(i in seq(0.5 , 20 , 2)){ abline(v = i,lty = 1, col = "grey")}
legend("topright", legend = c("ER", "AR"), 
       col = c("grey" , "darkseagreen2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.2, 
       horiz = F, inset = c(0.1, 0.1))
}
