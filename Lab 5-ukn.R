# code for lab 5

# mybin()
mybin = function(iter = 1000, n = 10, p = 0.5){
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)
  succ = c()
  for (i in 1:iter){
    sam.mat[,i] = sample(c(1,0), n, replace = TRUE, prob = c(p, 1-p))
    succ[i] = sum(sam.mat[,i])
  }
  
  succ.tab = table(factor(succ, levels = 0:n))
  
  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)
  lab = paste(iter.lab, n.lab, p.lab, sep = ", ")
  barplot(succ.tab / (iter), col = rainbow(n+1), main = "Binomial Simulation", sub = lab, xlab = "The Number of Successes")
  succ.tab/iter
}

# estimating
db.tab = round(dbinom(0:10, size = 10, prob = 0.7), 4)
names(db.tab) = 0:10
db.tab

# myhyper()
myhyper = function(iter = 100, N = 20, r = 12, n = 5) {
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)
  succ = c()
  
  for(i in 1:iter) {
    sam.mat[,i] = sample(rep(c(1,0), c(r, N - r)), n, replace = FALSE)
    succ[i] = sum(sam.mat[,i])
  }
  succ.tab = table(factor(succ, levels = 0:n))
  
  iter.lab = paste0("iter = ", iter)
  N.lab = paste0("N = ", N)
  r.lab = paste0("r = ", r)
  n.lab = paste0("n = ", n)
  lab = paste(iter.lab, N.lab, r.lab, n.lab, sep = ", ")
  barplot(succ.tab / (iter), col = rainbow(n+1), main = "Hypergeometric Simulation", sub = lab, xlab = "The Number of Successes")
  succ.tab / iter
}

# mysample()
mysample = function(n, iter = 10, time = 0.5) {
  for(i in 1:iter) {
    s = sample(1:10, n, replace = TRUE)
    sf = factor(s, levels = 1:10)
    barplot(table(sf)/n, beside = TRUE, col = rainbow(10), main = paste("Example sample()", "iteration", i, "n = ", n, sep = " "), ylim = c(0, 0.2)
    )
    Sys.sleep(time)
  }
}



