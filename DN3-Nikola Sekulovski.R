################################# NALOGA 8 - 48 ########################## 
n = 300
x = 13
p = x / n
alpha = 0.05 # Interval zaupanja je 1 - alpha = 0.95 torej 95%

interval.binom = function(alpha, p){ #podaj alpha in p
  z = qnorm(alpha/2, 0, 1, lower.tail = FALSE) 
  l = p - z*sqrt((p*(1-p)/n)) # spodnja meja
  u = p + z*sqrt((p*(1-p)/n)) # zgornja meja
  
  
  print(paste((1-alpha)*100, "% Interval zaupanja: [" , round(l,2), ",", round(u,2), "]"))
}

interval.binom(alpha, p)

################################# NALOGA 9 - 70 ########################## 

upper_perc_point = qchisq(0.05,3,lower.tail = FALSE) #zgornja odstotna točka
upper_perc_point

#p value
p_val = pchisq(27.001,3,lower.tail = FALSE) #the test statistic is on the bound of the rejection region, which is on the left
p_val #od te točke naprej je območje zavrnitve

