use toolsexample5.dta, clear 
logistic sale st200 dis20 ceaut [fw=people] if partial==1
predict prob_partial
gen numsales=prob_partial*10000
