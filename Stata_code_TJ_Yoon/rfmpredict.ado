*!  VERSION 0.1  10dec2014 
*!  Florian Zettelmeyer
*!  See help file for description

program define rfmpredict, eclass
version 11.0

syntax name =/exp, INDexvar(varname numeric) [TRAINvar(varname numeric)] [REPlace]

* Initalize temporary variables.
tempvar temp trainvaruse
local scorevar="`namelist'"

* Check whether scorevariable exists and needs to be replaced
	
capture confirm variable `scorevar'
	if !_rc {
		if "`replace'"=="" {            
			di in red `"Variable `scorevar' already exists. Use "replace" option to overwrite."'
			error 110		
			}
        else {
			drop `scorevar'
			qui gen `scorevar'=.
        	}
            }
	else {
                       qui gen `scorevar'=.
               }

* Check wether training variable is dummy 
if "`trainvar'"~="" {
	qui levelsof `trainvar', local(trainvarvalues)
	foreach level of local trainvarvalues {
		if `level'~=0 & `level'~=1 {
			display as error `"The training sample dummy variable "`trainvar'" specified in"' 
			display as error "trainvar() contains values other than 0 or 1"
			error 198			
		}
	}
* set variable for the if statement in egen if a training variable was specified
	gen `trainvaruse'=`trainvar'
}
else {
* set variable for the if statement in egen if a training variable was not specified
	gen `trainvaruse'=1
}

* Options and syntax checks.

* check whether depvar is numeric and exists
local depvar="`exp'"

confirm numeric variable `depvar'

* Check wether dependent variables is dummy 

qui levelsof `depvar', local(depvarvalues)
foreach level of local depvarvalues {
	if `level'~=0 & `level'~=1 {
		display as error `"The outcome variable "`depvar'" "' 
		display as error "contains values other than 0 or 1"
		error 198			
	}
}

qui levelsof `indexvar', local(indexvarvalues)
local numindex: word count `indexvarvalues'
	if `numindex'<=20 {
		display as error `"Warning: The index variable "`indexvar'" "' 
		display as error "splits the data into less than `numindex' RFM cells"
		display as error "(Predictions are still calculated)"
	}


* Do RFM procedure

capture drop `scorevar'
qui egen `scorevar'=mean(`depvar') if `trainvaruse', by(`indexvar')
qui egen `temp'=max(`scorevar'), by(`indexvar')
qui replace `scorevar'=`temp' if `scorevar'==.

display `"(The variable "`scorevar'" contains the new RFM prediction)"'

ereturn clear
ereturn local scorevar = "`scorevar'"
ereturn local indexvar = "`indexvar'"
ereturn local depvar = "`depvar'"
ereturn local cmd = "rfmpredict"

end

