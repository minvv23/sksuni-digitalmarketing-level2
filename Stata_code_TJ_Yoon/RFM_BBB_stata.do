****************************************
* RFM ANALYSIS IN THE BOOKBINDERS CASE *
****************************************

*************
* IMPORTANT *
*************
/*
1. You need to manually load the data file bbb.dta before running this file
   --> See the Stata Tutorial for how to do this

2. You need to manually open a results log file before running this file
   --> See the Stata Tutorial for how to do this
*/

*******************
* ABOUT THIS FILE *
*******************

/*
This allows you to replicate the analysis in the note

"Recency, Frequency and Monetary (RFM) Analysis"

with the Bookbinders data. You do not have to go through this when you prepare
the Bookbinders case; running and analyzing this is optional.

However, this file will be important because it contains everything you will
need to do for the "Tuango" assignment:

You can execute this entire file as a .do file, i.e. "do RFM_BBB_stata.do" 
or command by command.


In case you are wondering: Use * to comment out a single line. Use /*  */ to
comment out a whole paragraph or section.
*/

**************************************
/* Calculating and Graphing Quintiles */
**************************************

* Create Recency quintiles: "rec_quin" will contain the number of the quntile, 
* nquantile(5) means we are creating quintiles, if we wrote nquantile(10) we would be 
* creating deciles.
* Question: Will customers with the most recent purchases (i.e. the best customers) 
* be first quintile? 

qscatter buyer last, name(qscatter_last, replace)

* Since xtile always places the smallest values of a variable in the first quintile,
* the answer is yes! Hence, the variable we create with xtile is what we want:

xtile rec_quin=last, nquantile(5)

* Lets double check: We should find that the highest purchase probability is found for
* consumers in quintile 1.
* Here is a graph.

graph bar (mean) buyer, over(rec_quin) name(buyer_by_rq, replace)

* Create Frequency quintiles: Question: Will the customers with the most frequent 
* purchases (i.e. the best customers) be in the first quintile?

qscatter buyer purch, name(qscatter_purch, replace)

* The answer is no, because the best customer are those with the highest values
* for purch. As a result, after creating quintiles we need to reverse the number 
* of the quintiles:  1 becomes 5 and 5 becomes 1

xtile freq_quin=purch, nquantile(5)
replace freq_quin=6-freq_quin

* If this worked we should find that the highest purchase probability 
* is found for consumers in frequency quintile 1.

graph bar (mean) buyer, over(freq_quin) name(buyer_by_fq, replace)

* Create Monetary quintiles: Question: Will the customers with the most monetary 
* value (i.e. the best customers) be in the first quintile?
 
qscatter buyer total_, name(qscatter_total, replace)

* The answer is no, because the best customer are those with the highest values
* for total_. As a result, after creating quintiles we need to reverse the number 
* of the quintiles:  1 becomes 5 and 5 becomes 1

xtile mon_quin=total_, nquantile(5)
replace mon_quin=6-mon_quin

* If this worked we should find that the highest purchase probability 
* is found for consumers in frequency quintile 1.

graph bar (mean) buyer, over(mon_quin) name(buyer_by_mq, replace)

******************************************************
*  Contructing an RFM Index with independent N-tiles *
*                                                    *
*  NOTE: THIS IS TOTALLY OPTIONAL AND WILL NOT BE    *
*        REQUIRED FOR ANY ASSIGNMENT                 *
*  The index is alreay created for you in the case   *
******************************************************

* To distinquish between independent, sequential, and intuitive
* approaches we carry the ending "_iq" for independent quintile appoach
* in this subsection. 

* Create an RFM Index for independent quintiles. For example, 111 means 
* that the customers belongs to the first quintile in R, F, and M.
* 243 means that the customers belongs to the second quintile in recency, 
* the fourth quintile in frequency and the third quintile in monetary.

gen rfmindex_iq=100*rec_quin +10*freq_quin+mon_quin

* Graph the RFM index

graph bar (mean) buyer, over(rfmindex_iq) name(buyer_by_rfm_iq, replace)

******************************************************
*  Contructing an RFM Index with sequential N-tiles  *
*                                                    *
*  NOTE: THIS IS TOTALLY OPTIONAL AND WILL NOT BE    *
*        REQUIRED FOR ANY ASSIGNMENT                 *
*  The index is alreay created for you in the case   *
******************************************************

* Create Recency quintiles (we use "_sq" to mean "sequential quintiles")

xtile rec_sq=last, nquantile(5)

* For each Recency quintile, construct a Frequency quintile
* I have implemented this as a loop over the recency quintiles.
* For example, in for the first recency quintile, i.e. rec_sq==1
*    xtile freq_sq1=purch if rec_sq==1, nquantile(5)
*    replace freq_sq=freq_sq1 if freq_sq==.
*    drop freq_sq1
* This first calculates the quintile and puts the number in a temporary
* variable freq_sq1. Then I copy the result in the variable freq_sq
* Finally I delete the temporary variable freq_sq1.
* Some times the "xtile" command will leave a certain quintile empty 
* when there are only few value of the underlying variable. For example, 
* imagine a variable only has the value 34, 37, 40 and we try to put this 
* variable into quintiles. Then 2 quintiles will be empty. 
* To make sure that the fact that some quintiles do not contain any data
* does not yield an error message, we put the "capture" command before
* each command. This will make sure that the program continues without
* giving an error message.

gen freq_sq=.
forvalues i=1/5 {
   capture xtile freq_sq`i'=purch if rec_sq==`i', nquantile(5)
   capture replace freq_sq=freq_sq`i' if freq_sq==.
   capture drop freq_sq`i'
}

* Are the best customers in the first quintile of frequency-> NO!

replace freq_sq=6-freq_sq

* For each Recency quintile and Frequency quintile, construct a Monetary Value quintile

generate mon_sq=.
forvalues i=1/5 {
   forvalues j=1/5 {
      capture xtile mon_sq`i'`j'=total_ if rec_sq==`i' & freq_sq==`j', nquantile(5)
      capture replace mon_sq=mon_sq`i'`j' if mon_sq==.
      capture drop mon_sq`i'`j'
   }      
}

* Are the best customers in the first monetary quintile -> NO!

* So, reverse the number of the quintiles: 1 becomes 5 and 5 becomes 1

replace mon_sq=6-mon_sq

* Create an RFM Index for sequential quintiles. For example, 111 means 
* that the customers belongs to the first quintile in R, F, and M.
* 243 means that the customers belongs to the second quintile in recency, 
* the fourth quintile in frequency and the third quintile in monetary.

gen rfmindex_sq=100*rec_sq +10*freq_sq+mon_sq 

* Graph the RFM index

graph bar (mean) buyer, over(rfmindex_sq) name(buyer_by_rfm_sq, replace)

*******************************************************************
/* Calculating the average response rate without targeting */
*******************************************************************

sum buyer

*******************************************************************
/* Calculating the average response rate and profitability cutoff 
   for the independent quintile RFM Index */
*******************************************************************

* We now need to calculate the average response rate for each RFM Index.
* For this we want to create a new variable that contains for each individual
* the response rate of the RFM Index to which they belong. For example, if an
* individual is on index 243, we want the new variable to contain the average 
* response rate for all individuals in index 243. Since "buyer" is coded 
* "0" or "1", by taking the average of buyer for a group of customers we get 
* the response rate. 
* We use a command called "rfmpredict" which does that automatically. 
* Type "help rfmpredict" into the Stata command line for a more detailed
* explanation.

rfmpredict rfm_response_iq=buyer, indexvar(rfmindex_iq) replace
 
* Now we want to create a variable that tells us whether we should or should 
* not mail a consumer. For this we first need to figure out the break-even
* response rate. This is determined from the information about mailing costs
* and profits for each purchase that is contained in the case. For Bookbinders
* the break-even response rate is 0.083 (or 8.3%). With this information we
* can create a 0/1 (dummy) variable.

generate mailto_iq=0
replace mailto_iq=1 if rfm_response_iq>0.083

tabulate mailto_iq

* We now display the percentage of consumers which will be mailed based on
* the independent quintile RFM analysis. This, together with the cost of mailing,
* the number of people in the database, and the profit per responder is all we 
* need to calculate (by hand!) gross profits, gross profits as a % of gross sales, 
* and the return on marketing expenditure (gross profit/cost to mail catalogs)

sum buyer if mailto_iq==1

*******************************************************************
/* Calculating the average response rate and profitability cutoff 
   for the sequential quintile RFM Index */
*******************************************************************

* For an explanation of the commands, please see previous section on
* independent quintile RFM index

rfmpredict rfm_response_sq=buyer, indexvar(rfmindex_sq) replace

generate mailto_sq=0
replace mailto_sq=1 if rfm_response_sq>0.083

tabulate mailto_sq

sum buyer if mailto_sq==1
