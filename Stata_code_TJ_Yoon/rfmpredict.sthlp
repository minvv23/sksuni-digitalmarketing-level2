{smcl}
{* *! version 0.1  10dec2014}{...}
help for {cmd:rfmpredict}
{hline}

{title:Title}

{p2colset 2 17 19 2}{...}
{p2col:rfmpredict {hline 3}}Predict response rates by RFM index/cell{p_end}
{p2colreset}{...}

{title:Syntax}

{p 8 18 2}
{bf:rfmpredict} {newvar} {cmd:=} {varname}, indexvar({varname}) [options]

{synoptset 25 tabbed}{...}
{p2coldent:{it:required}}description{p_end}
{synoptline}
{synopt:{newvar}}the name of variable that will contain response rates by RFM index/cell (i.e. the scoring variable).{p_end}
{synopt:{varname}}the name of the dependent variable (i.e. the purchase/sale variable).{p_end}
{synopt:{opth indexvar(varname)}}is a required option that specifies the name of the RFM index/cell variable.{p_end}
{synoptline}


{synoptset 25 tabbed}{...}
{synopthdr}
{synoptline}
{synopt:{opth trainvar(varname)}}allows the RFM response rate prediction to be 
based only on observations in the training sample. If used, {varname} should be 
a dummy that is 1 if an observations is in the 
training sample and 0 if the observation is in the validation sample. If the 
option is omitted, the 
full dataset is used. 
Note: The prediction is always made for all observations in the data: Even if predictions are based on the training sample only, 
observations in the validation sample are assigned the same prediction as 
observations with the corresponding RFM index in the training sample. {p_end}
{synopt:{opt replace}}instructs Stata to overwrite the variable {newvar} if it 
already exists. This is the variable that will contain response rates by RFM 
index (i.e. the scoring variable). {p_end}
{synoptline}


{title:Description}

{pstd}
{opt rfmpredict} predicts purchase probabilities by RFM index/cell. The command requires users to specify the (new) variable name 
where the prediction should be placed, the dependent variable used to calculate 
the prediction, and the variable containing the RFM index.

{pstd}
This command does a very simple thing: Suppose the dependent variable is "buyer" 
and the RFM index is "rfmindex_iq". then the command simply executes: 

{phang}{cmd:. egen rfm_response_iq=mean(buyer), by(rfmindex_iq)}{p_end}

{pstd}
The value of having this command arises if one wants to use a training sample to 
predict. Using the {opt egen} command above with a training sample creates the 
problem that the prediction is only made for the training sample but not for the 
validation sample. As a result, one needs to also write:

{phang}{cmd:. egen temp=max(rfm_response_iq), by(rfmindex_iq)}{p_end}
{phang}{cmd:. replace rfm_response_iq=temp if rfm_response_iq==.}{p_end}
{phang}{cmd:. drop temp}{p_end}

{pstd}
This command gets rid of the need to make this adjustment. 

{title:Examples}

{pstd}
Suppose we have created an RFM index contained in "rfmindex_iq" and suppose further that the dependent variable is "buyer". 
Lets create a new variable "rfm_pred_prob" that contains the predicted purchase probability (i.e. it is the scoring variable). 
Suppose further that "training" is a variable that is "1" if the observation is in the training sample and "0" if it is in the validation sample.
 
{phang}{cmd:. rfmpredict rfm_pred_prob=buyer, index(rfmindex_iq)}{p_end}
{phang}{cmd:. rfmpredict rfm_pred_prob=buyer, index(rfmindex_iq) replace}{p_end}
{phang}{cmd:. rfmpredict rfm_pred_prob=buyer, index(rfmindex_iq) trainvar(training)}{p_end}
{phang}{cmd:. rfmpredict rfm_pred_prob=buyer, index(rfmindex_iq) trainvar(training) replace}{p_end}
{title:Author}

    Florian Zettelmeyer, Kellogg School of Management, Northwestern University
	f-zettelmeyer@kellogg.northwestern.edu
