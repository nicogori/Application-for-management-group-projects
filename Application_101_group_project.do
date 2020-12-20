*==================================================================================================================================================================================================*****/

log using "/Users/nicologori/Desktop/Application for Management /Group project/Final_output.smcl"
use "/Users/nicologori/Desktop/Application for Management /STATA/ESS9e02(2)(1).dta", clear

/*****==================================================================================================================================================================================================*
Y = Trust in the legal system, Trust in the police, Trust in politicians, trust in political parties, Trust in the European Parliament, Trust in the United Nations and Trust in the country's parliament
X = Use of internet = "Internet use, how often", "Internet use, how often on a typical day, in minutes"
Control variables = “Placement on the left-right scale”, "How interested in politics", "News about politics and current affairs, watching, reading or listening, in minutes", “Age of respondent”, "Gender", “Years of full-time education completed”, "Most people can be trusted or you can't be too careful", "Most people try to take advantage of you, or try to be fair", "Most of the time people helpful or mostly looking out for themselves"

*======================================================================================================================================================================================================*/
*=======================================================================================================================================================================================================*
							
									***DATA SCREENING***
		
* Preliminary screening (Missing values--Impossible or improbable values--Outliers--Non-normality)
	//ESS-Round 9 dataset, not so much data screening required as it's a sound secondary dataset already examined
	//Check of variables of interest
des trs* netusoft netustm lrscale nwspol polintr gndr agea eduyrs ppltrst pplfair pplhlp
	//Most of the variables are on Likert-Scale and thus do not require deep investigation
codebook trs* netusoft netustm lrscale nwspol polintr gndr agea eduyrs ppltrst pplfair pplhlp
tabstat trs* netusoft netustm lrscale nwspol polintr gndr agea eduyrs ppltrst pplfair pplhlp, stats(count, me, co, max, min, sd, v, sk, k)
	//Some variables might have to be inspected due to their distribution being non-normal and with outliers 
	//FA does NOT require normality for variables
misstable sum trs* netusoft netustm lrscale nwspol polintr gndr agea eduyrs ppltrst pplfair pplhlp
mdesc trs* netusoft netustm lrscale nwspol polintr gndr agea eduyrs ppltrst pplfair pplhlp
	//'lrscale' might be excluded from the analysis as it's not very important for the research and has high % of missing values
	//Missing values for trust variables will already be dropped when using the command 'egen, rowmean()' to derive their arithmetic mean
drop if nwspol > .
drop if polintr > .
drop if agea > .
drop if eduyrs > .
drop if netustm==.d | netustm==.b | netustm==.c
drop if ppltrst > .
drop if pplfair > .
drop if pplhlp > .
drop if trstplt > .
drop if trstprl > .
drop if trstprt > .
	//Proceded with listwise deletion to have sounder result and consistent number of observations in the regressions
	//The missing cases were almost always non-significant therefore they were dropped mainly for good practice
	
* Univariate data screening
	//We decided to recode the values that, on variables using time as their scale, took the value of the whole day(24h)
	//as we assumed that it is not humanly possible to pursue an action for 24h and that an average individual takes at least
	//6h of sleep.
	//We therefore restored this values to the maximum we allow it to take which is 1080 minutes, however they will be probably 
	//dropped when inspecting for outliers
drop if netustm > 1080 & netustm <= 1440
drop if nwspol > 1080 & nwspol <= 1440

	//Outliers detection, just on continuous variables:
	//Most of our variables are on likert-scale and therefore do not require this check
//netustm
display 198.9513 + 168.4026*2.5
	//619.9578
display 198.9513 - 168.4026*2.5
drop if netustm > 619 & netustm!=.a

//nwspol
display 85.9126 + 137.5469*2.5
	//429.77985
display 85.9126 - 137.5469*2.5
drop if nwspol > 429

//eduyrs
display 12.90902 + 4.07416*2.5
	//23.09442
display 12.90902 - 4.07416*2.5
	//2.72362
drop if eduyrs > 23 | eduyrs < 3

/*It was assumed that the missing values 'Not applicable' (those that in the previous survey's: "Never", "Only occasionally" or "A few times a week"), and coded in stata as '.a' lied in the category of low usage of the technology*/
tabstat netustm, stats(q)
	//To identify quartiles
//Therefore the variable was divided in five categories: 			
* "Light users" (>=0 and <=65)
replace netustm = 2 if netustm>=0 & netustm<=60 & netustm!=.a
* "Medium-light users" (>65 and <=150) 
replace netustm = 3 if netustm>60 & netustm<=120 
* "Moderate use of Internet" (>150 and <=240)
replace netustm = 4 if netustm>120 & netustm<=240 
* "Heavy users" (>240)
replace netustm = 5 if netustm>240 & netustm <=619 
* "Non-users" (missing values "Not applicable")
replace netustm = 1 if netustm==.a 
//And renamed "Use of Internet"
rename netustm internet_use
tab internet_use, miss

//Rescaling of polintr to make it concordant with the other variables scales
gen polintr_ordered = .
replace polintr_ordered = 1 if polintr == 4
replace polintr_ordered = 2 if polintr == 3
replace polintr_ordered = 3 if polintr == 2
replace polintr_ordered = 4 if polintr == 1

*'agea'
tabstat agea, stats(q)
pwcorr internet_use agea
//The correlation between 'internet_use' and 'agea' is quite strong but it was decided to ignore that as of now
tabstat agea, stats(q)
//To check for the quartiles which are then used to divide age in categories
*The variable 'agea' was divided in 4 categories to use it in the regression as a dummy and better capture the phenomenon
gen newvar = . 
//Then replace the missing values with a different value for each age group as appropriate
replace newvar = 1 if agea > 14 & agea <=37 
replace newvar = 2 if agea > 37 & agea <=52
replace newvar = 3 if agea > 52 & agea <=66
replace newvar = 4 if agea > 66
rename newvar age_categories

tabstat internet_use age_categories polintr_ordered eduyrs nwspol, stats(count, me, co, max, min, sd, v, sk, k)

//log transformation on nwspol
tab nwspol, miss
//as nwspol takes also the value 0 (3,071 observations) that would subsequently be accounted as missing values, we take the log of (nswpol+1) to smooth kurtosis and skeweness
gen log_nwspol = ln(nwspol+1)
hist log_nwspol, freq norm
tabstat internet_use age_categories polintr_ordered eduyrs log_nwspol, stats(count, me, co, max, min, sd, v, sk, k)

*=======================================================================================================================================================================================================*
*=======================================================================================================================================================================================================*
							
									***FACTOR ANALYSIS***	

* Check the Factor Analysis assumptions
corr trs*
pwcorr trs*, sig
	//The variables are almost all strongly correlated
	//Due to the scope of the research the factor that needs to be extracted is the one regarding 
	//trust in internal politics
	//Factor analysis is used to check if the expectations match empirical proofs
factortest trs*
	//Determinant = 0.006 --> almost 0 but not 0
	//Barlett test of sphericity: refuse null hypothesis that variables are not intercorrelated (p-value<0.05)
	//KMO MSA = 0.853
	//All the assumptions are verified
	
//The first factor explains 65% of all the variance, so we might consider using only this as a unique dependent variable: 'Trust in government'
//Latent root criteria, factor with eigen>1: here again we would just use the first factor 
//Use factors that explain the most variance, if we use 3 factors we can explain 86% of the total variance

* We decide to create 3 factors*
factor trs*
estat kmo
	//All individual variables' MSA are very high
rotate, promax blank(0.4)
	//Used oblique rotation to allow for correlation between the factors 
sortl
	//The rotated matrix is very easy to interpret:
	//one for "trust in own country's political system" - Politicians, Pol. parties, Parliament
	//one for "trust in own country's judicial system" – legal system and police
	//and one for "trust in international institutions" - UN, UE Parliament

* The aim is to retain only the factor for  "trust in own country's political system", as it is the main representative of the research question
//Assess the internal consistency of the three subsets of indicators for each factor using Cronbach’s alpha.
alpha trstprl trstplt trstprt, label std item

//All our trs* variables have the same scale and therefore we can create their summated scales as their mean
/* It creates the (row) means of the variables in varlist, ignoring missing values; for example, if three variables are specified and, in some observations, one of the variables is missing, in those observations newvar will contain the mean of the two variables that do exist.  Other observations will contain the mean of all three variables.  Where none of the variables exist, newvar is set to missing */
egen political_trust = rowmean (trstprl trstplt trstprt)


alpha ppltrst pplfair pplhlp
egen social_trust = rowmean(ppltrst pplfair pplhlp)	

mdesc political_trust internet_use  log_nwspol polintr_ordered gndr age_categories eduyrs social_trust

tabstat political_trust social_trust internet_use  age_categories log_nwspol polintr_ordered, stats(count, me, co, max, min, sd, v, sk, k)

*=======================================================================================================================================================================================================*
*=======================================================================================================================================================================================================*
										
										***REGRESSION***
										
//Generate overall weight to make analysis on Europe-level
gen overallwght = pspwght * pweight


//Simple linear regression
reg political_trust i.internet_use  [w=overallwght], beta
//Check on heteroskedasticity	, heteroskedasticity present possibly caused by ovb
estat hettest	
	//Heteroskedasticity
//Multicollinearity test
vif
	//VIF ok

/*	R-squared       =    0.0114
	Adj R-squared   =    0.0113
*/


//Add controls for individual characteristics																			
reg political_trust i.internet_use i.gndr i.age_categories [w=overallwght], beta

//Check on heteroskedasticity
estat hettest	
	//homoskedastik
	
//Multicollinearity test
vif
	//VIF ok

/*	R-squared       =    0.0165
	Adj R-squared   =    0.0163
*/


//Add control for literacy
reg political_trust i.internet_use i.gndr i.age_categories eduyrs [w=overallwght], beta
//Check on heteroskedasticity
estat hettest	
	//Causes heteroskedasticity
//Multicollinearity test
vif

/*	R-squared       =    0.0235
	Adj R-squared   =    0.0233
*/

corr polintr_ordered political_trust
pwcorr polintr_ordered internet_use
		

//Model with all the controls and regressors
reg political_trust i.internet_use i.gndr i.age_categories eduyrs social_trust log_nwspol polintr [w=overallwght], beta 
//Check on heteroskedasticity
estat hettest
	//homoskedastik
vif
	//Some variables are slighly off 2.0, overall ok

/*	R-squared       =    0.1931
	Adj R-squared   =    0.1929
*/

//Model with all the controls and regressors, excluding polintr
reg political_trust i.internet_use i.gndr i.age_categories eduyrs social_trust nwspol [w=overallwght], beta 
//Check on heteroskedasticity
estat hettest
	//homoskedastik
vif
	//Some variables are slighly off 2.0, overall ok

/*	R-squared       =    0.1679
	Adj R-squared   =    0.1676
*/


//Model with all the controls and regressors, excluding polintr and using log of nwspol
reg political_trust i.internet_use i.gndr i.age_categories eduyrs social_trust log_nwspol  [w=overallwght], beta 
//Check for residuals
predict y_hat
predict residuals, r
browse political_trust y_hat residuals
sum residuals, det	
scatter residuals y_hat
qnorm residuals
pnorm residuals
//Check on heteroskedasticity
estat hettest
	//homoskedastik
vif
	//Some variables are slighly off 2.0, overall ok

/*	R-squared       =    0.1715
	Adj R-squared   =    0.1713
*/



	
	
	



Even extreme multicollinearity (so long as it is not perfect) does not violate OLS assumptions. OLS estimates are still unbiased and BLUE (Best Linear Unbiased Estimators)
 the law of large numbers will kick in and assure that the distributions of the t-statistics you rely on for inference are well approximated by the standard normal distribution, so all of your tests and p-values will be correct
Normality of residuals is only needed in small samples.

Con una ols l'utilizzo di variabili categoriche porta alla violazione di alcune assumption nel dettagli abbiamo porblemi di heteroskedasticity perchè l'errore non segue più un andamento lineare e qunidi non è più "approssimabile/accettabile".






//IMPORTANT GRAPH

collapse (mean) political_trust social_trust, by(cntry)
list

