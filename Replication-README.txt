These files replicate the results in “Dissertation: Quantile Selection Models”
By Ellen Jung Hyun Kim, Ulrich Roschitsch and Thomas Schwarz

January 29, 2020

*******************************************************************************************
Prerequisites
*******************************************************************************************
These files require a working installation of R and Matlab. Please unpack the zip container
CPSData before running the code.

*******************************************************************************************
Replication
*******************************************************************************************

The plots in the paper can be replicated by running quant_reg.R using the attached saved
files.
For a full replication:
	first)		 run get_CPS_Data.R,
	second)	 run AB_QRS_code/Generic_code/Code_QRS.m
	third)		 run quant_reg.R

********************************************************************************************
File Structure
********************************************************************************************
get_CPS_Data.R:

	imports the IPUMS CPS data set, defines  the necessary variables,
	performs the first stage probit regression and saves the resulting data frame
	including the fitted probit values in cps_adults.csv.

	The code calculates the number of children for each individual as well as the total
	amount of income from other family members. This process is very time-intensive.
	Instead of re-calculating these you may import a saved version (cps_data_addedvars)
	by specifying the option do_load.

	input:
		- cps_00006.xlm
		- cps_00006.dat

		if do_load:
			- cps_data_addedvars

	output:

		if do_save:
			- cps_data_addedvars

		if do_write:
			- cps_adults.csv

___________________________________________________________________________________________________

AB_QRS_code/Generic_code/Code_QRS.m

	This script is a modified version from Arellano and Bonhomme. It computes the
	selection corrected quantile regression estimates using a Gaussian copula.

	calls
		../rq/rq.m <- the quantile regression estimation function of Daniel Morillo &
					Roger Koenker, translated from Ox by Paul Eilers

	input:
		- cps_adults.csv

	output:
		- corrected_quantreg_beta.txt <- beta coefficients from the corrected quantile regression

___________________________________________________________________________________________________

quant_reg.R

	This script estimates the uncorrected quantile regression specification, a Heckman-type
	mean regression and plots the results together with the selection corrected quantile
	estimates.

	input:
		- cps_adults.csv
		- corrected_quantreg_beta.txt
