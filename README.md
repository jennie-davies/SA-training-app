
Shiny app to compare the effect of different parameters on example series using JDemetra+ 
-----------------------------------------------------------------------------------------

Code to produce an R Shiny app which allows the user to compare seasonal adjustments using different parameters.

Author: Jennifer Davies.

Disclaimer: licence.txt applies.

Data: Source ONS


Overview:
---------

	- compare two seasonal adjustments for four example time series
	- seasonal adjustment uses JDemetra+
	- change the following parameters:
		- transformation
		- Easter
		- seasonal moving average
		- trend moving average
		- seasonal break 
	- output are:
		- overlay graph comparing the original series and two seasonally adjusted series
		- overlay graph comparing the original series and two trends
		- SI ratio charts for both seasonal adjustments
		- table with the specifications selected by a defualt seasonal adjustment
	
Quick guide:
------------

	1. download zipped repository (green ''Clone or download'' button)
	2. unpack into desired project directory
	3. download the jdemetra-R repository and unpack into the same directory. This can be found here https://github.com/nbbrd/jdemetra-R
	4. set the 'jdemetra-R-master' folder in this directory as the working directory
	5. run the app using the 'Run App' button in R Studio, or the command shiny::runApp('SA-training-app/Code') in the R GUI
					 
	                        
Dependencies:
-------------

	- The following R packages will need to be installed
		1. shiny
		2. ggplot2
		3. rjava

Directory structure:
-----------------------------------------------------

	- code includes the server and ui files for the app
	- data contains the example series
