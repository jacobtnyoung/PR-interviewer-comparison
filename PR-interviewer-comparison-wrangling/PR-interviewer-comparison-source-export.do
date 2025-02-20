******************************************************************
* PAPER TITLE
******************************************************************

* This is the code to pull the raw file and select the variables.
* It creates two files that have separate items.


* ============================================================== *
* Get the items for analzying differences.

* Call the file.
use "/Users/jyoung20/ASU Dropbox/Jacob Young/Perryville PAR/PV PAR Interviews Full_CLEANED.dta", clear


* Select the variables.
keep id InterviewerType Randomize S4Q1 lage ///
	 white black hispanic other S4Q3 ///
	 single has_children has_minor_children ///
	 firsttimer timein_yrs stretch_yrs lifer ////
	 S3SS1_1-S3SS1_15 ///
	 S3SS2_1-S3SS2_14 ///
	 S3SS3_1-S3SS3_7


* Drop the value labels.
label drop _all 


* Write an outfile.
outsheet using PR-interviewer-comparison-vars.csv , comma replace
