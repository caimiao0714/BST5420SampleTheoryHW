LIBNAME F '/folders/myfolders/BRFSS'; RUN;

PROC IMPORT datafile="/folders/myfolders/BRFSS/old_diabetes_brfss2017.csv"
     out=F.DAT
     dbms=csv
     replace;
     getnames=YES;
run;


PROC SURVEYMEANS
	data = F.DAT;
	/* Use common weight because question is from the core section */
	weight _llcpwt;
	strata _ststr;
	cluster _psu;
	var physhlth;
RUN;

proc surveyfreq
	data = F.DAT;
	/* Use common weight because question is from the core section */
	weight _llcpwt;
	strata _ststr;
	cluster _psu;
	table havarth3;
run;




DATA F.DAT1;
SET F.DAT;
	EYEEXAM11 = .;
	IF EYEEXAM IN (1, 2, 3, 4) THEN EYEEXAM1=1;
	ELSE EYEEXAM1 = 0;

PROC FREQ DATA=F.DAT1;
	TABLES EYEEXAM1 EYEEXAM;
	RUN;

* SURVEY LOGISTIC REGRESSION;
* https://support.sas.com/resources/papers/proceedings15/3320-2015.pdf;
proc surveylogistic data=F.DAT1 order=formatted;
	strata _ststr;
	cluster _psu;
	weight _llcpwt;
	model EYEEXAM1(event='1')= _AGEG5YR;
run;

proc genmod data=F.DAT1 order=formatted;
	strata _ststr;
	weight _llcpwt;
	class EDUCA(ref='1');
	model EYEEXAM = _AGEG5YR EDUCA/ dist=poisson;
run;



































































