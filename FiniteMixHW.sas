 /*Author: Kubam Ivo*/
/*Date: 8/11/2019*/
/*Course: Topics In Advance Modelling Techniques*/
/*Part: Finite Mixture Models*/

*Creating a path macro variable;
%let filepath=E:\SEED\OneDrive\Msc. Biostatistics\Level Two\Advanced Modelling Techniques\Project;

*Creating a Library;
libname amt "&filepath";

*Creating User Defined formats;
proc format;
	value f_gender 0="Male" 1="Female";
	value f_group 0="Placebo" 1= "Acupuncture";
	value f_migraine 0="No" 1="Yes";
run;

*Reading the data set;
data amt.headache;
	set "E:\SEED\OneDrive\Msc. Biostatistics\Level Two\Advanced Modelling Techniques\Project\acuamt2019.sas7bdat";
format sex f_gender. migraine f_migraine. group f_group.;	
	frequency=ceil(frequency);
run;

proc contents data=amt.headache;
run;

*Reading the dataset with just counts of headache and frequency;
options validvarname=v7;
proc import datafile="E:\SEED\OneDrive\Msc. Biostatistics\Level Two\Advanced Modelling Techniques\Project\headache2.csv"
	out=amt.headache2 dmbs="csv" replace;getnames=Yes;
run;

data amt.headache2;
	set amt.headache2;
	keep count freq;
run;

/*data amt.headache2;*/
/*	input count freq @@;*/
/*	cards;*/
/*1	102 2	5 3	5 4	13 5	9 6	16 7	22 8	22 9	13 10	21 11	24 12	13 13	17 14	10 15	1 16	8 17	12 18	10*/
/*19	12 20	6 21	6 22	4 23	1 24	4 25	1 26	7 27	2 28	3 29	2 30	8 31	22*/
/*;*/
/*run;*/

proc contents data=amt.headache2;
run;

*Exploring dataset;
*Exporting output to excel;

title "summary statistics for the response variable";
title2 "By Predictor variables";
proc means data=amt.headache var mean std min max range maxdec=3;
run;
title;

title "summary statistics for Predictor Variables";
proc sort data=amt.headache out=headache_sort;
*by sex;
run;

ods graphics on;
proc freq data=headache_sort nlevels order=freq;
tables sex*group*migraine/list ;
run;
title;

title "Distribution of days with headache during a four-week recording period at the end of the study";
proc sgplot data= amt.headache;
histogram frequency/  binstart=0;
run;
title;


/*Fitting a single poisson model for the data*/

/*proc genmod data= amt.headache;*/
/*model frequency =  /link=log dist=poission scale=p;*/
/*run;*/
/**/
/*data poisson(keep=x);*/
/*call streaminit(4321);*/
/*lambda=4.45;*/
/*do i= 1 to 1000;*/
/*	x=rand("poisson",lambda);*/
/*	output;*/
/*end;*/
/*run;*/
/**/
/*proc sgplot data= poisson;*/
/*histogram x/  binstart=0;*/
/*label x ="Number of headache";*/
/*density x;*/
/*run;*/
/*title;*/
proc print data=amt.headache;
run;
/*Non parametric maximum likelihood estimate*/
data test;
	set amt.headache;
run;
data test;
	set amt.headache;
	do x=0 to 28 by 1; output;
	end;
run;
data test;set test;
gradient = pdf('POISSON',frequency,x)/(0.006711798*pdf('POISSON',frequency,0)+(0.251153245*pdf('POISSON',frequency,0.02148139)+0.056492825*pdf('POISSON',frequency,3.51534695)
								+0.279212202*pdf('POISSON',frequency, 7.42992944)+0.248212925 *pdf('POISSON',frequency,13.00459065)+0.158217004*pdf('POISSON',frequency,24.08266289)));
run;
proc sort data = test;
	by x;
proc means data=test;
	var gradient;
	by x;
	output out=out;

data out;
	set out;
	if _STAT_ = 'MEAN';

title h=2.5 'Gradient HEADACHE';
proc gplot data = out;
plot gradient*x /nolegend haxis=axis1 vaxis=axis2 cvref=blue vref=1;
symbol c=red i=join w=5 l=1 mode=include;
axis1 label=(h=2 'Lambda') value=(h=1.5) order=(0 to 28 by 4) minor=none w=6;
axis2 label=(h=2 angle=90 'Gradient') value=(h=1.5) order=(0.4 to 1.1 by 0.1) minor=none w=5;
run;
quit;

proc print data=test;
run;

/*Fitting a four component Poissson mixture*/
/*Fitting a finite mixed model*/
ods graphics on;
proc fmm data = amt.headache plots=density(bins=15);
	model frequency = / dist=poisson k=4 parms(0.24,2.18,2.79,3.31);
	probmodel /parms(0.69,0.96,0.24);
	label frequency ="Number of headache";
	output out=out_pred posterior class;
run;
ods graphics off;

title "Classification of count of number of headaches";
proc print data=out_pred;
run;
title;

/*MODEL EXTENSION WITH COVARIATES*/

/*Fitting a single poisson model*/
proc genmod data= amt.headache;
class migraine sex group;
model frequency = age migraine group sex /link=log dist=poisson scale=p;
run;

/*MODEL TWO K=6*/
ODS graphics on;
title "MODEL TWO K=6";
proc fmm data=amt.headache;
class migraine sex group;
model frequency = age migraine group sex /dist=poisson k=6;
run;
TITLE;

/*MODEL TWO*/

title "MODEL TWO K=5";
title2 "Covariates vary across components";
proc fmm data=amt.headache  order=freq componentinfo fitdetails ;
class migraine sex group;
model frequency = age migraine group sex /dist=poisson k=5 ;
ID frequency age migraine group sex  ;
output out=amt.out_pred class pred(components);
run;
TITLE;


/*MODEL TWO B*/

title "MODEL TWO K=5";
title2 "Age Fixed";
proc fmm data=amt.headache  order=freq componentinfo fitdetails ;
class migraine sex group;
model frequency = age migraine group sex /dist=poisson k=5 equate=effects(age);
ID frequency;
output out=amt.out_pred class pred(components);
run;
TITLE;

/*MODEL THREE*/

title "MODEL TWO K=5";
title2 "Equal Effect in the components";
proc fmm data=amt.headache order=freq componentinfo fitdetails ;
class migraine sex group;
model frequency = age migraine group sex /dist=poisson k=5 equate=effects(age migraine group sex);
output out=amt.out_pred class pred(component) maxprob;
run;
TITLE;


/*MODEL THREE*/
data amt.headache;
set amt.headache;
agec=age-42;
run;


Title "MODEL Four";
title2 "Mixture Weights Depends on Covariates";
proc fmm data=amt.headache order=freq componentinfo noitprint;
class migraine sex group;
model frequency = age migraine group sex /dist=poisson k=5;
PROBMODEL age migraine group sex;
run;


*Importing predicted dataset from R to make plots;

proc import datafile="E:\SEED\OneDrive\Msc. Biostatistics\Level Two\Advanced Modelling Techniques\Project\predicted.csv"
	dbms=csv out=amt.predicted replace;
run;

data amt.predicted;
set amt.predicted;
if (age>=18 and age<28) then agroup="18-27";
else if (age>=28 and age<37) then agroup="28-36"; 
else if (age>=37 and age<46) then agroup="37-46";
else if (age>=46 and age<56) then agroup="46-55";
else if (age>=56) then agroup=">56";
run;

/* Predicted curve by sex*/
GOPTIONS RESET=ALL;
title "Plot of Observed vs Predicted number of headaches";
title2 "By Sex For The Five Components";
PROC sGPLOT DATA=amt.predicted;
SYMBOL INTERPOL=join;
vbox frequency /category=class group=sex lineattrs=(pattern=solid) whiskerattrs=(pattern=solid) ;
vbox fitted /category=class group=sex lineattrs=(pattern=solid) whiskerattrs=(pattern=solid) ;
keylegend / location=inside position=topright across=1;
format sex f_gender.;
xaxis label="Components";
yaxis label="Headaches";
RUN;

/* Predicted curve by group*/
GOPTIONS RESET=ALL;
title "Plot of Observed vs Predicted number of headaches";
title2 "By Group For The Five Components";
PROC sGPLOT DATA=amt.predicted;
SYMBOL INTERPOL=join;
vbox frequency /category=class group=group lineattrs=(pattern=solid) whiskerattrs=(pattern=solid) ;
vbox fitted /category=class group=group lineattrs=(pattern=solid) whiskerattrs=(pattern=solid) ;
keylegend / location=inside position=topright across=1;
format group f_group.;
xaxis label="Components";
yaxis label="Headaches";

RUN;

/* Predicted curve by migraine*/
GOPTIONS RESET=ALL;
title "Plot of Observed vs Predicted number of headaches";
title2 "By Migraine For The Five Components";
PROC sGPLOT DATA=amt.predicted;
SYMBOL INTERPOL=join;
vbox frequency /category=class group=migraine lineattrs=(pattern=solid) whiskerattrs=(pattern=solid) ;
vbox fitted /category=class group=migraine lineattrs=(pattern=solid) whiskerattrs=(pattern=solid) ;
keylegend / location=inside position=topright across=1;
xaxis label="Components";
yaxis label="Headaches";
format migraine f_migraine.;
RUN;


/* Predicted curve by migraine*/
GOPTIONS RESET=ALL;
title "Plot of Observed vs Predicted number of headaches";
title2 "By Age group For The Five Components";
PROC sGPLOT DATA=amt.predicted;
SYMBOL INTERPOL=join;
vbox frequency /category=class group=agroup lineattrs=(pattern=solid) whiskerattrs=(pattern=solid) ;
vbox fitted /category=class group=agroup lineattrs=(pattern=solid) whiskerattrs=(pattern=solid) ;
keylegend / location=inside position=topright across=2;
xaxis label="Components";
yaxis label="Headaches";

RUN;


