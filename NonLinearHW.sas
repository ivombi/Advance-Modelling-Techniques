/* Setting up libraray */
libname nl "E:\SEED\OneDrive\Msc. Biostatistics\Level Two\Advanced Modelling Techniques\Project";

proc format;
 value period 
 	1="control"
 	2="Active";
 run;
 
 data nl.songbird_amt19;
 	set nl.songbird_amt19;
 format period period.;
 where time >0;
if treatment=1 then treat1=1;else treat1=0; label treat1="Treatment 2 vs 1";
if treatment=3 then treat2=1;else treat2=0; label treat2="Treatment 3 vs 2";
run;
 
*Exploring the data set;
proc contents data=nl.songbird_amt19;
run;

options obs=10;
proc print data=nl.songbird_amt19;
run;
options obs=max;

proc means data=nl.songbird_amt19 maxdec=3;
var  SI_area_X SI_HVC;
class period treatment;
run;

proc means data=nl.songbird_amt19;
class period;
var  SI_area_X;
run;

proc freq data=nl.songbird_amt19;
tables bird*period;
run;

proc sgplot data = nl.songbird_amt19;
title "Sound Intensity In Area X Period One";
styleattrs datalinepatterns=(solid);
series x = time y =  SI_area_X/group=bird;
where period=1;
run;

proc sgplot data = nl.songbird_amt19;
title "Sound Intensity In Area X Period Two";
styleattrs datalinepatterns=(solid);
series x = time y =  SI_area_X/group=bird;
*where period=2;
run;

proc sgplot data = nl.songbird_amt19;
title "Sound Intensity In Area X";
styleattrs datalinepatterns=(solid);
series x = time y =  SI_area_X/group=bird;
run;


/* Homoscedastic Model */

title1 "Homoscedastic Model";
title2 "Individual Profiles";
proc nlmixed data=nl.songbird_amt19 noad qpoints=3;
parms beta1=1.27 beta2=1.04 beta3=1.23 beta4=1.51 sigma=0.1 ;
avg = (beta1)*exp(-exp(beta2+beta3*treat1 + beta4*treat2)*time);
model  SI_area_X ~normal(avg,sigma**2);
by bird;
run;


/* Heteroscedastic Model */
/* Power model */
/* Testing if Heteroscedastic model is a better fit */
title "Individual Profiles";
proc nlmixed data=nl.songbird_amt19 noad qpoints=5;
parms beta1=1.27 beta2=1.04 beta3=1.23 beta4=1.51 sigma=0.1 theta=1;
avg = (beta1)*exp(-exp(beta2+beta3*treat1 + beta4*treat2)*time);
model  SI_area_X ~normal(avg,(avg**(2*theta))*(sigma**2));
by bird;
run;

/* HIERICHICAL MODELS */

title1 "Homoscedastic Model";
title2 "Hierachical";
proc nlmixed data=nl.songbird_amt19 noad qpoints=3;
parms beta1=1.27 beta2=1.04 beta3=1.3 beta4=1.51 sigma=0.1  d11=0.052 d22=0.246;
avg = (beta1+b1)*exp((beta2 + beta3*treat1 + beta4*treat2 +b2)*time);
model  SI_area_X ~normal(avg,sigma**2);
random b1 b2 ~ normal([0,0],[d11,0,d22])
subject = bird;
predict avg out=nl.nlpred;
run;

proc sgplot data = nl.nlpred;
title "Sound Intensity In Area X";
scatter x = time y =  SI_area_X /markerattrs=(symbol=asterisk color=green);
series x = time y =  pred/group=bird lineattrs=(pattern=2);
where bird=5;
run;

title1 "Homoscedastic Model";
title2 "Hierachical";
proc nlmixed data=nl.songbird_amt19 noad qpoints=5;
parms beta1=1.27 beta2=1.04 beta3=1.3  sigma=0.1 d11=0.2 d22=0.1;
avg = (beta1+b1)*exp((beta2*treat1 + beta3*treat2 +b2)*time);
model  SI_area_X ~normal(avg,sigma**2);
random b1 b2~ normal([0,0],[d11,0,d22])
subject = bird;
predict avg out=nl.nlpred;
run;




proc nlmixed data=nl.songbird_amt19 qpoints=5;
	parms phim=0.14 phimdiff1=0.26 phimdiff2=0 eta=1.88 etadiff1=0.54 etadiff2=0 tau=3.68
	taudiff1=0.428 taudiff2=0 gamma=0.01 gdiff1=0.15 gdiff2=0 d11=0.01 sigma2=0.05 d22=0.0002;
	teller = (phim + phimdiff1 * treat1 + phimdiff2 * treat2+vm) *
	(time ** (eta + etadiff1 * treat1 + etadiff2*treat2));
	noemer = ((tau +t+ taudiff1*treat1+ taudiff2*treat2)
	** (eta + etadiff1 *treat1+ etadiff2 *treat2) )
	+ (time ** (eta + etadiff1 *treat1+ etadiff2 *treat2));
	gemid = teller/noemer + gamma + gdiff1 * treat1 + gdiff2 * treat2;
	model  SI_area_X ~ normal(gemid,sigma2);
	random vm t ~ normal([0,0],[d11,0,d22])
	subject=bird;
	predict gemid  out=nl.hvc;
run;

proc print data= nl.hvc;
var si_area_x pred;
run;

proc sgplot data = nl.hvc;
title "Sound Intensity In Area X";
scatter x = time y =  SI_area_X /markerattrs=(symbol=asterisk color=green);
series x = time y =  pred/group=bird lineattrs=(pattern=2);
where bird=6 and period=2;
run;

ods pdf file="E:\SEED\OneDrive\Msc. Biostatistics\Level Two\Advanced Modelling Techniques\Project\output4.pdf";
title "Full model with all paramters";
proc nlmixed data=nl.songbird_amt19 qpoints=3;
	parms phim=0.14 phimdiff1=0.26 phimdiff2=0.1 eta=1.4 etadiff1=0.54 etadiff2=0 tau=3.68
	taudiff1=0.4298 taudiff2=0 beta1=0.18 gamma=-0.01 gdiff1=0.01045 gdiff2=0.2
	d11=0.01 sigma2=0.01 d22=0.01 d33=0.01;
	teller = (phim + phimdiff1 * treat1 + phimdiff2 * treat2+vm) *
	(time ** (eta + etadiff1 * treat1 + etadiff2*treat2+n));
	noemer = ((tau + taudiff1*treat1+ taudiff2*treat2 +t )
	** (eta + etadiff1 *treat1+ etadiff2 *treat2 +n) )
	+ (time ** (eta + etadiff1 *treat1+ etadiff2 *treat2 + n));
	gam = gamma + gdiff1 * treat1 + gdiff2 * treat2+ beta1*vol_area_x;
	gemid = (teller/noemer) + gam;
	model  SI_area_X ~ normal(gemid,sigma2);
	random vm t n  ~ normal([0,0,0],[d11,0,d22,0,0,d33])
	subject=bird;
	*predict gemid  out=nl.hvc;
run;
title;
ods pdf close;

ods pdf file="E:\SEED\OneDrive\Msc. Biostatistics\Level Two\Advanced Modelling Techniques\Project\output5.pdf";
title "Vm random effect removed";
proc nlmixed data=nl.songbird_amt19 qpoints=3;
	parms phim=0.14 phimdiff1=0.26 phimdiff2=0.1 eta=1.4 etadiff1=0.54 etadiff2=0 tau=3.68
	taudiff1=0.4298 taudiff2=0 beta1=0.18 gamma=-0.01 gdiff1=0.01045 gdiff2=0.2
	 sigma2=0.01 d22=0.01 d33=0.01;
	teller = (phim + phimdiff1 * treat1 + phimdiff2 * treat2) *
	(time ** (eta + etadiff1 * treat1 + etadiff2*treat2+n));
	noemer = ((tau + taudiff1*treat1+ taudiff2*treat2 +t )
	** (eta + etadiff1 *treat1+ etadiff2 *treat2 +n) )
	+ (time ** (eta + etadiff1 *treat1+ etadiff2 *treat2 + n));
	gam = gamma + gdiff1 * treat1 + gdiff2 * treat2+ beta1*vol_area_x;
	gemid = (teller/noemer) + gam;
	model  SI_area_X ~ normal(gemid,sigma2);
	random t n  ~ normal([0,0],[d22,0,d33])
	subject=bird;
	*predict gemid  out=nl.hvc;
run;
title;
ods pdf close;


ods pdf file="E:\SEED\OneDrive\Msc. Biostatistics\Level Two\Advanced Modelling Techniques\Project\output9.pdf";
title Removing phidiff2;
proc nlmixed data=nl.songbird_amt19 qpoints=3;
	parms phim=0.14 phimdiff1=0.26  eta=1.4 etadiff1=0.54 etadiff2=0 tau=3.68
	taudiff1=0.4298 taudiff2=0 beta1=0.18 gamma=-0.01 gdiff1=0.01045 gdiff2=0.2
	d11=0.01 sigma2=0.01 d22=0.01 d33=0.01;
	teller = (phim + phimdiff1 * treat1+vm) *
	(time ** (eta + etadiff1 * treat1 + etadiff2*treat2+n));
	noemer = ((tau + taudiff1*treat1+ taudiff2*treat2 +t )
	** (eta + etadiff1 *treat1+ etadiff2 *treat2 +n) )
	+ (time ** (eta + etadiff1 *treat1+ etadiff2 *treat2 + n));
	gam = gamma + gdiff1 * treat1 + gdiff2 * treat2+ beta1*vol_area_x;
	gemid = (teller/noemer) + gam;
	model  SI_area_X ~ normal(gemid,sigma2);
	random vm t n  ~ normal([0,0,0],[d11,0,d22,0,0,d33])
	subject=bird;
	*predict gemid  out=nl.hvc;
run;
title;
ods pdf close;
/* HIERICHICAL MODELS */

title1 "Homoscedastic Model";
title2 "Hierachical";
proc nlmixed data=nl.songbird_amt19 noad qpoints=3;
parms beta1=1.27 beta2=1.04 beta4=1.23 sigma=0.1  d11=0.002 d22=0.246;
avg = (beta1+b1)*exp(-exp(beta2 + beta4*treat2 +b2)*time);
model  SI_area_X ~normal(avg,sigma**2);
random b1 b2 ~ normal([0,0],[d11,0,d22])
subject = bird;
predict avg out=nl.nlpred;
run;



beta1(beta3)=period 1(2) marginal song intensity in SI area X while b1 indicates how each bird in period 1(2) deviates from the marginal
beta2(beta4)=Marginal rate  of sound intensity period 1(2) while b2(b4) indicates the rate at which bird deviate from the marginal rate.

/* HETEROSCEDASTIC MODEL */
/* Constant coefficient of variation */

title1 "Heteroscedastic Model";
title2 "Hierachical";
proc nlmixed data=nl.songbird_amt19 noad qpoints=3;
parms beta1=1.27 beta2=1.04 beta3=1.23 beta4=1.51 sigma=0.1  d11=0.052 d22=0.246;
avg = (beta1+b1)*exp(-exp(beta2 + beta3*treat1 + beta4*treat2 +b2)*time);
model  SI_area_X ~normal(avg,(avg**2)*sigma**2);
random b1 b2 ~ normal([0,0],[d11,0,d22])
subject = bird;
predict avg out=nl.nlpred;
run;

beta1(beta3)=period 1(2) marginal song intensity in SI area X while b1 indicates how each bird in period 1(2) deviates from the marginal
beta2(beta4)=Marginal rate  of sound intensity period 1(2) while b2(b4) indicates the rate at which bird deviate from the marginal rate.



/* HETEROSCEDASTIC MODEL */
/* Constant coefficient of variation Including area-X as covariate */

title1 "Heteroscedastic Model";
title2 "Hierachical";
proc nlmixed data=nl.songbird_amt19 noad qpoints=3;
parms beta1=-2.7505 beta2=-23.6505 beta3=-4.5855 beta4=3.9273 sigma=0.9 d11=0.052 d22=0.246 d33=0.509 d44=0.937;
avg =  vol_area_X*exp(beta1+b1)*exp(-exp(beta2 +b2)*time)+ vol_area_X*exp(beta3 + b3)*exp(-exp(beta4 +b4)*time);
model  SI_area_X ~normal(avg,(avg**2)*sigma**2);
random b1 b2 b3 b4 ~ normal([0,0,0,0],[d11,0,d22,0,0,d33,0,0,0,d44])
subject = bird;
run;



/* SI HVC in the second period only */
data nl.songbird2;
set nl.songbird_amt19;
if  period=1  then delete;
run;

proc sgplot data = nl.songbird2;
styleattrs datalinepatterns=(solid);
title "Sound Intensity In HVC";
series x = time y =  SI_HVC/group=bird;
run;

proc nlmixed data=nl.songbird2 noad qpoints=3;
parms beta1=2.7505 beta2=1.5 beta3=-1.5855 beta4=-2.9273 sigma=0.9 
eta1=2.5 eta2=-1.3 eta3=2.3 eta4=-3.2;
avg = exp(beta1+ eta1*group )*exp(-exp(-beta2 + eta2*group)*time) - exp(beta3 + eta3*group )*exp(-exp(-beta4 +eta4*group )*time);
model  SI_HVC~normal(avg,sigma**2);
*random b1 b2 b3 b4 ~ normal([0,0,0,0],[d11,0,d22,0,0,d33,0,0,0,d44]);
by bird;
*predict avg out=nl.hvc;
run;

*General Equation;
*Homoscedastic model;
proc nlmixed data=nl.songbird2 noad qpoints=3;
parms beta1=2.7505 beta2=-1.5 beta3=2.5855 beta4=3.9273 sigma=0.9 d11=0.052 d22=0.246 d33=0.509 d44=0.937
eta1=2.5 eta2=-1.3 eta3=2.3 eta4=-3.2;
avg = exp(beta1+ eta1*group + b1)*exp(-exp(-beta2 + eta2*group +b2)*time) - exp(beta3 + eta3*group + b3)*exp(-exp(-beta4 +eta4*group+b4)*time);
model  SI_HVC~normal(avg,sigma**2);
random b1 b2 b3 b4 ~ normal([0,0,0,0],[d11,0,d22,0,0,d33,0,0,0,d44])
subject = bird;
predict avg out=nl.hvc;
run;


proc sgplot data = nl.hvc;
title "Sound Intensity In Area X";
scatter x = time y =  SI_HVC /markerattrs=(symbol=asterisk color=green);
series x = time y =  pred/group=bird lineattrs=(pattern=2);
*where bird=9;
run;

*eta1 and eta3 removed;
proc nlmixed data=nl.songbird2 noad qpoints=3;
parms beta1=2.7505 beta2=-1.5 beta3=2.5855 beta4=3.9273 sigma=0.9 d11=0.052 d22=0.246 d33=0.509 d44=0.937
 eta2=-3.1068 eta4=-3.2;
avg = exp(beta1+ b1)*exp(-exp(-beta2 + eta2*group+b2)*time) - exp(beta3 + b3)*exp(-exp(-beta4 +eta4*group +b4)*time);
model  SI_HVC~normal(avg,sigma**2);
random b1 b2 b3 b4 ~ normal([0,0,0,0],[d11,0,d22,0,0,d33,0,0,0,d44])
subject = bird;
predict avg out=nl.hvc1;
run;

*No fixed effects;
proc nlmixed data=nl.songbird2 noad qpoints=3;
parms beta1=2.7505 beta2=-1.5 beta3=2.5855 beta4=3.9273 sigma=0.9 d11=0.052 d22=0.246 d33=0.509 d44=0.937;
avg = exp(beta1+ b1)*exp(-exp(-beta2 +b2)*time) - exp(beta3 +  b3)*exp(-exp(-beta4+b4)*time);
model  SI_HVC~normal(avg,sigma**2);
random b1 b2 b3 b4 ~ normal([0,0,0,0],[d11,0,d22,0,0,d33,0,0,0,d44])
subject = bird;
predict avg out=nl.hvc2;
run;

proc sgplot data = nl.hvc;
*styleattrs datalinepatterns=(dash solid);
title "Sound Intensity In HVC";
series x = time y =  SI_HVC/group=bird lineattrs=(pattern=1);
series x = time y =  pred/group=bird lineattrs=(pattern=2);
*where bird=4;
run;

