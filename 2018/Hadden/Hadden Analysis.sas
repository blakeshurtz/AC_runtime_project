proc anova data=data;
class Date HDD_Class;
model Daily_Run = Date HDD_Class; 
*output out = resids r=res; 
	means Date /lsd;
run; quit;

proc univariate normal plot;
/* Tells SAS to run tests of normality and give a QQ-plot */
var res;
run;

data data; set data;
logdaily=log(daily_run);
run;

proc glm data=data;
class Date HDD_Class;
model logdaily = Date HDD_Class/solution; 
output out = resids r=res; 
run; quit;

proc univariate normal plot;
/* Tells SAS to run tests of normality and give a QQ-plot */
var res;
run;

