* set periods as groups: exp and unexp;


data cases2;
set cases2;
IF (year_grp=-1) or (year_grp=0) THEN expldct =0;
if (year_grp=1) or (year_grp=2) THEN expldct = 1;
run;
 
* check distribution 
proc freq data=cases2;
tables year_grp expldct;
run;

*set pre and post LDCT time in both groups;

data cases2;
set cases2;
IF (year_grp=2) or (year_grp=0) then timeLDCT =1;
if (year_grp=-1) or (year_grp=1) THEN timeLDCT = 0;
run;

proc freq data=cases2;
tables year_grp timeldct;
run;

*set interaction term;

DATA cases2;
SET cases2;
    intterm =  (expldct*timeldct);
    
RUN;

proc freq data=cases2;
tables stage intterm timeldct*expldct;
run;

*create 1/0 for stage4;

data cases2;
set cases2;
IF (stage=4) THEN mets=1;
if (stage=1) or (stage=2) THEN mets = 0;
run;

proc freq data=cases2;
tables mets stage;
run;

data cases2;
set cases2;
if mets=. then delete;
run;

*proc genmod with interaction term;

proc genmod;
        class timeldct expldct;
        model mets (event='1') = timeldct expldct timeldct*expldct;
        estimate "Diff in Diff" timeldct*expldct 1 -1 -1 1;
        lsmeans timeldct*expldct/Cl;
        lsmestimate timeldct*expldct "Diff in Diff" 1 -1 -1 1;
        run;


proc genmod;
class timeldct expldct;
model mets (event='1')=timeldct expldct timeldct*expldct/dist=binomial link=identity;
lsmeans timeldct*expldct;
run;

proc logistic data=cases2;
class ldct exposed;
model advanced (event='1') =ldct exposed ldct*exposed;
lsmeans ldct*exposed;
run;

proc logistic data = cases2;

model advanced (event = '1') = ldct exposed ldct*exposed;

run;


*from forum post;

proc logistic data=cases2; 
 class ldct exposed/ param=ref; 
 model advanced(event="1")=ldct|exposed; 
 output out=out p=p xbeta=xb;
 store log;
 run; 

%NLEstimate(instore=log, label=diff in diff probs, 
  f=(logistic(b_p1+b_p2+b_p3+b_p4)-logistic(b_p1+b_p2)) - (logistic(b_p1+b_p3)-logistic(b_p1)), df=100)
%NLEstimate(instore=log, label=diff in diff log odds, 
  f=((b_p1+b_p2+b_p3+b_p4)-(b_p1+b_p2)) - ((b_p1+b_p3)-(b_p1)), df=100)
  
  proc means data=out mean nway; 
 class a b; 
 var p xb; 
 output out=probs mean=; 
 run; 
proc transpose data=probs out=tprobs;
 var p xb;
 run; 
data tprobs; 
 set tprobs; 
 difdif=(col1-col3)-(col2-col4); 
 run; 
proc print noobs;
 var _label_ difdif;
 run;
 
proc nlmixed data=mydata; 
 p=logistic(b0 + b1*(a=1) + b2*(b=1) + b3*(a=1 and b=1)); 
 model y ~ binary(p); 
 estimate "(a1b1-a1b2)-(a2b1-a2b2)" ((b0+b1+b2+b3)-(b0+b1)) - ((b0+b2)-(b0)); 
 estimate "(Pa1b1-Pa1b2)-(Pa2b1-Pa2b2)" (logistic(b0+b1+b2+b3)-logistic(b0+b1)) - (logistic(b0+b2)-logistic(b0)); 
 run; 
 
 
 * final did proc genmod with binomial and link;


        
proc genmod;
        class expldct timeldct/ref=first;
        model distant_mets (event='1') =timeldct expldct timeldct*expldct/dist=binomial link=identity;
        estimate "Diff in Diff" timeldct*expldct 1 -1 -1 1;
        lsmeans timeldct*expldct/Cl;
        lsmestimate timeldct*expldct "Diff in Diff" 1 -1 -1 1;
        run;


  
  



