column,label,type,guide,required,comment
C,comment flag,character,//NA/not ignored//C/ignored//,0,.
ID,integer subject identifier,integer,.,1,.
TIME,time,numeric,h,1,.
SEQ,sequence identifier for sorting,integer,.,1,.
EVID,event type identifier,integer,//0/PK observation//1/dose//,1,.
AMT,dose amount of imaginary drug,numeric,mg,EVID == 1,.
DV,plasma drug concentration,numeric,ng/mL,EVID == 0,.
SUBJ,universal subject identifier,character,.,1,.
HOUR,time as recorded in source data,numeric,h,1,.
HEIGHT,height,numeric,cm,1,.
WEIGHT,weight,numeric,kg,1,.
SEX,sex,integer,//0/female//1/male//,1,.
AGE,age,numeric,y,1,.
DOSE,regimen dose,numeric,mg,1,.
FED,prandial state,integer,//0/fasted//1/fed//,1,.
SMK,smoker status,integer,//0/nonsmoker//1/smoker//,1,.
DS,disease status,integer,//0/healthy//1/patient//,1,.
CRCN,creatinine clearance,numeric,mL/min,1,.
TAFD,time after first dose,numeric,h,1,.
TAD,time since most recent dose,numeric,h,0,.
LDOS,most recent dose amount,numeric,mg,0,.
MDV,missing dependent value flag,integer,//0/dependent value not missing//1/dependent value missing//,1,.
predose,predose flag,integer,//0/record not predose//1/record is predose//,1,.
zerodv,zero DV flag,integer,//0/dependent value not zero//1/dependent value is zero//,1,.
