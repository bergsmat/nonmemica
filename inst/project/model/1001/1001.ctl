$PROBLEM //like/1001//but/2 CMT//
$INPUT C ID TIME SEQ=DROP EVID AMT DV SUBJ HOUR HEIGHT WT SEX AGE DOSE FED
$DATA ../../data/derived/drug.csv IGNORE=C
$SUBROUTINE ADVAN4 TRANS4
$PK
 CL=THETA(1)*EXP(ETA(1)) * THETA(6)**SEX * (WT/70)**THETA(7)
 V2 =THETA(2)*EXP(ETA(2))
 KA=THETA(3)*EXP(ETA(3))
 Q  =THETA(4)
 V3=THETA(5)
 S2=V2
 
$ERROR
 Y=F*(1+ERR(1)) + ERR(2)
 IPRE=F

$THETA 
(0,10,50)     ;CL/F;clearance;L/h
(0,10,100)    ;Vc/F;central volume;L
(0,0.2, 5)    ;Ka;absorption rate constant;1/h
(0,10,50)     ;Q/F;intercompartmental clearance;L/h
(0,100,1000)  ;Vp/F;peripheral volume;L
(0,1,2)       ;MALE_CL;male effect on clearance;
(0,0.75,3)    ;WT_CL;weight effect on clearance;

$OMEGA BLOCK(3)
.1            ;IIV_CL;interindividual variability on clearance
.01           ;CL_V;interindividual clearance-volume covariance
.1            ;IIV_Vc;interindividual variability on central volume
.01           ;CL_Ka;interindividual clearance-Ka covariance
.01           ;Vc_Ka;interindividual volume-Ka covariance
.1            ;IIV_Ka;interindividual variability on Ka

$SIGMA 
0.1           ;ERR_PROP;proportional error
0.1           ;ERR_ADD;additive error

$ESTIMATION MAXEVAL=9999 PRINT=5 NOABORT METHOD=1 INTER MSFO=mod.msf
$COV PRINT=E
$TABLE NOPRINT FILE=mod.tab ONEHEADER 
ID            ;ID;NONMEM subject identifier;
AMT           ;AMT;dose amount;mg
TIME          ;TIME;time;h
EVID          ;EVID;event type;//0/observation//1/dose
PRED          ;PRED;population prediction;ng/mL
IPRE          ;IPRED;individual prediction;ng/mL
CWRESI        ;CWRESI;conditional weighted residual;
CIWRESI       ;CIWRESI;conditional indvividual weighted residual;

$TABLE NOPRINT FILE=mod2.tab ONEHEADER 
ID            ;ID;subject identifier
TIME          ;TIME;time;h
CL            ;CLI;posthoc systemic clearance;L/h
V2            ;V2I;posthoc systemic volume;L
KA            ;KAI;posthoc absorption rate;1/h
Q             ;Q2I;posthoc intercompartmental clearance;1/h
V3            ;V3I;posthoc peripheral volume;L
ETA1          ;BSV_CL;clearance between-subject variability;
ETA2          ;BSV_V2;volume between-subject variability;
ETA3          ;BSV_KA;absorption between-subject variability;
