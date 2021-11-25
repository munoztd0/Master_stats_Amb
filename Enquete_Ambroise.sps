* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.

*Condition

IF  (FL_24_DO_AffectivePriming = 1) Priming=1.
EXECUTE.

IF  (FL_24_DO_ComputationalPriming = 1) Priming=0.
EXECUTE.

IF  (FL_27_DO_EvaluationA = 1) Product=0.
EXECUTE.

IF  (FL_27_DO_EvaluationA__ = 1) Product=1.
EXECUTE.

*Time 

COMPUTE TimeComPRIMING=Q4.4_Page_Submit + Q4.6_Page_Submit +Q4.8_Page_Submit  + Q4.10_Page_Submit + 
    Q4.12_Page_Submit.
EXECUTE.

COMPUTE TimeAffPRIMING=Q5.4_Page_Submit + Q5.6_Page_Submit + Q5.8_Page_Submit + Q5.10_Page_Submit + 
    Q5.12_Page_Submit + Q5.14_Page_Submit + Q5.16_Page_Submit.
EXECUTE.

IF  (Priming= 0) TimePRIMING=TimeComPRIMING.
EXECUTE.

IF  (Priming= 1) TimePRIMING=TimeAffPRIMING.
EXECUTE.


*Prix

IF  (FL_27_DO_EvaluationA__ = 1) Prix=Q8.2_1.
EXECUTE.

IF  (FL_27_DO_EvaluationA= 1) Prix=Q7.2_1.
EXECUTE.

*ProbabilitéACHAT

IF  (FL_27_DO_EvaluationA__ = 1) ProbabilitéACHAT=Q8.3.
EXECUTE.

IF  (FL_27_DO_EvaluationA= 1) ProbabilitéACHAT=Q7.3.
EXECUTE.

*Reccomend

IF  (FL_27_DO_EvaluationA__ = 1) Reccomend=Q8.4.
EXECUTE.

IF  (FL_27_DO_EvaluationA= 1) Reccomend=Q7.4.
EXECUTE.

*EcologiqueEVAL

IF  (FL_27_DO_EvaluationA__ = 1) EcologiqueEVAL=Q8.5.
EXECUTE.

IF  (FL_27_DO_EvaluationA= 1) EcologiqueEVAL=Q7.5.
EXECUTE.

*Comportement pro-environnemental

COMPUTE ComportementEnvironmental=mean(Q13.1,Q13.2,Q13.3,Q13.4,Q13.5,Q13.6,Q13.7,Q13.8,Q13.9,Q13.10,
    Q13.11,Q13.12,Q13.13,Q13.14).
EXECUTE.




