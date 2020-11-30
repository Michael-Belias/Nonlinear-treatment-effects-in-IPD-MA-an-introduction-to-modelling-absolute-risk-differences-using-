options nodate nonumber nocenter linesize=90 pagesize=60;

options NOFMTERR;

libname ana_oma2 'F:\Data IPD AOM\Damoiseaux\';


data Damois;
set ana_oma2.ana_oma2;
run;



Data Damois;
set Damois;

*recoding subgroups;
if trialnr^=. then study=1;
if sexe=1 then gender=1;
else if sexe=2 then gender=0;
else gender=.;
if  treatm=1 then treat=1;
else if treatm=2 then treat=0;
else treat=.;
if at111=1 then rAOM=1;
else if at111=2 then rAOM=0;
else rAOM=.;
if at112=1 then breast=1;
else if at112=2 then breast=0;
else breast=.;
if AT116=0 then sibling=0;
else if 0<at116<15 then sibling=1;
else sibling=.;
if at118=1 then famhist=1;
else if at118=2 then famhist=0;
else famhist=.;
if at103=1 then vom_0=1;
else if at103=2 then vom_0=0;
else vom_0=.;
if at119=1 then smoke=1;
else if at119=2 then smoke=0;
else smoke=.;
if at106=1 then fever_0=1;
else if at106=2 then fever_0=0;
else fever_0=.;
if at104=1 then cry_0=1;
else if at104=2 then cry_0=0;
else cry_0=.;
if at116=0 then sibling=0;
else if 0<at116<15 then sibling=1;
else sibling=.;
if oma_x2=1 then bilat_0=1;
else if oma_x2=0 then bilat_0=0;
else bilat_0=.;
if at101b=1 then painr_0=1;
else if at101b=2 then painr_0=0;
else painr_0=.;
if at101a=1 then painl_0=1;
else if at101a=2 then painl_0=0;
else painl_0=.;
if sum(painl_0,painr_0)=. then pain_0=.;
else if (painl_0=1 or painr_0=1) then pain_0=1;
else pain_0=0;
if at107=1 then rnose_0=1;
else if at107=2 then rnose_0=0;
else rnose_0=.;
if at108=1 then cough_0=1;
else if at108=2 then cough_0=0;
else cough_0=.;
if ot121ar=1 then tmnorr_0=0;
else if ot121ar=2 then tmnorr_0=1;
else tmnorr_0=.;
if ot121al=1 then tmnorl_0=0;
else if ot121al=2 then tmnorl_0=1;
else tmnorl_0=.;
if sum(tmnorr_0,tmnorl_0)=. then tmnor_0=.;
else if (tmnorr_0=1 or tmnorl_0=1) then tmnor_0=1;
else tmnor_0=0;
if ot121br=8 then ot121br=.;
else if ot121br=9 then ot121br=.;
if ot121cr=8 then ot121cr=.;
else if ot121cr=9 then ot121cr=.;
if sum(ot121br,ot121cr)=. then tmcolr_0=.;
else if ot121br=1 then tmcolr_0=1;
else if ot121cr=1 then tmcolr_0=1;
else tmcolr_0=0;
if ot121bl=8 then ot121bl=.;
else if ot121bl=9 then ot121bl=.;
if ot121cl=8 then ot121cl=.;
else if ot121cl=9 then ot121cl=.;
if sum(ot121bl,ot121cl)=. then tmcoll_0=.;
else if ot121bl=1 then tmcoll_0=1;
else if ot121cl=1 then tmcoll_0=1;
else tmcoll_0=0;
if sum(tmcoll_0,tmcolr_0)=. then tmcol_0=.;
else if (tmcoll_0=1 or tmcolr_0=1) then tmcol_0=1;
else tmcol_0=0;
if ot121er=2 then tmbulr_0=0;
else if ot121er=1 then tmbulr_0=1;
else tmbulr_0=.;
if ot121el=2 then tmbull_0=0;
else if ot121el=1 then tmbull_0=1;
else tmbull_0=.;
if sum(tmbull_0,tmbulr_0)=. then tmbul_0=.;
else if (tmbull_0=1 or tmbulr_0=1) then tmbul_0=1;
else tmbul_0=0;
if ot121fl=1 then perfl_0=1;
else if ot121fl=2 then perfl_0=0;
else perfl_0=.;
if ot121fr=1 then perfr_0=1;
else if ot121fr=2 then perfr_0=0;
else perfr_0=.;
if (perfl_0=1 or perfr_0=1) then perf_0=1;
else if sum( perfl_0,perfr_0)=. then perf_0=.;
else perf_0=0;
if ot122l=1 then otol_0=1;
else if ot122l=2 then otol_0=0;
else otol_0=.;
if ot122r=1 then otor_0=1;
else if ot122r=2 then otor_0=0;
else otor_0=.;
if (otol_0=1 or otor_0=1) then oto_0=1;
else if sum( otol_0,otor_0)=. then oto_0=.;
else oto_0=0;





* recoding age;
age=(leeftijd/12);

*recoding outcomes per day;
if d011f=1 then fevd1=1;
else if d011f=2 then fevd1=0;
else fevd1=.;
if (d011ar=1 and d011al=1) then paind1=1;
else if (d011ar=1 and d011al=2) then paind1=1;
else if (d011ar=2 and d011al=1) then paind1=1;
else if (d011ar=2 and d011al=2) then paind1=0;
else paind1=.;
if sum(fevd1,paind1)=. then po_d1=.;
else if (fevd1=1 or paind1=1) then po_d1=1;
else po_d1=0;

if d021f=1 then fevd2=1;
else if d021f=2 then fevd2=0;
else fevd2=.;
if (d021ar=1 and d021al=1) then paind2=1;
else if (d021ar=1 and d021al=2) then paind2=1;
else if (d021ar=2 and d021al=1) then paind2=1;
else if (d021ar=2 and d021al=2) then paind2=0;
else paind2=.;
if sum(fevd2,paind2)=. then po_d2=.;
else if (fevd2=1 or paind2=1) then po_d2=1;
else po_d2=0;

if d031f=1 then fevd3=1;
else if d031f=2 then fevd3=0;
else fevd3=.;
if (d031ar=1 and d031al=1) then paind3=1;
else if (d031ar=1 and d031al=2) then paind3=1;
else if (d031ar=2 and d031al=1) then paind3=1;
else if (d031ar=2 and d031al=2) then paind3=0;
else paind3=.;
if sum(fevd3,paind3)=. then po_d3=.;
else if (fevd3=1 or paind3=1) then po_d3=1;
else po_d3=0;

if d041f=1 then fevd4=1;
else if d041f=2 then fevd4=0;
else fevd4=.;
if (d041ar=1 and d041al=1) then paind4=1;
else if (d041ar=1 and d041al=2) then paind4=1;
else if (d041ar=2 and d041al=1) then paind4=1;
else if (d041ar=2 and d041al=2) then paind4=0;
else paind4=.;
if sum(fevd4,paind4)=. then po_d4=.;
else if (fevd4=1 or paind4=1) then po_d4=1;
else po_d4=0;

if d051f=1 then fevd5=1;
else if d051f=2 then fevd5=0;
else fevd5=.;
if (d051ar=1 and d051al=1) then paind5=1;
else if (d051ar=1 and d051al=2) then paind5=1;
else if (d051ar=2 and d051al=1) then paind5=1;
else if (d051ar=2 and d051al=2) then paind5=0;
else paind5=.;
if sum(fevd5,paind5)=. then po_d5=.;
else if (fevd5=1 or paind5=1) then po_d5=1;
else po_d5=0;

if d061f=1 then fevd6=1;
else if d061f=2 then fevd6=0;
else fevd6=.;
if (d061ar=1 and d061al=1) then paind6=1;
else if (d061ar=1 and d061al=2) then paind6=1;
else if (d061ar=2 and d061al=1) then paind6=1;
else if (d061ar=2 and d061al=2) then paind6=0;
else paind6=.;
if sum(fevd6,paind6)=. then po_d6=.;
else if (fevd6=1 or paind6=1) then po_d6=1;
else po_d6=0;

if d071f=1 then fevd7=1;
else if d071f=2 then fevd7=0;
else fevd7=.;
if (d071ar=1 and d071al=1) then paind7=1;
else if (d071ar=1 and d071al=2) then paind7=1;
else if (d071ar=2 and d071al=1) then paind7=1;
else if (d071ar=2 and d071al=2) then paind7=0;
else paind7=.;
if sum(fevd7,paind7)=. then po_d7=.;
else if (fevd7=1 or paind7=1) then po_d7=1;
else po_d7=0;

if d081f=1 then fevd8=1;
else if d081f=2 then fevd8=0;
else fevd8=.;
if (d081ar=1 and d081al=1) then paind8=1;
else if (d081ar=1 and d081al=2) then paind8=1;
else if (d081ar=2 and d081al=1) then paind8=1;
else if (d081ar=2 and d081al=2) then paind8=0;
else paind8=.;
if sum(fevd8,paind8)=. then po_d8=.;
else if (fevd8=1 or paind8=1) then po_d8=1;
else po_d8=0;

if d091f=1 then fevd9=1;
else if d091f=2 then fevd9=0;
else fevd9=.;
if (d091ar=1 and d091al=1) then paind9=1;
else if (d091ar=1 and d091al=2) then paind9=1;
else if (d091ar=2 and d091al=1) then paind9=1;
else if (d091ar=2 and d091al=2) then paind9=0;
else paind9=.;
if sum(fevd9,paind9)=. then po_d9=.;
else if (fevd9=1 or paind9=1) then po_d9=1;
else po_d9=0;

if d101f=1 then fevd10=1;
else if d101f=2 then fevd10=0;
else fevd10=.;
if (d101ar=1 and d101al=1) then paind10=1;
else if (d101ar=1 and d101al=2) then paind10=1;
else if (d101ar=2 and d101al=1) then paind10=1;
else if (d101ar=2 and d101al=2) then paind10=0;
else paind10=.;
if sum(fevd10,paind10)=. then po_d10=.;
else if (fevd10=1 or paind10=1) then po_d10=1;
else po_d10=0;


*recoding poor outcome;
if at207=1 then fever_2=1;
else if at207=2 then fever_2=0;
else fever_2=.;
if (at202a=1 and at202b=1) then pain_2=1;
else if (at202a=1 and at202b=2) then pain_2=1;
else if (at202a=2 and at202b=1) then pain_2=1;
else if (at202a=2 and at202b=2) then pain_2=0;
else pain_2=.;
if sum(fever_2,pain_2)=. then poutcome=.;
else if (fever_2=1 or pain_2=1) then poutcome=1;
else poutcome=0;


*recoding outcomes after 1 month;
if (o17a=3 or o17a=4) then effr_am=1;
else if (o17a=1 or o17a=2) then effr_am=0;
else effr_am=.;
if (o17b=3 or o17b=4) then effl_am=1;
else if (o17a=1 or o17a=2) then effl_am=0;
else effl_am=.;
if sum(effl_am,effr_am)=. then eff_am=.;
else if (effl_am=1 or effr_am=1) then eff_am=1;
else eff_am=0;

run;


Data Damois;
set Damois;
rename at111a=AOMepi;
rename at116=sibs;
run;

Data Damois;
set Damois;
if AOMepi=88 then AOMepi=.;
if sibs>15 then sibs=.;
run;



Data Damois;
set Damois;
label
study='study (1=Damois 2=Burke 3=Appelman 4=Little)'
gender='gender (1=male 0=female)'
treat='treament (1=AB 0=placebo)'
season='season (1=autumn+winter 0=spring+summer)'
RAOM='recurrent otitis media (1=yes 0=no)'
AOMepi='no. of past episodes of aom'
smoke='passive smoking (1=yes 0=no)'
sibling='siblings (1=yes 0=no)'
sibs='number of sibs'
breast='being breast fed (1=yes 0=no)'
famhist='family history of AOM (1=yes 0=no)'
cry_0='crying at inclusion (1=yes 0=no)'
perf_0='perforation at inclusion (1=yes 0=no)'
vom_0='vomiting at inclusion (1=yes 0=no)'
oto_0='otorrhea at inclusion (1=yes 0=no)'
fever_0='fever at inclusion (1=yes 0=no)'
bilat_0='bilateral AOM at inclusion (1=yes 0=no)'
painr_0='pain right ear at inclusion (1=yes 0=no)'
painl_0='pain left ear at inclusion (1=yes 0=no)'
pain_0='earpain at inclusion (1=yes 0=no)'
rnose_0='runny nose at inclusion (1=yes 0=no)'
tmnorr_0='right tympanic mebrane normal at inclusion (1=no 0=yes)'
tmnorl_0='left tympanic mebrane normal at inclusion (1=no 0=yes)'
tmnor_0='tympanic mebrane normal at inclusion (1=no 0=yes)'
tmcolr_0='colour of right tympanic mebrane at inclusion (0=normal 1=red)'
tmcoll_0='colour of left tympanic mebrane at inclusion (0=normal 1=red)'
tmcol_0='colour of tympanic mebrane at inclusion (0=normal 1=red)'
tmbulr_0='bulging right tympanic membrane at inclusion (1=yes 0=no)'
tmbull_0='bulging left tympanic membrane at inclusion (1=yes 0=no)'
tmbul_0='bulging tympanic membrane at inclusion (1=yes 0=no)'
fever_1='fever after 1 day follow-up (1=yes 0=no)'
pain_1='earpain after 1 day follow-up (1=yes 0=no)'
analg_1='analgesics used  after 1 day follow-up (1=yes 0=no)'
fever_2='fever after 3-7 days follow-up (1=yes 0=no)'
analg_2='analgesics used  after 3-7 days follow-up (1=yes 0=no)'
pain_2='earpain after 3-7 days follow-up (1=yes 0=no)'
poutcome='fever and/or earpain after 3-7 days follow-up (1=yes 0=no)'
fevd1='fever at 1 day follow-up (1=yes 0=no)'
fevd2='fever at 2 days follow-up (1=yes 0=no)'
fevd3='fever at 3 days follow-up (1=yes 0=no)'
fevd4='fever at 4 days follow-up (1=yes 0=no)'
fevd5='fever at 5 days follow-up (1=yes 0=no)'
fevd6='fever at 6 days follow-up (1=yes 0=no)'
fevd7='fever at 7 days follow-up (1=yes 0=no)'
fevd8='fever at 8 days follow-up (1=yes 0=no)'
fevd9='fever at 2 days follow-up (1=yes 0=no)'
fevd10='fever at 10 days follow-up (1=yes 0=no)'
paind1='pain at 1 day follow-up (1=yes 0=no)'
paind2='pain at 2 days follow-up (1=yes 0=no)'
paind3='pain at 3 days follow-up (1=yes 0=no)'
paind4='pain at 4 days follow-up (1=yes 0=no)'
paind5='pain at 5 days follow-up (1=yes 0=no)'
paind6='pain at 6 days follow-up (1=yes 0=no)'
paind7='pain at 7 days follow-up (1=yes 0=no)'
paind8='pain at 8 days follow-up (1=yes 0=no)'
paind9='pain at 9 days follow-up (1=yes 0=no)'
paind10='pain at 10 days follow-up (1=yes 0=no)'
po_d1='poor outcome at 1 day follow-up (1=yes 0=no)'
po_d2='poor outcome at 2 days follow-up (1=yes 0=no)'
po_d3='poor outcome at 3 days follow-up (1=yes 0=no)'
po_d4='poor outcome at 4 days follow-up (1=yes 0=no)'
po_d5='poor outcome at 5 days follow-up (1=yes 0=no)'
po_d6='poor outcome at 6 days follow-up (1=yes 0=no)'
po_d7='poor outcome at 7 days follow-up (1=yes 0=no)'
po_d8='poor outcome at 8 days follow-up (1=yes 0=no)'
po_d9='poor outcome at 9 days follow-up (1=yes 0=no)'
po_d10='poor outcome at 10 days follow-up (1=yes 0=no)'
effr_am='middle ear effusion in right ear after one month follow-up (1=yes 0=no)'
effl_am='middle ear effusion in left ear after one month follow-up (1=yes 0=no)'
eff_am='middle ear effusion after one month follow-up (1=yes 0=no)'
;
run;


proc freq data=Damois;
tables age study gender treat rAOM AOMepi smoke season breast famhist sibling sibs bilat_0 cry_0
       painr_0 painl_0 pain_0 fever_0 rnose_0 cough_0 perf_0 oto_0 vom_0
       tmnorr_0 tmnorl_0 tmnor_0 tmcolr_0 tmcoll_0 tmcol_0 
       tmbulr_0 tmbull_0 tmbul_0 
	   fever_2 pain_2 poutcome effr_am effl_am eff_am
       fevd1 fevd2 fevd3 fevd4 fevd5 fevd6 fevd7 fevd8 fevd9 fevd10
       paind1 paind2 paind3 paind4 paind5 paind6 paind7 paind8 paind9 paind10
       po_d1 po_d2 po_d3 po_d4 po_d5 po_d6 po_d7 po_d8 po_d9 po_d10;
run;


libname Damois 'F:\Data IPD AOM\Damoiseaux\final version';
proc sort data=Damois; by trialnr;
data Damois.Damois (keep=trialnr study age gender treat rAOM AOMepi season 
       breast famhist sibling sibs smoke bilat_0 
       cry_0 pain_0 fever_0 rnose_0 cough_0 perf_0 oto_0 vom_0
       tmnor_0 tmcol_0 
       tmbul_0 
	   fever_2 pain_2 poutcome eff_am
       fevd1 fevd2 fevd3 fevd4 fevd5 fevd6 fevd7 fevd8 fevd9 fevd10
       paind1 paind2 paind3 paind4 paind5 paind6 paind7 paind8 paind9 paind10
       po_d1 po_d2 po_d3 po_d4 po_d5 po_d6 po_d7 po_d8 po_d9 po_d10);
set Damois;
proc contents data=Damois.Damois position;
run;






