Sets

t    'number of hours'                       /1*8760/
i    'number of countries'                  /1*38/
u    'index for print'                      /p1*p15/
m    'index for energy distribution'         /1*1/;

$call gdxxrw.exe AI_rad.xlsx par=radiation rng=sheet1!A1:LXY39 
*=== Now import data from GDX
Parameter radiation(i,t);
$gdxin AI_rad.gdx
$load radiation
$gdxin

$call gdxxrw.exe AI_speed.xlsx par=speed rng=sheet1!A1:LXY39 
*=== Now import data from GDX
Parameter speed(i,t);
$gdxin AI_speed.gdx
$load speed
$gdxin

$call gdxxrw.exe AI_PUE.xlsx par=pu rng=sheet1!A1:LXY39 
*=== Now import data from GDX
Parameter pu(i,t);
$gdxin AI_PUE.gdx
$load pu
$gdxin

$call gdxxrw.exe Carbon_intensity.xlsx par=ci rng=sheet1!A1:AM2
*=== Now import data from GDX
Parameter ci(m,i);
$gdxin Carbon_intensity.gdx
$load ci
$gdxin

$call gdxxrw.exe Price_intensity.xlsx par=pri rng=sheet1!A1:AM2
*=== Now import data from GDX
Parameter pri(m,i);
$gdxin Price_intensity.gdx
$load pri
$gdxin

$call gdxxrw.exe Server_ratio.xlsx par=sratio rng=sheet1!A1:AM2
*=== Now import data from GDX
Parameter sratio(m,i);
$gdxin Server_ratio.gdx
$load sratio
$gdxin

Acronym zero

Parameter

NY /8/

cap /0.1662/
vcin /2/
vcout /25/
vr /11/

ICsolar /200/
sfoc /18.61057243/
ICwind /1333318.883/
wfoc /29750.14805/
IChpump /300000/

ICele /630000/
ICfc /300000/
IChtank /700/

elemax /1000/
fcmax /1000/

d /0.1/

ssolar /0.471/
swind /0.471/
sdac /0.471/
sele /0.471/
sfc /0.05/
shtank /0.471/

N /1000000000/
epsilon /0.001/

dacmax /1000/
daceff /0.805/
ICdac /4069000/

optransport /0.01075/
opstorage /0.00517/

effele /0.68/
efffc /0.58/
LHV /33.33/

egp /8.35/

pen /0.049/

nserver /5000/

cfsolar /0.07849332/
cfwind /0.013218984/

cfele /28/
cffc /57.7/
cfdac /0.015/

radin(t)
wspeed(t)
Pw(t)
PUE(t)
carbon
gprice
sr
obj_save(i,u);


integer variable
ndac
nele
nfc
;

Positive variable

Asolar
ratedcapacity
Pratedwind

Psolar(t)
Pwind(t)
Pgrid(t)
Pele(t)
Pfc(t)
Ptotal(t)
Pai(t)
Pdac(t)

h2p(t)
h2o(t)
soh(t)

eleop(t)
fcop(t)
dacop(t)

carboncap(t)
carbontotal

emission(t)
emissiontotal

Ctotal
Csolar
Cwind
Cdac
Cele
Cfc
Chtank

captank

OPEXtotal
OPEXsolar
OPEXwind
OPEXgrid
OPEXtransport
OPEXstorage
OPEXdac
OPEXele
OPEXfc
OPEXPenalty
OPEXtank

SALtotal 
SALsolar 
SALwind 
SALdac
SALele
SALfc
SALhtank

Pdemandtotal
Pgridtotal

Pit(t)
Paictotal
Paiatotal

Psolartotal
Pwindtotal

Psolarutilized(t)
Pwindutilized(t)
;

Variable
TC
;

Equations
eqPsolar(t)
eqPwind(t)
eqPtotal(t)

eqPgridtotal
eqPsolartotal
eqPwindtotal

eqPbalance(t)

eqh2p(t)
eqPele2(t)
eqeleop(t)

eqPfc(t)
eqPfc2(t)
eqfcop(t)
eqsoh(t)
eqsoh2(t)

eqPit(t)
eqPai(t)

eqCsolar
eqCwind
eqCdac
eqCele
eqCfc
eqChtank
eqCtotal

eqrated
eqOPEXsolar
eqOPEXwind
eqOPEXgrid
eqOPEXdac
eqOPEXtransport
eqOPEXstorage
eqOPEXele
eqOPEXfc
eqOPEXPenalty
eqOPEXtank
eqOPEXtotal

eqSALtotal
eqSALsolar
eqSALwind
eqSALdac
eqSALele
eqSALfc
eqSALhtank

eqTC

eqPdemandtotal

eqcarboncaptured(t)
eqdacop(t)
eqPdac(t)
eqcarbontotal

eqemission(t)
eqemissiontotal

eqcarbonbalance

eqPaictotal
eqPaiatotal

eqPsolarutilized(t)
eqPwindutilized(t)
;


eqPsolar(t).. Psolar(t) =e= cap*Asolar*radin(t)/1000 ;
eqPwind(t).. Pwind(t) =e= Pratedwind*Pw(t) ;
eqPtotal(t).. Ptotal(t) =e= Psolarutilized(t)+Pwindutilized(t)+Pgrid(t)+Pfc(t);

eqPsolarutilized(t).. Psolarutilized(t) =l= Psolar(t) ; 
eqPwindutilized(t).. Pwindutilized(t) =l= Pwind(t) ; 

eqPgridtotal.. Pgridtotal =e= sum(t,Pgrid(t));
eqPsolartotal.. Psolartotal =e= sum(t,Psolarutilized(t)); 
eqPwindtotal.. Pwindtotal =e= sum(t,Pwindutilized(t));  

eqPbalance(t).. Ptotal(t) =e= Pai(t)+Pdac(t)+Pele(t) ;

eqh2p(t).. h2p(t) =e= Pele(t)*effele/LHV ;
eqPele2(t).. Pele(t) =e= eleop(t)*elemax ;
eqeleop(t).. eleop(t) =l= nele ;

eqPfc(t).. Pfc(t) =e= h2o(t)*efffc*LHV ;
eqPfc2(t).. Pfc(t) =e= fcop(t)*fcmax ;
eqfcop(t).. fcop(t) =l= nfc ;
eqsoh(t).. soh(t) =e= (ord(t) = 1) * 0 + (ord(t) > 1) * (soh(t-1) + h2p(t-1) - h2o(t-1));
eqsoh2(t).. soh(t) =l= captank ;

eqPit(t).. Pit(t) =e= nserver*egp ;
eqPai(t).. Pai(t) =e= Pit(t)*PUE(t);

eqCsolar.. Csolar =e= Asolar*ICsolar ;
eqCwind.. Cwind =e= Pratedwind*ICwind ; 
eqCdac.. Cdac =e= ndac*ICdac ;
eqCele.. Cele =e= nele*ICele ;
eqCfc.. Cfc =e= nfc*ICfc ;
eqChtank.. Chtank =e= captank*Ichtank ;
eqCtotal.. Ctotal =e= Csolar+Cwind+Cdac+Cele+Cfc+Chtank ;

eqrated.. ratedcapacity =e= sum(t,Psolar(t))/8760 ;
eqOPEXsolar.. OPEXsolar =e= sfoc*ratedcapacity*NY ;
eqOPEXwind.. OPEXwind =e= wfoc*Pratedwind*NY ;
eqOPEXgrid.. OPEXgrid =e= sum(t,Pgrid(t))*gprice*NY ;
eqOPEXdac.. OPEXdac =e= 0.04*Cdac*NY ;
eqOPEXtransport.. OPEXtransport =e= optransport*carbontotal*NY ;
eqOPEXstorage.. OPEXstorage =e= opstorage*carbontotal*NY ;
eqOPEXele.. OPEXele =e= 0.03*Cele*NY ; 
eqOPEXfc.. OPEXfc =e= 0.025*Cfc*NY ;
eqOPEXPenalty.. OPEXPenalty =e= sum(t,emission(t))*pen*NY ;
eqOPEXtank.. OPEXtank =e= Chtank*NY*0.17 ;
eqOPEXtotal.. OPEXtotal =e= OPEXsolar+OPEXwind+OPEXgrid+OPEXdac+OPEXtransport+OPEXstorage+OPEXPenalty+OPEXtank+OPEXele+OPEXfc ;

eqSALtotal.. SALtotal =e= (SALsolar+SALwind+SALdac+SALele+SALfc+SALhtank)/((1+d)**NY) ;
eqSALsolar.. SALsolar =e= Csolar*ssolar ;
eqSALwind.. SALwind =e= Cwind*swind ;
eqSALdac.. SALdac =e= Cdac*sdac ;
eqSALele.. SALele =e= Cele*sele ;
eqSALfc.. SALfc =e= Cfc*sfc ;
eqSALhtank.. SALhtank =e= Chtank*shtank ; 

eqTC.. TC =e= ((OPEXtotal)/(NY*d)*(1-1/((1+d)**NY))+Ctotal-SALtotal) ;

eqPdemandtotal.. Pdemandtotal =e= sum(t,Ptotal(t)) ;

eqcarboncaptured(t).. carboncap(t) =e= Pdac(t)*daceff*(1-cfdac) ;
eqdacop(t).. dacop(t) =l= ndac ;
eqPdac(t).. Pdac(t) =e= dacop(t)*dacmax ;
eqcarbontotal.. carbontotal =e= sum(t,carboncap(t)) ;

eqemission(t).. emission(t) =e= Pgrid(t)*carbon+Psolarutilized(t)*cfsolar+Pwindutilized(t)*cfwind+ cfele*nele + cffc*nfc ;
eqemissiontotal.. emissiontotal =e= sum(t,emission(t)) ;

eqcarbonbalance.. carbontotal =e= emissiontotal ;

eqPaictotal.. Paictotal =e= sum(t,Pai(t))/(d)*(1-1/((1+d)**NY)) ;
eqPaiatotal.. Paiatotal =e= sum(t,Pai(t)) ;

model AI_BTC_synergy / all /;

loop (i$(ord(i)<= 38),

radin(t) = radiation(i,t) ;
wspeed(t) = speed(i,t) ;
PUE(t) = pu(i,t);

Pw(t)$(wspeed(t) gt vcin and wspeed(t) le vr) = 1.434*1000*(wspeed(t)- vcin)/(vr-vcin);
Pw(t)$(wspeed(t) le vcin)=0;
Pw(t)$(wspeed(t) gt vr and wspeed(t) le vcout)= 1.434*1000;


loop (m$(ord(m)<= 1),
sr = sratio(m,i);
carbon = ci(m,i);
gprice = pri(m,i);
)

solve AI_BTC_synergy using mip minimizing TC ;

obj_save(i,'p1')=TC.l;
obj_save(i,'p2')=Paictotal.l;
obj_save(i,'p3')=Paiatotal.l;
obj_save(i,'p4')=Pgridtotal.l;
obj_save(i,'p5')=Asolar.l;
obj_save(i,'p6')=Pratedwind.l;
obj_save(i,'p7')=ndac.l;
obj_save(i,'p8')=nele.l;
obj_save(i,'p9')=nfc.l;
obj_save(i,'p10')=Cele.l;
obj_save(i,'p11')=Cfc.l;
obj_save(i,'p12')=Chtank.l;
obj_save(i,'p13')=Psolartotal.l;
obj_save(i,'p14')=Pwindtotal.l;
obj_save(i,'p15')=carbontotal.l;

);

obj_save(i,u)$(NOT obj_save(i,u)) = zero;
display obj_save;




