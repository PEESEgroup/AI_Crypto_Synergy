Sets

t    'number of hours'                       /1*8760/
i    'number of countries'                  /1*38/
u    'index for print'                      /p1*p14/
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

d /0.1/

ssolar /0.471/
swind /0.471/
sdac /0.471/
sele /0.471/
sfc /0.05/
shtank /0.05/
sbattery /0.371/
sinterface /0.371/

N /1000000000/
epsilon /0.001/

dacmax /1000/
daceff /0.805/
ICdac /4069000/

optransport /0.01075/
opstorage /0.00517/

egp /8.35/

bmax /1000/
ICbattery /250250/
bfoc /3510/
cyceff /0.91/

ICinterface /125130/
ifoc /1750/

pen /0.049/

nserver /5000/

cfsolar /0.07849332/
cfwind /0.013218984/

cfbattery /16.1/
cfdac /0.015/

radin(t)
wspeed(t)
Pw(t)
PUE(t)
carbon
gprice
sr
obj_save(i,u);

Binary variable
x1(t)
x2(t)
;

integer variable
nbattery
ndac
;

Positive variable

Asolar
ratedcapacity
Pratedwind

Psolar(t)
Pwind(t)
Pgrid(t)
Ptotal(t)
Pai(t)
Pcharge(t)
Pdischarge(t)
Pbattery(t)
Pdac(t)

bop(t)
dacop(t)

carboncap(t)
carbontotal

emission(t)
emissiontotal

Ctotal
Csolar
Cwind
Cdac
Cbattery
Cinterface

capinterface

OPEXtotal
OPEXsolar
OPEXwind
OPEXgrid
OPEXtransport
OPEXstorage
OPEXdac
OPEXbattery
OPEXinterface
OPEXPenalty

SALtotal 
SALsolar 
SALwind 
SALdac
SALbattery
SALinterface 

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

eqbop(t)

eqSOC(t)
eqPbattery(t)

eqPci(t)
eqPdi(t)

eqPit(t)
eqPai(t)

eqCsolar
eqCwind
eqCdac
eqCbattery
eqCinterface
eqCtotal

eqrated
eqOPEXsolar
eqOPEXwind
eqOPEXgrid
eqOPEXdac
eqOPEXtransport
eqOPEXstorage
eqOPEXbattery
eqOPEXPenalty
eqOPEXinterface
eqOPEXtotal

eqSALtotal
eqSALsolar
eqSALwind
eqSALdac
eqSALbattery
eqSALinterface

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

eqbinary(t)
eqdischargelimit1(t)
eqchargelimit1(t)

eqdischargelimit2(t)
eqchargelimit2(t)
;


eqPsolar(t).. Psolar(t) =e= cap*Asolar*radin(t)/1000 ;
eqPwind(t).. Pwind(t) =e= Pratedwind*Pw(t) ;
eqPtotal(t).. Ptotal(t) =e= Psolarutilized(t)+Pwindutilized(t)+Pgrid(t)+Pdischarge(t)*cyceff;

eqPsolarutilized(t).. Psolarutilized(t) =l= Psolar(t) ; 
eqPwindutilized(t).. Pwindutilized(t) =l= Pwind(t) ; 

eqPgridtotal.. Pgridtotal =e= sum(t,Pgrid(t));
eqPsolartotal.. Psolartotal =e= sum(t,Psolarutilized(t)); 
eqPwindtotal.. Pwindtotal =e= sum(t,Pwindutilized(t));  

eqPbalance(t).. Ptotal(t) =e= Pai(t)+Pdac(t)+Pcharge(t)*cyceff ;

eqbop(t).. bop(t) =l= nbattery ; 

eqSOC(t).. Pbattery(t) =e= (ord(t) = 1) * 0 + (ord(t) > 1) * (Pbattery(t-1) + Pcharge(t-1)*cyceff - Pdischarge(t-1)*cyceff);
eqPbattery(t).. Pbattery(t) =l= nbattery*bmax ;

eqPci(t).. Pcharge(t) =l= capinterface*1000 ;
eqPdi(t).. Pdischarge(t) =l= capinterface*1000 ;

eqPit(t).. Pit(t) =e= nserver*egp ;
eqPai(t).. Pai(t) =e= Pit(t)*PUE(t);

eqCsolar.. Csolar =e= Asolar*ICsolar ;
eqCwind.. Cwind =e= Pratedwind*ICwind ; 
eqCdac.. Cdac =e= ndac*ICdac ;
eqCbattery.. Cbattery =e= nbattery*ICbattery ;
eqCinterface.. Cinterface =e= capinterface*ICinterface ;
eqCtotal.. Ctotal =e= Csolar+Cwind+Cdac+Cbattery+Cinterface ;

eqrated.. ratedcapacity =e= sum(t,Psolar(t))/8760 ;
eqOPEXsolar.. OPEXsolar =e= sfoc*ratedcapacity*NY ;
eqOPEXwind.. OPEXwind =e= wfoc*Pratedwind*NY ;
eqOPEXgrid.. OPEXgrid =e= sum(t,Pgrid(t))*gprice*NY ;
eqOPEXdac.. OPEXdac =e= 0.04*Cdac*NY ;
eqOPEXtransport.. OPEXtransport =e= optransport*carbontotal*NY ;
eqOPEXstorage.. OPEXstorage =e= opstorage*carbontotal*NY ;
eqOPEXbattery.. OPEXbattery =e= bfoc*nbattery*NY ;
eqOPEXPenalty.. OPEXPenalty =e= sum(t,emission(t))*pen*NY ;
eqOPEXinterface.. OPEXinterface =e= capinterface*ifoc*NY ;
eqOPEXtotal.. OPEXtotal =e= OPEXsolar+OPEXwind+OPEXgrid+OPEXdac+OPEXtransport+OPEXstorage+OPEXbattery+OPEXPenalty+OPEXinterface ;

eqSALtotal.. SALtotal =e= (SALsolar+SALwind+SALdac+SALbattery+SALinterface)/((1+d)**NY) ;
eqSALsolar.. SALsolar =e= Csolar*ssolar ;
eqSALwind.. SALwind =e= Cwind*swind ;
eqSALdac.. SALdac =e= Cdac*sdac ;
eqSALbattery.. SALbattery =e= Cbattery*sbattery ;
eqSALinterface.. SALinterface =e= Cinterface*sinterface ; 

eqTC.. TC =e= ((OPEXtotal)/(NY*d)*(1-1/((1+d)**NY))+Ctotal-SALtotal) ;

eqPdemandtotal.. Pdemandtotal =e= sum(t,Ptotal(t)) ;

eqcarboncaptured(t).. carboncap(t) =e= Pdac(t)*daceff*(1-cfdac) ;
eqdacop(t).. dacop(t) =l= ndac ;
eqPdac(t).. Pdac(t) =e= dacop(t)*dacmax ;
eqcarbontotal.. carbontotal =e= sum(t,carboncap(t)) ;

eqemission(t).. emission(t) =e= Pgrid(t)*carbon+Psolarutilized(t)*cfsolar+Pwindutilized(t)*cfwind + cfbattery*nbattery ;
eqemissiontotal.. emissiontotal =e= sum(t,emission(t)) ;

eqcarbonbalance.. carbontotal =e= emissiontotal ;

eqPaictotal.. Paictotal =e= sum(t,Pai(t))/(d)*(1-1/((1+d)**NY)) ;
eqPaiatotal.. Paiatotal =e= sum(t,Pai(t)) ;

eqbinary(t).. x1(t)+x2(t) =e= 1 ;
eqdischargelimit1(t).. Pdischarge(t) =l= N*x1(t) ;
eqchargelimit1(t).. Pcharge(t) =l= N*x2(t) ;

eqdischargelimit2(t).. Pdischarge(t) =l= nbattery*bmax ;
eqchargelimit2(t).. Pcharge(t) =l= nbattery*bmax ;

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
obj_save(i,'p8')=nbattery.l;
obj_save(i,'p9')=capinterface.l;
obj_save(i,'p10')=Cbattery.l;
obj_save(i,'p11')=Cinterface.l;
obj_save(i,'p12')=Psolartotal.l;
obj_save(i,'p13')=Pwindtotal.l;
obj_save(i,'p14')=carbontotal.l;
);

obj_save(i,u)$(NOT obj_save(i,u)) = zero;
display obj_save;




