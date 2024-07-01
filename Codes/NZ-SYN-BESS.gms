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

COP /3/
hf /1.06/

af /0.255/

ICsolar /200/
sfoc /18.61057243/
ICwind /1333318.883/
wfoc /29750.14805/
IChpump /300000/

hpmax /1000/

difficulty /59/

price /34001/

d /0.1/

ssolar /0.471/
swind /0.471/
sminer /0/
shpump /0.371/
sdac /0.471/
sbattery /0.371/
sinterface /0.371/

R /3.25/
ICminer /3395/
minerene /3.25/
minerhash /100/

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
nminer
nhpump
ndac
nbattery
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
Pbitcoin(t)
Pcharge(t)
Pdischarge(t)
Pbattery(t)
Pauxiliary(t)

Pminer(t)
Phpump(t)
Pheat(t)
Pdac(t)

hpumpop(t)
minerop(t)
bop(t)
dacop(t)

carboncap(t)
carbontotal

emission(t)
emissiontotal

Ctotal
Csolar
Cwind
Cminer
Chpump
Cdac
Cbattery
Cinterface

capinterface

OPEXtotal
OPEXsolar
OPEXwind
OPEXhpump
OPEXgrid
OPEXtransport
OPEXstorage
OPEXdac
OPEXbattery
OPEXPenalty
OPEXinterface

SALtotal 
SALsolar 
SALwind 
SALminer 
SALhpump
SALdac
SALbattery
SALinterface 

revcrypto(t)
Hash(t) 
revcryptototal 
 
Pdemandtotal

Pit(t)
Pgridtotal

Paictotal
Paiatotal

NPV

Psolartotal
Pwindtotal

Bitcointotal

minercap

Psolarutilized(t)
Pwindutilized(t)
;

Variable
Pbitcointotal
;

Equations
eqPsolar(t)
eqPwind(t)
eqPtotal(t)

eqPsolarutilized(t)
eqPwindutilized(t)

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

eqPbitcoin(t)
eqPauxiliary(t)
eqPheat(t)
eqPhpump(t)
eqPhpump2(t)
eqhpumpop(t)
eqPminer(t)
eqminerop(t)

eqCsolar
eqCwind
eqCminer
eqChpump
eqCdac
eqCbattery
eqCinterface
eqCtotal

eqrated
eqOPEXsolar
eqOPEXwind
eqOPEXhpump
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
eqSALminer
eqSALhpump
eqSALdac
eqSALbattery
eqSALinterface

eqrevcrypto(t)
eqHash(t)

eqHash2(t)

eqrevcryptototal

eqNPV
eqNPV2

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
eqPbitcointotal
eqBitcointotal

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

eqPbalance(t).. Ptotal(t) =e= Pai(t)+Pbitcoin(t)+Pdac(t)+Pcharge(t)*cyceff ;

eqbop(t).. bop(t) =l= nbattery ; 

eqSOC(t).. Pbattery(t) =e= (ord(t) = 1) * 0 + (ord(t) > 1) * (Pbattery(t-1) + Pcharge(t-1)*cyceff - Pdischarge(t-1)*cyceff);
eqPbattery(t).. Pbattery(t) =l= nbattery*bmax ;

eqPci(t).. Pcharge(t) =l= capinterface*1000 ;
eqPdi(t).. Pdischarge(t) =l= capinterface*1000 ;

eqPit(t).. Pit(t) =e= nserver*egp ;
eqPai(t).. Pai(t) =e= Pit(t)*PUE(t);

eqPbitcoin(t).. Pbitcoin(t) =e= Pminer(t)+Pauxiliary(t);
eqPauxiliary(t).. Pauxiliary(t) =e= Pminer(t)*af + Phpump(t) ;
eqPheat(t).. Pheat(t) =e= Pminer(t)*hf ;
eqPhpump(t).. Pheat(t) =e= Phpump(t)*COP ;
eqPhpump2(t).. Phpump(t) =l= hpumpop(t)*hpmax;
eqhpumpop(t).. hpumpop(t) =l= nhpump ;
eqPminer(t).. Pminer(t) =e= minerop(t)*minerene ;
eqminerop(t).. minerop(t) =l= nminer ;

eqCsolar.. Csolar =e= Asolar*ICsolar ;
eqCwind.. Cwind =e= Pratedwind*ICwind ; 
eqCminer.. Cminer =e= nminer*ICminer*2 ;
eqChpump.. Chpump =e= nhpump*IChpump ;
eqCdac.. Cdac =e= ndac*ICdac ;
eqCbattery.. Cbattery =e= nbattery*ICbattery ;
eqCinterface.. Cinterface =e= capinterface*ICinterface ;
eqCtotal.. Ctotal =e= Csolar+Cwind+Cminer+Chpump+Cdac+Cbattery+Cinterface ;

eqrated.. ratedcapacity =e= sum(t,Psolar(t))/8760 ;
eqOPEXsolar.. OPEXsolar =e= sfoc*ratedcapacity*NY ;
eqOPEXwind.. OPEXwind =e= wfoc*Pratedwind*NY ;
eqOPEXhpump.. OPEXhpump =e= 0.05*nhpump*IChpump*NY  ;
eqOPEXgrid.. OPEXgrid =e= sum(t,Pgrid(t))*gprice*NY ;
eqOPEXdac.. OPEXdac =e= 0.04*Cdac*NY ;
eqOPEXtransport.. OPEXtransport =e= optransport*carbontotal*NY ;
eqOPEXstorage.. OPEXstorage =e= opstorage*carbontotal*NY ;
eqOPEXbattery.. OPEXbattery =e= bfoc*nbattery*NY ;
eqOPEXPenalty.. OPEXPenalty =e= sum(t,emission(t))*pen*NY ;
eqOPEXinterface.. OPEXinterface =e= capinterface*ifoc*NY ;
eqOPEXtotal.. OPEXtotal =e= OPEXsolar+OPEXwind+OPEXhpump+OPEXgrid+OPEXdac+OPEXtransport+OPEXstorage+OPEXbattery+OPEXPenalty+OPEXinterface ;

eqSALtotal.. SALtotal =e= (SALsolar+SALwind+SALminer+SALhpump+SALdac+SALbattery+SALinterface)/((1+d)**NY) ;
eqSALsolar.. SALsolar =e= Csolar*ssolar ;
eqSALwind.. SALwind =e= Cwind*swind ;
eqSALminer.. SALminer =e= Cminer*sminer ;
eqSALhpump.. SALhpump =e= Chpump*shpump ;
eqSALdac.. SALdac =e= Cdac*sdac ;
eqSALbattery.. SALbattery =e= Cbattery*sbattery ;
eqSALinterface.. SALinterface =e= Cinterface*sinterface ; 

eqrevcrypto(t).. revcrypto(t) =e= (price*R*Hash(t)*86400)/(difficulty*4294967296) ;
eqHash(t).. Hash(t) =e= minerop(t)*minerhash ;

eqHash2(t).. Hash(t) =l= minercap ;

eqrevcryptototal.. revcryptototal =e= NY*sum(t,revcrypto(t)) ;

eqNPV.. NPV =e= ((revcryptototal-OPEXtotal)/(NY*d)*(1-1/((1+d)**NY))-Ctotal+SALtotal) ;
eqNPV2.. NPV =e= 0 ; 

eqPdemandtotal.. Pdemandtotal =e= sum(t,Ptotal(t)) ;

eqcarboncaptured(t).. carboncap(t) =e= Pdac(t)*daceff*(1-cfdac) ;
eqdacop(t).. dacop(t) =l= ndac ;
eqPdac(t).. Pdac(t) =e= dacop(t)*dacmax ;
eqcarbontotal.. carbontotal =e= sum(t,carboncap(t)) ;

eqemission(t).. emission(t) =e= Pgrid(t)*carbon+Psolarutilized(t)*cfsolar+Pwindutilized(t)*cfwind+ cfbattery*nbattery ;
eqemissiontotal.. emissiontotal =e= sum(t,emission(t)) ;

eqcarbonbalance.. carbontotal =e= emissiontotal ;

eqPaictotal.. Paictotal =e= sum(t,Pai(t))/(d)*(1-1/((1+d)**NY)) ;
eqPaiatotal.. Paiatotal =e= sum(t,Pai(t)) ;
eqPbitcointotal.. Pbitcointotal =e= sum(t,Pbitcoin(t)) ; 
eqBitcointotal.. Bitcointotal =e= sum(t,(R*Hash(t)*86400)/(difficulty*4294967296)) ;

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

solve AI_BTC_synergy using mip minimizing Pbitcointotal ;

obj_save(i,'p1')=Paictotal.l;
obj_save(i,'p2')=Paiatotal.l;
obj_save(i,'p3')=Pgridtotal.l;
obj_save(i,'p4')=Asolar.l;
obj_save(i,'p5')=Pratedwind.l;
obj_save(i,'p6')=Pbitcointotal.l;
obj_save(i,'p7')=Bitcointotal.l;
obj_save(i,'p8')=ndac.l;
obj_save(i,'p9')=nbattery.l;
obj_save(i,'p10')=capinterface.l;
obj_save(i,'p11')=Cbattery.l;
obj_save(i,'p12')=Cinterface.l;
obj_save(i,'p13')=Psolartotal.l;
obj_save(i,'p14')=Pwindtotal.l;
obj_save(i,'p15')=carbontotal.l;

);

obj_save(i,u)$(NOT obj_save(i,u)) = zero;
display obj_save;




