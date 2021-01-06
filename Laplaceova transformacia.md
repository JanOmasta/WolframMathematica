```Mathematica

(*LAPLACEOVA TRANSFORMÁCIA*)

(*
ALGORITMUS RIEŠENIA - VYSVETLENIE PRINCÍPU LAPLACEOVEJ TRANSFORMÁCIE:
Laplaceova transformácia sa využíva na riešenie diferenciálnych rovníc. Riešením diferenciálnych rovníc sa získa èasový priebeh
výstupných velièín dynamických systémov pri zadefinovaných vstupných velièinách a zaèiatoèných podmienkach. Laplaceova
transformácia funkcie raálnej premennej f(t) je funkcia
komplexnej premennej F(p) definovaná vzahom, ktorý sa nazýva
Laplaceov integrál. Riešenie diferenciálnych rovníc pomocou
Laplaceovej transformácie možno rozdeli do troch krokov:
1.) 	Urobí sa Laplaceova transformácia diferenciálnej rovnice.
	Znamená to, že sa k originálom, ktoré v rovnici vystupujú
	nájdu obrazy. Rovnica, ktorú po transformácii dostaneme,
	je algebraická rovnica a neznámou v nej je obraz riešenia
	diferenciálnej rovnice.
2.)	Vyrieši sa algebraická rovnica. Riešením algebraickej
	rovnice je obraz riešenia diferenciálnej rovnice, ktorý
	má najèastejšie tvar zlomku.
3.)	Urobí sa spätná (inverzná) Laplaceova transformácia.
	Spätnou Laplaceovou transformáciou sa získa originál k
	obrazu riešenia diferenciálnej rovnice, a teda riešenie
	diferenciálnej rovnice v èasovej oblasti.	


ZADANIE:
Riešte diferencialnu rovnicu: y''(t)- 9*y(t)=u(t) so zaèiatoènými podmienkami y(0)=0, y'(0)=1, kde u(t)=2-t.
*)

(*RIESENIE*)
step1=LaplaceTransform[y''[t]-9*y[t]==2-t,t,p]

step2=step1/.{x[0]->0,x'[0]->1}

ries_dif=Solve[step2,LaplaceTransform[x[t],t,p]]

step3=InverseLaplaceTransform[ries_dif[[1,1,2]],p,t]

Plot[step3,{t,0,5}]

