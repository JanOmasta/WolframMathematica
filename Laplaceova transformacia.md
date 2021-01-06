```Mathematica

(*LAPLACEOVA TRANSFORMÁCIA*)

(*
ALGORITMUS RIEŠENIA - VYSVETLENIE PRINCÍPU LAPLACEOVEJ TRANSFORMÁCIE:
Laplaceova transformácia sa vyuíva na riešenie diferenciálnych rovníc. Riešením diferenciálnych rovníc sa získa èasovı priebeh
vıstupnıch velièín dynamickıch systémov pri zadefinovanıch vstupnıch velièinách a zaèiatoènıch podmienkach. Riešenie diferenciálnych rovníc pomocou Laplaceovej transformácie mono rozdeli do troch krokov:
1.) 	Urobí sa Laplaceova transformácia diferenciálnej rovnice.
	Znamená to, e sa k originálom, ktoré v rovnici vystupujú
	nájdu obrazy. Rovnica, ktorú po transformácii dostaneme,
	je algebraická rovnica a neznámou v nej je obraz riešenia
	diferenciálnej rovnice.	


ZADANIE:
Riešte diferencialnu rovnicu: y''(t)- 9*y(t)=u(t) so zaèiatoènımi podmienkami y(0)=0, y'(0)=1, kde u(t)=2-t.



*)

(*RIESENIE*)
step1=LaplaceTransform[y''[t]-9*y[t]==2-t,t,p]