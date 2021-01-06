```Mathematica

(*LAPLACEOVA TRANSFORM�CIA*)

(*
ALGORITMUS RIE�ENIA - VYSVETLENIE PRINC�PU LAPLACEOVEJ TRANSFORM�CIE:
Laplaceova transform�cia sa vyu��va na rie�enie diferenci�lnych rovn�c. Rie�en�m diferenci�lnych rovn�c sa z�ska �asov� priebeh
v�stupn�ch veli��n dynamick�ch syst�mov pri zadefinovan�ch vstupn�ch veli�in�ch a za�iato�n�ch podmienkach. Laplaceova
transform�cia funkcie ra�lnej premennej f(t) je funkcia
komplexnej premennej F(p) definovan� vz�ahom, ktor� sa naz�va
Laplaceov integr�l. Rie�enie diferenci�lnych rovn�c pomocou
Laplaceovej transform�cie mo�no rozdeli� do troch krokov:
1.) 	Urob� sa Laplaceova transform�cia diferenci�lnej rovnice.
	Znamen� to, �e sa k origin�lom, ktor� v rovnici vystupuj�
	n�jdu obrazy. Rovnica, ktor� po transform�cii dostaneme,
	je algebraick� rovnica a nezn�mou v nej je obraz rie�enia
	diferenci�lnej rovnice.
2.)	Vyrie�i sa algebraick� rovnica. Rie�en�m algebraickej
	rovnice je obraz rie�enia diferenci�lnej rovnice, ktor�
	m� naj�astej�ie tvar zlomku.
3.)	Urob� sa sp�tn� (inverzn�) Laplaceova transform�cia.
	Sp�tnou Laplaceovou transform�ciou sa z�ska origin�l k
	obrazu rie�enia diferenci�lnej rovnice, a teda rie�enie
	diferenci�lnej rovnice v �asovej oblasti.	


ZADANIE:
Rie�te diferencialnu rovnicu: y''(t)- 9*y(t)=u(t) so za�iato�n�mi podmienkami y(0)=0, y'(0)=1, kde u(t)=2-t.
*)

(*RIESENIE*)
step1=LaplaceTransform[y''[t]-9*y[t]==2-t,t,p]

step2=step1/.{x[0]->0,x'[0]->1}

ries_dif=Solve[step2,LaplaceTransform[x[t],t,p]]

step3=InverseLaplaceTransform[ries_dif[[1,1,2]],p,t]

Plot[step3,{t,0,5}]

