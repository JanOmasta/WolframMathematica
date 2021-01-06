```Mathematica

(*LAPLACEOVA TRANSFORM�CIA*)

(*
ALGORITMUS RIE�ENIA - VYSVETLENIE PRINC�PU LAPLACEOVEJ TRANSFORM�CIE:
Laplaceova transform�cia sa vyu��va na rie�enie diferenci�lnych rovn�c. Rie�en�m diferenci�lnych rovn�c sa z�ska �asov� priebeh
v�stupn�ch veli��n dynamick�ch syst�mov pri zadefinovan�ch vstupn�ch veli�in�ch a za�iato�n�ch podmienkach. Rie�enie diferenci�lnych rovn�c pomocou Laplaceovej transform�cie mo�no rozdeli� do troch krokov:
1.) 	Urob� sa Laplaceova transform�cia diferenci�lnej rovnice.
	Znamen� to, �e sa k origin�lom, ktor� v rovnici vystupuj�
	n�jdu obrazy. Rovnica, ktor� po transform�cii dostaneme,
	je algebraick� rovnica a nezn�mou v nej je obraz rie�enia
	diferenci�lnej rovnice.	


ZADANIE:
Rie�te diferencialnu rovnicu: y''(t)- 9*y(t)=u(t) so za�iato�n�mi podmienkami y(0)=0, y'(0)=1, kde u(t)=2-t.



*)

(*RIESENIE*)
step1=LaplaceTransform[y''[t]-9*y[t]==2-t,t,p]