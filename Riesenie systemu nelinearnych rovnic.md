```Mathematica

(*RIESENIE SYSTEMU NELINEARNYCH ROVNIC*)

(*
ZADANIE:
Rieste system nelinearnych rovnic 2x-y+(1/9*e^-x)-1=0 a -x+2y+(1/9*e^-y=0 s presnostou 10^-15 pouzitim Newtonovej metody.
*)

(*RIESENIE*)
ContourPlot[{2x-y+1/9 Exp[-x]-1==0,-x+2y+1/9Exp[-y]==0},{x,-5,5},{y,-5,5}]

Clear[A,f,f1,f2,x,y];
f={f1[x,y],f2[x,y]};
f1[x_,y_]=2x-y+1/9 Exp[-x]-1;
f2[x_,y_]=-x+2y+1/9 Exp[-y];
A[x_,y_]=D[f,{{x,y}}];		(*Vypocet Jacobiho matice.*)
INV[x_,y_]=Inverse[A[x,y]];	(*Vypocet inverznej Jacobiho matice.*)

A[x,y]//MatrixForm
INV[x,y]//MatrixForm

Clear[x,y]
x[0]=1;
y[0]=1;
pocetopakovani=10;
tolerancia= 10^(-15);

Do[
  {x[i+1],y[i+1]}= {x[i],y[i]} -INV[x[i],y[i]].{f1[x[i],y[i]],f2[x[i],y[i]]}//N;
  If[Max[Abs[x[i+1]-x[i]], Abs[y[i+1]-y[i]]]<tolerancia,Break[]],{i,0,pocetopakovani}]

TableForm[
	Table[{i,NumberForm[x[i],16],NumberForm[y[i],16],Max[Abs[x[i]-x[i-1]], Abs[y[i]-y[i-1]]]},{i,0,pocetopakovani}],
 	TableHeadings->{None,{"i","xi","yi","|xi+1-xi|"}},
 	TableSpacing->{1,5}]
