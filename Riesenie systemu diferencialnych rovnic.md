```Mathematica

(*RIESENIE SYSTEMU DIFERENCIALNYCH ROVNIC*)

(*
ZADANIE:
Dany je system dvoch diferencialnych rovnic: y1'=2y1-y2-x a y2'=3y1-2y2+3x2 so zaciatocnymi podmienkami y1(0)=1 a y2(0)=-2. Rieste ho na intervale [0,3].

1.) Najdite jeho presne analyticke riesenie. Vysetrite existenciu a jednoznacnost jeho riesenia na zadanom intervale.
2.) Vypocitajte priblizne riesenie pomocou explicitnej jednokrokovej metody Euler s krokom h=0.1.
*)

(*RIESENIE*)
(*1.)*)
(*Presne analyticke riesenie je v tvare: vseobecne riesenie + zaciatocna podmienka:*)
Clear[x,y1,y2,ries]

ries=DSolve[{
    y1'[x]==2y1[x]-y2[x]-x,
    y2'[x]==3y1[x]-2y2[x]+3*x^2,
    y1[0]==1,y2[0]==-2},
   {y1[x],y2[x]},x]//Simplify

(*Definovanie presneho riesenia yp1 a yp2:*)
Clear[yp1]
yp1[x_]=ries[[1,1,2]]

Clear[yp2]
yp2[x_]=ries[[1,2,2]]

Clear[gr1]
gr1=Plot[yp1[x],{x,0,3},{PlotRange->{-1,9},PlotStyle->RGBColor[1,0,0]}]

Clear[gr2]
gr2=Plot[yp2[x],{x,0,3},{PlotRange->{-5,20},PlotStyle->RGBColor[0,1,0]}]

Show[gr1,gr2,PlotRange->{-5,20}]

(*Vysetrenie existencie a jednoznacnosti funkcie f1 a f2:*)
f1[x_,y_]=2*y1-y2-x
D[f1[x,y],y]
(*Zaver1 - f1 je definovana na celom intervale.*)

f2[x_,y_]=3*y1-2*y2+3*x^2
D[f2[x,y],y]
(*Zaver2 - f2 je definovana na celom intervale.*)

(*2.)*)
Clear[x,A,g,y]        
A={{2,-1},{3,-2}};
g[x_]={-x,3*x^2};
y[0]={1,-2};
x[0]=0;
h=0.1;
pocetiteracii=30;
Print["A = ",A//MatrixForm]
Print["g[x] = ",g[x]//MatrixForm]

Do[	
 x[i+1]=x[i]+h;
 y[i+1]=y[i]+h*(A.y[i]+g[x[i]])//N,
 {i,0,pocetiteracii}]

TableForm[
 Table[{x[i],y[i][[1]],y[i][[2]],Abs[y[i][[1]]-yp1[x[i]]],
  Abs[y[i][[2]]-yp2[x[i]]],
  Sqrt[(y[i][[1]]-yp1[x[i]])^2+(y[i][[2]]-yp2[x[i]])^2]},   {i,0,pocetiteracii}],
 TableHeadings->{None,{"xi","y1i","y2i", "chyba |yp-y1i|","chyba |yp-y2i|","celková glob. chyba"}},
 	TableSpacing->{1,3}]




