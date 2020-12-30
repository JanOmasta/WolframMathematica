```Mathematica

(*RIESENIE DIFERENCIALNEJ ROVNICE - VIACKROKOVA METODA*)

(*
ZADANIE:
Rieste  diferencialnu rovnicu (x+1).y'(x)-y(x)+2x=0 so zaciatocnou podmienkou y(0)=1 na intervale [0,2].

1.) Vysetrite existenciu a jednoznacnost jej riesenia. Ak je to mozne, najdite presne riesenie danej zaciatocnej ulohy.
2.) Najdite jej riesenie modifikovanou Eulerovou metodou na intervale [0,2]. Vypocitajte globalnu chybu.
3.) Najdite jej riesenie viackrokovou metodou MIDPOINT na intervale [0,2].  Vypocitajte globalnu chybu.
4.)Najdite jej riesenie viackrokovou metodou Adams - Bashford 2. radu na intervale [0,2].  Vypocitajte globalnu chybu.
*)

(*RIESENIE*)
(*1.)*)
Clear[yp,x]
riesenie=DSolve[{(x+1)*y'[x]-y[x]+2x==0,y[0]==1},y[x],x];
yp[x_]=riesenie[[1,1,2]]

gp=Plot[yp[x],{x,0,2},PlotStyle->RGBColor[1,0,0]]

f[x_,y_]=(y-2x)/(x+1)   (*Overenie existencie riesenia.*)
D[f[x,y],y]            (*Overenie jednoznacnosti riesenia.*)

(*2.)*)
Clear[f,x,y,i,h,pocetiteracii]
f[x_,y_]=(y-2*x)/(x+1);
h=0.1;
pocetiteracii=20;
x[0]=0;
y[0]=1;

Do[
  x[i+1]=x[i]+h;
  k1=f[x[i],y[i]];
  k2=f[x[i]+h/2,y[i]+h/2*k1];
  y[i+1]=y[i]+h*k2,
  {i,0,pocetiteracii}];

TableForm[Table[{i,NumberForm[x[i],5],NumberForm[y[i],5]},{i,0,pocetiteracii}],
 	TableHeadings->{None,{"i","xi","yi"}},
 	TableSpacing->{1,3}]

modeuler=Table[{x[n],y[n]},{n,0,pocetiteracii}];
obrmodEuler=ListPlot[modeuler,PlotStyle->PointSize[0.015]];
Show[gp,obrmodEuler,PlotRange->{0,1.5}]

Abs[yp[2]-y[20]]

(*3.)*)
Clear[f,x,y,i,h,pocetiteracii]
f[x_,y_]=(y-2*x)/(x+1);
h=0.1;
pocetiteracii=20;
x[0]=0;
y[0]=1;

Do[
  x[i+1]=x[i]+h;
  y[i+1]=y[i]+h*f[x[i],y[i]],
  {i,0,1}];

TableForm[Table[{i,NumberForm[x[i],5],NumberForm[y[i],5]},{i,0,1}],
 	TableHeadings->{None,{"i","xi","yi"}},
 	TableSpacing->{1,3}]

Do[   
  x[i+1]=x[i]+h;        
  y[i+1]=y[i-1]+2*h*f[x[i],y[i]],
    {i,1,pocetiteracii}];

TableForm[Table[{i,NumberForm[x[i],5],NumberForm[y[i],5]},{i,0,pocetiteracii}],
 	TableHeadings->{None,{"i","xi","yi"}},
 	TableSpacing->{1,3}]

midpoint=Table[{x[n],y[n]},{n,0,pocetiteracii}];
obrmidpoint=ListPlot[midpoint,PlotStyle->PointSize[0.015]];
Show[gp,obrmidpoint,PlotRange->{0,1.5}]

Abs[yp[2]-y[20]]