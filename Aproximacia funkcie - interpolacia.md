```Mathematica

(*APROXIMACIA FUNKCIE - INTERPOLACIA*)

(*
ZADANIE:
Najdite Newtonov interpolacny polynom 3. stupna, ktory co najlepsie popisuje zadanu nasledovnu tabulku.

xi	-2.300 0.263 1.056 3.397 3.995 4.948 5.508	
yi	31.462 -0.522 1.291 -14.724 -81.649 -157.138 -322.556

Pomocou tabulky diferencii najdite Newtonov interpolacny polynom 3. stupna, pomocou ktoreho dokazete co najpresnejsie vypocitat pribliznu hodnotu f(-1.531).

1.) Uvedte Newtonov interpolacny polynom.
2.) Uvedte hodnotu interpolacneho polynomu v bode f(-1.531).
3.) Uvedte najdeny interpolacny polynom. Zalezi na tom, ci vypocitate Newtonov interpolacny polynom vpred alebo Newtonov interpolacny polynom vzad?
*)

(*RIESENIE*)
(*1.)*)
data={{-2.3,31.462},{0.263,-0.522},{1.056,1.291},
{3.397,-14.724}}

polynom[x_]=a*x^3+b*x^2+c*x+d

x=Transpose[data][[1]]
f=Transpose[data][[2]]

Clear[rov1,rov2,rov3,rov4,a,b,c,d]

rov1=a*x[[1]]^3+b*x[[1]]^2+c*x[[1]]+d==f[[1]]
rov2=a*x[[2]]^3+b*x[[2]]^2+c*x[[2]]+d==f[[2]]
rov3=a*x[[3]]^3+b*x[[3]]^2+c*x[[3]]+d==f[[3]]
rov4=a*x[[4]]^3+b*x[[4]]^2+c*x[[4]]+d==f[[4]]

riesenie=Solve[{rov1,rov2,rov3,rov4},{a,b,c,d}]   (*Riesenie sustavy styroch rovnic.*)	  

Clear[x,f]

polynom[x_]=a*x^3+b*x^2+c*x+d

polynom[x]/.riesenie[[1]]

P[x_]=-0.721229+0.0203263*x+3.14059*x^2-1.28349*x^3

grpolynomu=Plot[P[x],{x,-2.3,3.397}]  

grafbody=ListPlot[data, PlotStyle->PointSize[0.02]]

Show[Out[56],Out[57]]	

(*2.)*)
P[-1.531]

(*3.)*)
Clear[x,f,t,a]

pol[t_]=InterpolatingPolynomial[data,t]//Expand

Clear[data,x,d,a,f,t]

data={{-2.3,31.462},{0.263,-0.522},{1.056,1.291},{3.397,-14.724}}

x=Transpose[data][[1]]
f=Transpose[data][[2]]

d[1,1]=(f[[2]]-f[[1]])/(x[[2]]-x[[1]])

d[1,2]=(f[[3]]-f[[2]])/(x[[3]]-x[[2]])

d[1,3]=(f[[4]]-f[[3]])/(x[[4]]-x[[3]])

d[2,1]=(d[1,2]-d[1,1])/(x[[3]]-x[[1]])

d[2,2]=(d[1,3]-d[1,2])/(x[[4]]-x[[2]])

d[3,1]=(d[2,2]-d[2,1])/(x[[4]]-x[[1]])

newtonvpred[t_]=31.462+d[1,1]*(t+2.3)+d[2,1]*((t+2.3)*(t-0.263))+d[3,1]*((t+2.3)*(t-0.263)*(t-1.056))//Expand

newtonvzad[h_]=-14.724+d[1,3]*(h-3.397)+d[2,2]*((h-3.397)*(h-1.056))+d[3,1]*((h-3.397)*(h-1.056)*(h-0.263))//Expand

(*Nezalezi.*)
