```Mathematica

(*HLADANIE MAXIMA FUNKCIE JEDNEJ PREMENNEJ*)

(*
ZADANIE:
Numerickymi metodami najdite maximum funkcie f(x)=3-1.2x+ln(-2+x) na internvale [2,5].

1.) Urobte analyzu ulohy, zistite, v ktorom intervale sa nachadza bod xmax, v ktorom zadana funkcia dosahuje maximum. Na tomto intervale overte splnenie podmienky unimodalnosti.
2.) Maximum funkcie najdite metodou Golden Ratio Serch. V ktorom intervale sa nachadza xmax funkcie s presnostou 10^-4 vzhladom na dlzku intervalu [c,d]?
3.) Uvedte iteracnu schemu a startovaci interval. Zdovodnite vas vyber startovacieho intervalu. Kolko iteracii je potrebnych na vypocet xmax? Aku hodnotu maxima funkcia nadobuda?
*)

(*RIESENIE*)
(*1.)*)
f[x_]=-(3-1.2*x+Log[-2+x])
Plot[f[x],{x,2,5}
f'[x]
Plot[f'[x],{x,2,5}]
(*Z grafu vidime, ze na celom intervale bude mat funkcia jedno minimum. Na celom intervale je funkcia unimodalna a preto nie je potrebne interval zmensovat. Funkcia je unimodalna, ak jej derivacia na danom intervale ma jediny koren.*)		

(*2.)*)
f[x_]=-(3-1.2*x+Log[-2+x]);
pocetopakovani=25;
tolerancia=10^(-4);
a[0]=2.5;
b[0]=3;
r=(Sqrt[5]-1)/2 //N; 

Do[
 c[n]=a[n]+(1- r)*(b[n]-a[n]); 
 d[n]=b[n]-(1-r)*(b[n]-a[n]); 
 
 If[f[c[n]]<f[d[n]],
  
  	b[n+1]=d[n];
  	d[n+1]=c[n];
  	c[n+1]=a[n]+(1-r)*(b[n]-a[n]);
  	a[n+1]=a[n],
  	a[n+1]=c[n];
  	c[n+1]=d[n];
  	d[n+1]=b[n]-(1-r)*(b[n]-a[n]);
  	b[n+1]=b[n]];
 If[Abs[b[n+1]-a[n+1]]<tolerancia, Break[]], 
 {n,0,pocetopakovani}]

TableForm[Table[{n,NumberForm[a[n],10],
		NumberForm[c[n],10],NumberForm[d[n],10],
		NumberForm[b[n],10],NumberForm[f[c[n]],10],
		NumberForm[f[d[n]],10]},{n,0,pocetopakovani}],
 	TableHeadings->{None,{"n","an","cn","dn","bn",
				"f(cn)","f(dn)"}},
 	TableSpacing->{1,5}]

(*3.)*)
(*Po 20.iteracii sa interval zuzil na [a18, b18]= 2.833268834 ; 2.833355369. Minimum som nasiel v bode 2.833268834 a jeho hodnota je 0.5823215569. Maximum je teda v bode 2.833268834 a jeho hodnota je  -0.5823215569.*)

(*Povodna funkcia:*)
g[x_]=3-1.2*x+Log[-2+x]
Plot[g[x],{x,2,5}]