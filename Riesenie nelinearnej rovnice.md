```Mathematica

(*RIESENIE NELINEARNEJ ROVNICE*)

(*
ZADANIE:
Najdete korene nelinearnej rovnice -2x+log(x)+3=0 na internvale [-1,4].

1.) Kolko korenov nadobuda rovnica na danom intervale? Nacrtnite graf.
2.) Separujte najvacsi koren. Overte existenciu a jednoznacnost korena na vami zvolenom intervale.
3.) Metodou linearnej interpolacie (metodou secnic) najdite hodnotu najvacsieho korena s toleranciou |f(xn)|<10^-5. Uvedte iteracnu schemu a startovacie body.
4.) Kolko iteracii je potrebnych na vypocet korena? Vypiste ich.
5.) Vypocitajte odhad chyby poslednej iteracie korena.
*)

(*RIESENIE*)
(*1.)*)
f[x_]=-2*x+Log[x]+3
Plot[f[x],{x,-1,4}		

(*2.)*)
Plot[f[x],{x,1,3}]
f[1.5]*f[2.5]<0		
(*Vystup je True. Teda existuje koren a v okoli korena je funkcia spojita.*)

(*3.)*)
(*Volim startovacie body x0=1 a x1=1.5*)
Clear[f,x]
f[x_]=-2*x+Log[x]+3;
x[0]=1;
x[1]=1.5;
pocetopakovani=10;
tolerancia=10^(-5);
Do[x[i+1]=x[i]-f[x[i]]*(x[i]-x[i-1])/(f[x[i]]-f[x[i-1]])//N;
 If[Abs[x[i+1]-x[i]]<tolerancia,Break[]],
 {i,1,pocetopakovani}]
TableForm[Table[{i,NumberForm[x[i],10],Abs[x[i+1]-x[i]]},{i,0,pocetopakovani}],
 	TableHeadings->{None,{"i","xi","|xi+1-xi|"}},
 	TableSpacing->{1,5}]

(*4.)*)
(*Na vypocet korena je potrebnych 4 iteracie. Funkcia nadobuda koren priblizne v bode x=1.791527687. Priblizna hodnota korena na 16 desatinnych miest je:*)
NumberForm[x[4],16]

(*5.)*)
Plot[f'[x],{x,1,3}]	(*Prva derivacia funkcie f(x).*)
f'[3]//N
Abs[f[x[4]]]/f'[3]	(*Odhad chyby.*)