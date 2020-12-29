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
(*1.*)
f[x_]=-2*x+Log[x]+3
Plot[f[x],{x,-1,4}]
