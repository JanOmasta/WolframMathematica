Do[x[i+1]=x[i]-f[x[i]]/f'[x[i]]//N;
 If[Abs[f[x[i+1]]]<tolerancia,Break[]],
 {i,0,pocetopakovani}]
