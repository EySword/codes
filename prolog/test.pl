test(Sig):-findall(sym(Sym1,_Artiy,_Used1),(between(1,5,I),atomic_list_concat(['name','_',I],Sym1)),Sig).

test2(Sym1) :- between(1,5,I),atomic_list_concat(['name','_',I],Sym1).