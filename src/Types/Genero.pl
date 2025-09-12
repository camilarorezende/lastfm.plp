:- module(genero, [genero/1, generos/1]).

genero(rock).
genero(pop).
genero(eletronica).
genero(classico).
genero(hiphop).
genero(rap).
genero(funk).
genero(mpb).
genero(sertanejo).
genero(forro).
genero(indie).
genero(pagode).
genero(reggae).

generos(Lista) :- findall(G, genero(G), Lista).
