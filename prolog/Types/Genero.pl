
:- module(genero, [
    genero/1,               
    genero_string_atom/2    
]).


genero(rock).
genero(pop).
genero(eletronica).
genero(hiphop).
genero(rap).
genero(funk).
genero(mpb).
genero(sertanejo).
genero(forro).
genero(indie).
genero(pagode).


genero_string_atom("Rock",       rock).
genero_string_atom("Pop",        pop).
genero_string_atom("Eletronica", eletronica).
genero_string_atom("HipHop",     hiphop).
genero_string_atom("Rap",        rap).
genero_string_atom("Funk",       funk).
genero_string_atom("MPB",        mpb).
genero_string_atom("Sertanejo",  sertanejo).
genero_string_atom("Forro",      forro).
genero_string_atom("Indie",      indie).
genero_string_atom("Pagode",     pagode).