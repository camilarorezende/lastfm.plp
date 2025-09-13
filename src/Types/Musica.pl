
:- module(musica, [
    is_musica/1,           
    json_to_musica/2,      
    musica_to_json/2       
]).

:- use_module('genero').

is_musica(Dict) :-
    is_dict(Dict, musica),
    get_dict(genero, Dict, GeneroAtom),
    genero(GeneroAtom). 

json_to_musica(JsonDict, MusicaDict) :-
    get_dict(genero, JsonDict, GeneroString),
    genero_string_atom(GeneroString, GeneroAtom),
    MusicaDict = JsonDict.put(_{tag:musica, genero:GeneroAtom}).


musica_to_json(MusicaDict, JsonDict) :-
    get_dict(genero, MusicaDict, GeneroAtom),
    genero_string_atom(GeneroString, GeneroAtom),
    TempDict = MusicaDict.put(genero, GeneroString),
    del_dict(tag, TempDict, _, JsonDict). 
