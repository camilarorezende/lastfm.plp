
:- module(scrobble, [
    is_scrobble/1,          
    json_to_scrobble/2,     
    scrobble_to_json/2     
]).

:- use_module('musica').

is_scrobble(Dict) :-
    is_dict(Dict, scrobble),
    get_dict(musica, Dict, MusicaDict),
    is_musica(MusicaDict).


json_to_scrobble(JsonDict, ScrobbleDict) :-
    get_dict(musica, JsonDict, RawMusicaDict),
    json_to_musica(RawMusicaDict, TypedMusicaDict), 
    ScrobbleDict = JsonDict.put(_{tag:scrobble, musica:TypedMusicaDict}).


scrobble_to_json(ScrobbleDict, JsonDict) :-
    get_dict(musica, ScrobbleDict, TypedMusicaDict),
    musica_to_json(TypedMusicaDict, RawMusicaDict), 
    TempJsonDict = ScrobbleDict.put(musica, RawMusicaDict),
    del_dict(tag, TempJsonDict, _, JsonDict).

