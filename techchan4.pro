% 16B13041 SyogoFujita 2018/2/2

%Program Code
techchan(QueryStr,_):-
    concat_atom(Query, ' ', QueryStr),
    recognize(Query, Slots, Flags),!,
    categorize(Slots, Flags).
techchan(_,_):-
    Answer=[nani,wo,itteiruka,'wakaranaiya.','gomenne.'],
    concat_atom(['\ntech chan:\n'|Answer], ' ', AnswerStr),
    putAA(tech),
    writeln(AnswerStr),!.

categorize([transport,From,To,US,Nori,PN,Keyu], Flags):-
    putAA(tech),writeln('\ntech chan:'),
    (nonvar(US);US is 1),(nonvar(Nori);Nori is 0),(nonvar(PN);PN is 3),
    transport_answer([transport,From,To,US,Nori,PN,Keyu], Flags),
    allCostDelete(),!.
categorize([conv|Slots], Flags):-
    putAA(tech),writeln('\ntech chan:'),
    defo_answer([conv|Slots], Flags),!.
categorize([tequest|_], _):-
    tequest(1,20).

defo_answer(Slots, Flags):-
    think(Slots, Flags, Answer),concat_atom(['\n'|Answer], ' ', AnswerStr),writeln(AnswerStr).

transport_answer(Slots, Flags):-
    findall(Answer,think(Slots, Flags, Answer),AnswerList),
    findall(Cost,cost(Cost,_),CostList),quickSort(CostList,SCostList),pickOne(Slots,5,PutNum),
    pickPart(SCostList,PutNum,SSCostList),transport_show(AnswerList,SSCostList).

transport_show(AnswerList,[Cost|SSCostList]):-
    cost(Cost,Answer),concat_atom(['\n'|Answer], ' ', AnswerStr),writeln(AnswerStr),
    transport_show(AnswerList,SSCostList).
transport_show(_,[]).

% Basic concepts

station(chuorinkan).
station(suzukakedai).
station(nagatsuta).
station(futakotamagawa).
station(kikuna).
station(jiyugaoka).
station(oookayama).
station(yokohama).
station(shibuya).
station(ooimachi).
station(mizonoguchi).
station(hiyoshi).
station(nakameguro).
station(hatanodai).
station(gotanda).
station(ginza).
station(meguro).
station(midorigaoka).
station(ueno).
station(akihabara).
station(tokyo).
station(shinyokohama).
station(shinagawa).

train(denentoshisen).
train(yokohamasen).
train(toyokosen).
train(ooimachisen).
train(ginzasen).
train(yamanotesen).
train(megurosen).
train(ikegamisen).
train(blueline).
train(tokaido_sanyo_shinkansen).
train(tohoku_shinkansen).

% Reacting words
reactwords([kawaii],[terechauyo,'arigato.']).
reactwords([shukudai, oshiete],[jibunde, sinaito, 'damedayo.']).
reactwords([msm],[zenninno, mishima, gakuchou, no ,'kotodayone.']).
reactwords([cocoro],[taiwan_mazesoba, cocoro, ha ,'oishiiyo.',shio,mazesoba,ga,'sukinanda.']).
reactwords([kara],[eki,ga,toroku,saretenaika,',',nyuryoku_keishiki,ga,'okashiiyo.']).
reactwords([ookayama],[ookayama,zyanaiyo,oookayama,'dayo.']).
reactwords([oookayama],[boku,ha,tokodai,oookayama,campaus,ni,sunde,'iruyo.']).
reactwords([kimino,na,ha],[konnichiwa, boku, tokodai, no, techchan, 'desu.']).
reactwords([shokudo],[ichishoku, no, toritendon, ga, 'oishiiyo.']).

% knowledge about language to understand the query(automaton)
%% delta(fromState, arcSymbol, toState, slot, flag)
%% [transport,from,to,useShinkansen,Norikae,putNum,Keiyu]
%% [conv,arg1,arg2(optional),arg3(optional)]
%% [deito,place,launch]

delta(0, W, 1, [transport, W,_,_,_,_,_],[1,1,0]):-station(W).
delta(0, W, 1, [transport, _,W,_,_,_,_],[1,0,1]):-station(W).

delta(1, kara, 2, [transport, _,_,_,_,_,_], _).
delta(1, made, 3, [transport, _,_,_,_,_,_], _).

delta(2, W, 4, [transport, _,W,_,_,_,_], [1,0,1]):-station(W).
delta(2, W, 10, [transport, _,W,_,_,_,_], [1,0,1]):-station(W).

delta(3, W, 4, [transport, W,_,_,_,_,_], [1,1,0]):-station(W).
delta(3, W, 10, [transport, W,_,_,_,_,_], [1,1,0]):-station(W).

delta(4, I, 4, [transport, _,_,_,_,N,_], _):-atom_number(I,N).
delta(4, I, 10, [transport, _,_,_,_,N,_], _):-atom_number(I,N).

delta(4, norikae, 4, [transport, _,_,_,1,_,_], _).
delta(4, norikae, 10, [transport, _,_,_,1,_,_], _).

delta(4, shinkansen, 5, [transport, _,_,_,_,_,_], _).
delta(5, wo, 6, [transport, _,_,_,_,_,_], _).
delta(6, tsukawazuni, 4, [transport, _,_,0,_,_,_], _).
delta(6, tsukawazuni, 10, [transport, _,_,0,_,_,_], _).

delta(4, W, 4, [transport, _,_,_,_,_,W], _):-station(W).
delta(4, W, 10, [transport, _,_,_,_,_,W], _):-station(W).

delta(0, tequest, 10, [tequest, _, _], _).

delta(0, X, 1, [conv, X, _,_], _):-reactwords(Z,_),pickOne(Z,0,X),pickOne(Z,1,_).
delta(0, X, 10, [conv, X, _,_], _):-reactwords(Z,_),pickOne(Z,0,X),not(pickOne(Z,1,_)).
delta(1, X, 2, [conv, _ ,X,_], _):-reactwords(Z,_),pickOne(Z,1,X),pickOne(Z,2,_).
delta(1, X, 10, [conv, _ ,X,_], _):-reactwords(Z,_),pickOne(Z,1,X),not(pickOne(Z,2,_)).
delta(2, X, 10, [conv, _ ,_,X], _):-reactwords(Z,_),pickOne(Z,2,X).

delta(4, _, 4, [transport|_], _).
delta(4, _, 10, [transport|_], _).
delta(0, _, 0, _ ,_).
delta(10, _, 10, _ ,_).

goal(10).

% knowledge about transportation to answer the question (directed graph)
%% link(transportation, fromStation, toStation)

link(denentoshisen, chuorinkan, suzukakedai,5).
link(denentoshisen, suzukakedai, nagatsuta,4).
link(denentoshisen, nagatsuta, mizonoguchi,16).
link(denentoshisen, mizonoguchi,futakotamagawa,2).
link(denentoshisen, futakotamagawa, shibuya,13).

link(yokohamasen, nagatsuta, kikuna,17).
link(yokohamasen, kikuna, yokohama,12).
link(yokohamasen, kikuna, shinyokohama,14).

link(toyokosen, yokohama,kikuna,10).
link(toyokosen, kikuna, hiyoshi,6).
link(toyokosen, hiyoshi,jiyugaoka,13).
link(toyokosen, jiyugaoka ,nakameguro,8).
link(toyokosen, nakameguro,shibuya,5).

link(ooimachisen,mizonoguchi, futakotamagawa,2).
link(ooimachisen,futakotamagawa, jiyugaoka,8).
link(ooimachisen, jiyugaoka, midorigaoka,2).
link(ooimachisen, midorigaoka, oookayama,1).
link(ooimachisen, oookayama,hatanodai,3).
link(ooimachisen, hatanodai,ooimachi,7).

link(ikegamisen,hatanodai,gotanda,7).

link(ginzasen,shibuya,ginza,16).
link(ginzasen,ginza,ueno,12).

link(yamanotesen,ueno,akihabara,3).
link(yamanotesen,akihabara,tokyo,4).
link(yamanotesen,tokyo,shinagawa,11).
link(yamanotesen,shinagawa,gotanda,4).
link(yamanotesen,gotanda,meguro,2).
link(yamanotesen,meguro,shibuya,5).
link(yamanotesen,shibuya,ueno,32).

link(blueline,yokohama,shinyokohama,11).

shinlink(tokaido_sanyo_shinkansen,tokyo,shinagwa,6).
shinlink(tokaido_sanyo_shinkansen,shinagwa,shinyokohama,11).

shinlink(tohoku_shinkansen,tokyo,ueno,5).

% Assume the links are bi-directional (make it undirected)
% US is Use Shinkansen
bilink(X, Y, Z, US,Time):-link(X, Y, Z,Time);(US is 1,shinlink(X, Y, Z,Time)).
bilink(X, Y, Z, US,Time):-link(X, Z, Y,Time);(US is 1,shinlink(X, Z, Y,Time)).

% recognize (Slot filling based on automaton)
recognize(Ss, Slots, Flags):-goal(G),path(0, Ss, G, Slots, [0,0,0], Flags).
path(X,[S],Y, Slots, Flags, NewFlags):-delta(X, S, Y, Slots, Flags1), listOr(Flags, Flags1, NewFlags).
path(X,[S|Ss],Y, Slots, Flags, NewFlags)
:-delta(X, S, Z, Slots, Flags1), listOr(Flags, Flags1, NewFlags1), path(Z, Ss, Y, Slots, NewFlags1, NewFlags).

% think to answer
think([transport, From, To, US, 1,_,Keyu], [1,1,1], Answer):-
    nonvar(Keyu),reachBy(From, Keyu, _, Transports1,US,1,ACost,Time1),
    reachBy(Keyu, To, _, Transports2,US,1,BCost,Time2),myAppend(Transports1,Transports2,ATransports_),
    myAppend(ATransports_,[[To,you_arrived]],ATransports),transports2utterance(ATransports, Answer_),
    CCost is ACost+BCost,Time3 is Time1+Time2,myAppend(Answer_,[Time3,hun,'kakaruyo.'],Answer),setCost(CCost,Answer).
think([transport, From, To, US, 0,_,Keyu], [1,1,1], Answer):-
    nonvar(Keyu),reachBy(From, Keyu, _, Transports1,US,0,ACost,_),
    reachBy(Keyu, To, _, Transports2,US,0,BCost,_),myAppend(Transports1,Transports2,ATransports_),
    myAppend(ATransports_,[[To,you_arrived]],ATransports),transports2utterance(ATransports, Answer_),
    CCost is ACost+BCost,myAppend(Answer_,[CCost,hun,'kakaruyo.'],Answer),setCost(CCost,Answer).

think([transport, From, To, US, 1,_,Keyu], [1,1,1], Answer):-
    var(Keyu),reachBy(From, To, _, Transports_,US,1,ACost,Time),myAppend(Transports_,[[To,you_arrived]],Transports),
    transports2utterance(Transports, Answer_),myAppend(Answer_,[Time,hun,'kakaruyo.'],Answer),setCost(ACost,Answer).
think([transport, From, To, US, 0,_,Keyu], [1,1,1], Answer):-
    var(Keyu),reachBy(From, To, _, Transports_,US,0,ACost,_),myAppend(Transports_,[[To,you_arrived]],Transports),
    transports2utterance(Transports, Answer_),myAppend(Answer_,[ACost,hun,'kakaruyo.'],Answer),setCost(ACost,Answer).

think([tequest, _, _],_,_).

think([conv, W, _,_],_, Answer):-reactwords([W], Answer).
think([conv, W, Z._],_, Answer):-reactwords([W,Z], Answer).
think([conv, W, Z,P],_, Answer):-reactwords([W,Z,P], Answer).

% think engine about transportation (graph search)
% search how to move
reachBy(From, To, Stations, Transports, US,Nori,ACost,AT):-
    reachBySub(From, To, [], Stations, [], Transports,US,Nori,0,ACost,0,AT).
reachBySub(P, P, Ss, Stations, Y, Transports,_,_,Cost,Cost,T,T):-
    not(myMember(P, Ss)),myReverse([P|Ss], Stations), myReverse(Y, Transports).

reachBySub(From, To, [], Stations, [], Transports,US,1,Cost,ACost,T,AT):-
    bilink(Tr, From, Y,US,Time),NCost is Cost + 1,NT is T+Time,
    reachBySub(Y, To, [From], Stations, [[From,Tr]], Transports,US,1,NCost,ACost,NT,AT).
reachBySub(From, To, [], Stations, [], Transports,US,0,Cost,ACost,_,_):-
    bilink(Tr, From, Y,US,Time),NCost is Cost + Time,
    reachBySub(Y, To, [From], Stations, [[From,Tr]], Transports,US,0,NCost,ACost,_,_).

reachBySub(From, To, Ss, Stations, [[Fr,Tr]|FrTrs], Transports,US,1,Cost,ACost,T,AT):-
    not(myMember(From, Ss)),bilink(Tr, From, Y,US,Time),NT is T+Time,
    reachBySub(Y, To, [From|Ss], Stations, [[Fr,Tr]|FrTrs], Transports,US,1,Cost,ACost,NT,AT).
reachBySub(From, To, Ss, Stations, [[Fr,Tr]|FrTrs], Transports,US,0,Cost,ACost,_,_):-
    not(myMember(From, Ss)),bilink(Tr, From, Y,US,Time),NCost is Cost + Time,
    reachBySub(Y, To, [From|Ss], Stations, [[Fr,Tr]|FrTrs], Transports,US,0,NCost,ACost,_,_).

reachBySub(From, To, Ss, Stations, [[Fr,Tr]|FrTrs], Transports,US,1,Cost,ACost,T,AT):-
    not(myMember(From, Ss)),bilink(Tr1, From, Y,US,Time),not(Tr==Tr1),NCost is Cost + 1,NT is T+Time,
    reachBySub(Y, To, [From|Ss], Stations, [[From,Tr1],[Fr,Tr]|FrTrs], Transports,US,1,NCost,ACost,NT,AT).
reachBySub(From, To, Ss, Stations, [[Fr,Tr]|FrTrs], Transports,US,0,Cost,ACost,_,_):-
    not(myMember(From, Ss)),bilink(Tr1, From, Y,US,Time),not(Tr==Tr1),NCost is Cost + Time,
    reachBySub(Y, To, [From|Ss], Stations, [[From,Tr1],[Fr,Tr]|FrTrs], Transports,US,0,NCost,ACost,_,_).

% save the cost of transfer.
setCost(C,W):-
    nonvar(C),NC is C+(random(10000)/10001),nonvar(W),retract(cost(NC,_)),!, asserta(cost(NC,W)).
setCost(C,W):-
    nonvar(C),NC is C+(random(10000)/10001),nonvar(W),asserta(cost(NC,W)).
allCostDelete():-abolish(cost,2).

% form answer utterance
transports2utterance([[X, you_arrived]], [X, ni, 'tsukeruyo.']).
transports2utterance([[From, By]|X], [From, kara, By, ninotte|Utterance]):-transports2utterance(X, Utterance).

% some list tools
myAppend([], X, X).
myAppend([X | L], Y, [X | Z]) :- myAppend(L, Y, Z).

myReverse([], []).
myReverse([X | L], Y):-myReverse(L, Z), myAppend(Z, [X], Y).

myMember(X, [X | _]).
myMember(X, [_ | L]):-myMember(X, L).

listOr([],[],[]).
listOr([0|X], [0|Y], [0|Z]):-!,listOr(X, Y, Z).
listOr([_|X], [_|Y], [1|Z]):-listOr(X, Y, Z).

pickOne([X|_],0,X).
pickOne([_|L],I,A):-J is I-1,pickOne(L,J,A).

pickPart([],_,[]).
pickPart(_,0,[]).
pickPart([A|L],X,[A|As]):-Y is X-1,pickPart(L,Y,As).

quickSort([X | Xs], Ys)
    :-partition(Xs, X, Ls, Bs),quickSort(Ls, SLs),quickSort(Bs, SBs),myAppend(SLs, [X | SBs], Ys).
quickSort([], []).

partition([X | Xs], Y, [X | Ls], Bs):-X =< Y, partition(Xs, Y, Ls, Bs).
partition([X | Xs], Y, Ls, [X | Bs]):-X > Y, partition(Xs, Y, Ls, Bs).
partition([], _, [], []).


% rpg
skill(Level,techchan,0,attack,D,0):-D is 5+random(Level).
skill(Level,techchan,1,cure,0,-D):-D is 5+random(Level).
skill(Level,techchan,2,super_attack,D,5):-D is 2*(5+random(Level)).

enemy(slime,N):-N is random(2).
skill(slime,0,attack,3,0).
skill(slime,1,cure,0,-2).
levelup(slime,Level,NLevel,RHp,NHp):-NLevel is Level+1,NHp is RHp + 5.

enemy(witch,N):-N is random(2).
skill(witch,0,magic,5,0).
skill(witch,1,cure_attack,4,-4).
levelup(witch,Level,NLevel,RHp,NHp):-NLevel is Level+2,NHp is RHp + 10.



battle(_,MyHp,EneHp,MyHp):-EneHp<1.
battle(Enemy,MyHp,EneHp,RestHp,Level):-
    EneHp>0,
    format('\ntech chan: ~d, ~a:~d [0:attack,1:cure,2:super_attack]\n\n[select one action]\n\n', [MyHp, Enemy, EneHp]),
    commandGet(N),
    skill(Level,techchan,N,Com1,Dam1,RDam1),
    format('\n~d: tech chan no ~a! ~a ni ~d damage\n', [N, Com1, Enemy,Dam1]),
    NEneHp_ is EneHp - Dam1,
    NMyHp_ is MyHp - RDam1,
    (NMyHp_ < 1 -> format('\ntech chan ha taoreta!'),1 is 0 ;
        (NEneHp_ < 1 -> format('\n~a wo taoshita!',[Enemy]),battle(Enemy,NMyHp_,NEneHp_,RestHp);
            enemy(Enemy,EN),
            skill(Enemy,EN,Com2,Dam2,RDam2),
            format('\n~a no ~a! tech chan ni ~d damage\n', [Enemy,Com2,Dam2]),
            NEneHp is NEneHp_ - RDam2,
            NMyHp is NMyHp_-Dam2,
            (NMyHp < 1 -> format('\ntech chan ha taoreta!'),1 is 0 ;
                (NEneHp < 1 -> format('\n~a wo taoshita!',[Enemy]),battle(Enemy,NMyHp,NEneHp,RestHp,Level)
                    ; battle(Enemy,NMyHp,NEneHp,RestHp,Level))))).

commandGet(N):-repeat,get(C),N is C-48,N < 3,N > -1,!.

allBattle(Enemy,MyHp,EneHp,RestHp,Level,NLevel):-
    putAA(Enemy),
    format('\n~a ga arawareta!\n',[Enemy]),
    battle(Enemy,MyHp,EneHp,RestHp_,Level),
    levelup(slime,Level,NLevel,RestHp_,RestHp),
    format("\n\ntech chan no level ga ~d ni natta!\n",[NLevel]),
    format("\ntech chan no hp ga ~d ni natta.\n",[RestHp]).

story(0):-
    putAA(title),
    writeln('\n\n[press Enter Key]\n'),
    get0(_),
    putAA(king),
    writeln('\nking:\n techchan yo. tokodai wo monster kara sukuttekure.\n\n[press Enter Key]\n'),
    get0(_),
    putAA(tech),
    writeln('\ntechchan:\n wakatta. makasete!\n\n[press Enter Key]\n'),
    get0(_),
    writeln('\n\nnarrator:\n techchan no bouken ga hazimatta.\n\n[press Enter Key]\n').

story(1):-
    putAA(tech),
    writeln('\n\ntechchan:\n saisho no tekiha yowakatta kedo, tsugi ha donna teki ga kurunokana.\n\n[press Enter Key]\n'),
    get0(_),
    putAA(minihatena),
    writeln('\n???:\n watashi ha katsu te toukoudaisei datta.\n\n[press Enter Key]\n'),
    get0(_),
    putAA(minihatena),
    writeln('\n???:\nshikashi black kenkyuu shitsu ni haitta seide zinsei wo kuruwasarera. kanozyo mo dekinai mama 30 sai wo mukaete shimatta noda.\n\n[press Enter Key]\n'),
    get0(_),
    putAA(minihatena),
    writeln('\n???:\n tech chan. kimi ni urami ha naiga toukoudai no kata wo motsu toiunonara kietemorau.\n\n[press Enter Key]\n').

tequest(Level,Hp):-
    story(0),
    get0(_),
    (allBattle(slime,Hp,10,Hp1,Level,Level1) -> 1 is 1;
        writeln('\n\ntech chan no bouken ha owatta ...\n')),
    get0(_),
    story(1),
    (allBattle(witch,Hp1,15,Hp2,Level1,Level2) -> writeln("\n\ntoukodai ni heiwaga modotta.\n");
        writeln('\n\ntech chan no bouken ha owatta ...\n')).

% put ASCII art
putAA(slime):-writeln('\n　　　 ∩\n　　　ノヽ\n　　／　　＼\n　 / (･)(･)∧\n　｜(ヽ＿ノ)｜\n　 ＼二二二／').
putAA(tech):-writeln('\n`.`     ``.   .-dWHHQkWWWkkQQQkZZUS.. .((-..  ``.\n           .(kWHWyyQWHWWkkyZZyNHkZZZXS...._<<.\n    ```  .JHQkWpWMWQkXZZuZZWHkhHkHkZZZOu&<<___\n       JNWHZZZuZuZuZZZuZZuZZuZUHkHkHkZ&++?G+. ...\n```   (WHZZuZZZuZZZuZZuZZuZuZZZZXHkHWkZWkwZZZkkY~\n     .WSZZuZuZZZuZZuZZuZZZZZuZuZZZXHKXHR?kZuuuuqS\n  ...WZZZuZZZ0VT777<<<<<+(+nJ+uXn+dZWky@?zHZZXXdX\nXUZZXQQX977<~~~~((JuwuZZZZZZZZWkZZZZXkWkb?dHZQ81HZ\nV"=(Y7<~~~~-(+wuZZuZXRZuZZXWWWUZHkuZZZHWbzKZX3?dSZ\n .t~~~_(JXXZZuZuZZZZXkkuZuZuZZZuXkZZuZHNdWKugHWWf\n J~_(duZK9YWZZZZuZuZuX2__<??CXWXY"UZZZXkyZWHkWpWW\n $JZZZZXI-?!?WZZZuZZZuXo`.J94JZVWHe4ZZZHH#XmmQQAd\n.HZZZuZXr_`` .(4XZZuXZZXL_E.RrtrrdJbWZZXHWKpppppW\n,SuZKZuZr ``.ZWMMNXZZHWkXh. SrtO+.<%OZuX.WpfpHe.\n..HZXZZZZh_`J$JrtrU/(7XXe. ``.4wwwZ!`JZZWHpppfppWH\n HXHZuZuXO,d;jrri...```_7!``` .-~~_`JZXNfpWY"""7"\n.dWZZZZXZb(nT,4yrwZ ``````````````.(#ZdHYYUW. ```\nJ(HZuZuWkdHah`....`````.``.`.```.,YWSXBWHn,```\nI(XZZuZdWHWR+__` ``.``. `.7```.J:iJWVG.   Tk,`\n0duZWZZPHppMHh,.````````   .-3+7!(Y^..(1 `  ?X+..\n_JUZZKZX`.WfWTWpb?77<<?dI<?!(?`  _.(!`.4J{.    (1Z\n').
putAA(king):-writeln('\n``__.____........................~..~.~..~~~~~~~_~\n..................._((--(--(----..-.....--__...__~\n~~..~.........-.-JwXwywwXAXWWksw+_..~..~..~~.~~~~.\n.............(JdXWHHWBYYYC<<<<?7OC_......~....~..~\n.~.~~~~~....(dWWWUOz<:___~~~~~~~(++<_.~.~.~....~~.\n....... ....XHHUrtz=?>>;:;::~~~~_(1wA+..-...--_...\n........`.-udWXrrrOz>;:<;;_~~~~_~(+zOX_..~~~~~~~~~\n.......`. jHNkXuwuQma&z+<+zzwXwz-:<XQf_.~~~.~~~~~~\n~.........-@WNXXUUZllIOdIj0wAa+-((JdMR_~...~..~~.~\n-...... .` dMHV0OUHWSVXHCjVO0VIz>;:j0Z__________..\n..~.......-XWkZzwvI1<+z0>~<<><:<~~:jI<..~~~~~~~~~~\n..........`?WHOOOz1?+uVXz__<zO+_(_::<<..~..~~~~.~~\n~...........W9OXwXOzdHWHSVvC>zXO=z<:_~.~~.~~.~~~~~\n   ....`.```(ZwXyXwXXXXwzz+++zzZ??>;<!___________~\n.~.~..........wZyyyZUHMkyyzZ6<zI??1=!....~~.~...~.\n~..~..........jfWVWXzXUUwOI<::<====>........~~~~~~\n.~...~~.......`?pVppkXO==><;<+1=z?u,..............\n ```````      `-JWppbqkkAwwOttl=>:-bn.-....~..~~.~\n......`..`. ..(W#?UppVWyuvrtllzv! Jbppffk&-.......\n..```. ..gW@Hg@@#:<?TUwVVOOtZ!`  .HkbpppfppWWyyXA&\n ..(JXHgggmgg@@MNz;;:_.?11v^     JgHkbppbpppbpppWW\nWWHmmmmgmNHg@M@HHr~~~:_ .&.      dkkkkHWHHppppbkqk\ng@@gggggg@MM@@@@@]...__JMHSe.`  .HHkkkkbbpppppbbkq').
putAA(title):-writeln('\n\n\n  %@#@#@#%$           %&%&%&                                ..\n    JM3            .MD!   JMF                             .dM"\n   JM^  &(X""WNJ  JM^    .M#`Jg~   .ge ..JY""Na, ..yYT0 QAMM8Z"\n  JM3  MM!   .MF dM^    .M#`JM|   .MM!.M@    JM|.MN,.   .MM!\n gM?  MM!"""""" JM|    .M@ (M%   .MM!.MF"""""""  .7MM% .MM!\ndM^   THkJzT"`  ?WN+JdT"!  ?WH&&7T"` ,"B&JZ"= .S%&d9^  MH!\n                  "!?TM%\n\n                               `').
putAA(witch):-writeln('\n　⊂ｱﾍ_へ_ﾉﾏつ\n　　Y〈ｿﾘ〉ｿ\n　　ﾊ ヽﾉ ﾉ\n　 (_ミ彡ｲ)').
putAA(minihatena):-writeln('\n   ..\n.MM"WMN,\n#&5   .MF\n   .MM"\n  %MF\n\n  ?Mi').
putAA(hatena):-writeln('\n\n                ...+NNNNNNNNNNgJ..\n             .JMMMMMMMMMMMMMMMMMMMNa,\n           .MMMMMMMMMMMMMMMMMMMMMMMMMN,\n          .MMMMMMMMMM8"7???"TMMMMMMMMMMx\n         (MMMMMMMM@!          .WMMMMMMMM,\n        .MMMMMMMMF              WMMMMMMM]\n        dMMMMMMM#               JMMMMMMMF\n                `              .MMMMMMMM|\n                             .&MMMMMMMMF\n                          ..dMMMMMMMMMD\n                        .dMMMMMMMMMMB"\n                      .MMMMMMMMMMM"`\n                     jMMMMMMMMM"!\n                    .MMMMMMMM=\n                    MMMMMMM#\n                    MMMMMMM|\n                    MMMMMMM)\n                   ."""""""^\n\n                   .........\n                   (MMMMMMM#\n                   (MMMMMMM#\n                   (MMMMMMM#\n                   JMMMMMMM#').
