
% Programa Skyline.pl
% David Pastor Sanz
% Práctica de Teoría de los Lenguajes de Programación
% Curso 2015-2016

% Predicado principal. Recibe una lista de edificios y devuelve una lista de coordenadas
% que describen la línea de horizonte que dibuja dicha lista de edificios de entrada
resuelveSkyline([],[]) :- !.
resuelveSkyline([X|[]],S) :- edificioAskyline(X,S), !.
resuelveSkyline(R,D) :- divide(R,L1,L2),
                        resuelveSkyline(L1,R1),
			resuelveSkyline(L2,R2),
			combina(R1,R2,D).

% Recibe un único edifiio y devuelve su línea de horizonte
edificioAskyline(ed(X1,X2,H),[c(X1,H),c(X2,0)]) :- X1 < X2, H > 0.

% Divide la lista metida como parámetro en dos mitades
divide([],[],[]) :- !.
divide([ed(X1,X2,H)],[ed(X1,X2,H)|[]],[]) :- !.
divide([ed(X1,X2,H1),ed(Y1,Y2,H2)],[ed(X1,X2,H1)|[]],[ed(Y1,Y2,H2)|[]]) :- !.
divide([ed(X1,X2,H1),ed(Y1,Y2,H2)|R],[ed(X1,X2,H1)|A],[ed(Y1,Y2,H2)|B]) :- divide(R,A,B).

% Combina las soluciones parciales de los subproblemas
combina(L1,L2,D) :- combinaAux(L1,L2,D,0,0,0), !.
combinaAux([],[],[],_,_,_) :- !.
combinaAux([c(X,HX)|R],[],[c(X,HX)|R],_,_,LH) :- HX =\= LH, !.
combinaAux([c(X,HX)|R],[],R,_,_,LH) :- HX =:= LH, !.
combinaAux([],[c(Y,HY)|R],[c(Y,HY)|R],_,_,LH) :- HY =\= LH, !.
combinaAux([],[c(Y,HY)|R],R,_,_,LH) :- HY =:= LH, !.
combinaAux([c(X,H1)|R1],[c(Y,H2)|R2],[c(X,A)|D],_,_,LH) :- X =:= Y,
							   max(H1,H2,A),
							   A =\= LH,
							   combinaAux(R1,R2,D,H1,H2,A).
combinaAux([c(X,H1)|R1],[c(Y,H2)|R2],D,_,_,LH) :- X =:= Y,
                                                  max(H1,H2,A),
						  A =:= LH,
						  combinaAux(R1,R2,D,H1,H2,A).
combinaAux([c(X,H1)|R1],[c(Y,H2)|R2],[c(X,A)|D],_,PH2,LH) :- X < Y,
                                                             max(H1,PH2,A),
							     A =\= LH,
						             combinaAux(R1,[c(Y,H2)|R2],D,H1,PH2,A).
combinaAux([c(X,H1)|R1],[c(Y,H2)|R2],D,_,PH2,LH) :- X < Y,
                                                    max(H1,PH2,A),
						    A =:= LH,
						    combinaAux(R1,[c(Y,H2)|R2],D,H1,PH2,A).
combinaAux([c(X,H1)|R1],[c(Y,H2)|R2],[c(Y,A)|D],PH1,_,LH) :- X > Y,
                                                             max(PH1,H2,A),
							     A =\= LH,
                                                             combinaAux([c(X,H1)|R1],R2,D,PH1,H2,A).
combinaAux([c(X,H1)|R1],[c(Y,H2)|R2],D,PH1,_,LH) :- X > Y, max(PH1,H2,A), A =:= LH,
                                                    combinaAux([c(X,H1)|R1],R2,D,PH1,H2,A).
						    
% Predicado auxiliar que calcula cual de los dos parámetros de entrada es mayor
max(A,B,C) :- A >= B, C is A, !.
max(A,B,C) :- A < B,  C is B, !.

% Devuelve una cadena de carácteres que simbolizan un skyline
dibujaSkyline(L) :- dibujaSkyline(L,D), write(D).
dibujaSkyline(L,D) :- maxh(L,M), listh(L,LH), dibujaSkylineAux(LH,M,LH,A), atomics_to_string(A,D), !.
dibujaSkylineAux([],0,_,['-'|[]]) :- !.
dibujaSkylineAux([_|R],0,_,['-'|S]) :- dibujaSkylineAux(R,0,_,S).
dibujaSkylineAux([],H,LH,['\n'|S]) :- H1 is H-1,
                                      dibujaSkylineAux(LH,H1,LH,S).
dibujaSkylineAux([X|R],H,LH,['*'|S]) :- X >= H, !,
                                         dibujaSkylineAux(R,H,LH,S).
dibujaSkylineAux([X|R],H,LH,[' '|S]) :- X < H, !,
                                         dibujaSkylineAux(R,H,LH,S).

% Devuelve una lista con todas las alturas
listh([],[]) :- !.
listh([_|[]],[]) :- !.
listh([c(X1,H1),c(X2,H2)|R],L) :- X is X2-X1,
				  listhAux([c(X2,H2)|R],X,H1,L2), !,
				  takeAndRepeat(X1,0,L1),
				  concatenar(L1,L2,L).
listhAux([_|[]],1,H,[H|[]]) :- !.
listhAux([c(X1,H1),c(X2,H2)|R],0,_,L) :- X is X2-X1,
                                         listhAux([c(X2,H2)|R],X,H1,L).
listhAux(R,X,H,[H|L]) :- X1 is X-1,
			 listhAux(R,X1,H,L).
				  
% Concatena dos listas
concatenar([],L,L) :- !.
concatenar([X|R1],L,[X|R2]) :- concatenar(R1,L,R2).

% Introduce X alturas H en una lista
takeAndRepeat(0,_,[]) :- !.
takeAndRepeat(X,H,[H|LH]) :- X1 is X-1, takeAndRepeat(X1,H,LH).

% Devuele la altura máxima de un Skyline
maxh([c(_,H)|[]],H) :- !.
maxh([c(_,H)|R],H) :- maxh(R,C2), H > C2, !.
maxh([_|R],C) :- maxh(R,C).
