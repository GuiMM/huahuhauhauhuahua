sala(a).
sala(b).
sala(c).

aberto(X) :- not(fechado(X)).

%generalizando para qualquer equipamento, de qualquer sala.
ligado(X,Y) :- not(desligado(X,Y)).


professor(aline).
professor(leonardo).
professor(marcos).
professor(karina).

materia(ia,aline).
materia(es,leonardo).
materia(visualizacao,marcos).
materia(arquitetura,karina).

usa(aline,datashow).
usa(aline,computador).
usa(aline,luzes).
usa(leonardo,datashow).
usa(leonardo,arcondicionado).
%usa(leonardo,luzes).
usa(marcos,computador).
usa(marcos,arcondicionado).
usa(karina,datashow).


arcondicionado(leonardo,15).
arcondicionado(marcos,20).

aula(aline,a,7,9).
aula(leonardo,a,11,13).
aula(karina,b,9,11).
aula(marcos,b,11,13).

%ligarLuz(Sala).
can(ligarluz(Sala),[at(Professor,Sala), usa(Professor,luzes)], sala) :- 
	sala(Sala),
	%aula(Professor,Sala,_,_),
	professor(Professor).

adds(ligarluz(Sala),[ligado(Sala,luzes)],ligado(Sala,luzes), sala):-
	sala(Sala).
	

deletes(ligarluz(Sala),[desligado(Sala,luzes)], sala):-
	sala(Sala).
	
%ligararcondicionado(Sala)
can(ligararcondicionado(Sala),[at(Professor,Sala),usa(Professor,arcondicionado)], sala) :- 
	sala(Sala),
	%aula(Professor,Sala,_,_),
	professor(Professor).
	
adds(ligararcondicionado(Sala),[ligado(Sala,arcondicionado)],ligado(Sala,arcondicionado), sala):-
	sala(Sala).
	
deletes(ligararcondicionado(Sala),[desligado(Sala,arcondicionado)], sala):-
	sala(Sala).	

%desligararcondicionado(Sala)
can(desligararcondicionado(Sala),[not(usa(Professor,arcondicionado))], sala) :-
	sala(Sala),
	professor(Professor),
	aula(Professor,Sala,_,_).
	
adds(desligararcondicionado(Sala),[desligado(Sala,arcondicionado)],desligado(Sala,arcondicionado), sala):-
	sala(Sala).
		
deletes(desligararcondicionado(Sala),[ligado(Sala,arcondicionado)], sala) :-
	sala(Sala).

%ajustararcondicionado

%ligarcomputador	
%desligarcomputador
%ligardatashow
%desligardatashow
%desligarluz
%preparar(sala)
can(preparar(sala), [at(Professor,uff),horario(N),aberto(Sala)],sala):-
	sala(Sala),
	%usa(Professor, X),
	%not(usa(Professor,Y)),
	aula(Professor,Sala,N,_).
	
adds(preparar(sala),[at(Professor,Sala)],[at(Professor,Sala)],sala):-
	sala(Sala),
	aula(Professor,Sala,_,_).	
	
deletes(preparar(sala),[at(Professor,uff),fechado(Sala)],sala) :-
	sala(Sala),
	aula(Professor,Sala,_,_).
	
%daraula(avançar o horario, adicionar deuaula(Professor)).
can(daraula(Sala,Professor), [at(Professor,Sala),ligado(Sala,X),horario(N)],sala):-
	sala(Sala),
	usa(Professor, X),
	aula(Professor,Sala,N,_).

adds(daraula(Sala,Professor),[at(Professor,uff),deuaula(Professor),horario(M),fechado(Sala)]
	,[at(Professor,uff),deuaula(Professor),horario(M),fechado(Sala)],sala):-
	sala(Sala),
	aula(Professor,Sala,_,M),
	usa(Professor,X).
	
deletes(daraula(Sala,Professor),[horario(N)],sala) :-	
	sala(Sala),
	aula(Professor,Sala,N,_).
	

%abrirsala
can(abrirsala(sala), [at(Professor,uff),horario(N)],sala):-
	sala(Sala),
	%usa(Professor, X),
	%not(usa(Professor,Y)),
	aula(Professor,Sala,N,_).
	
adds(abrirsala(sala),[aberto(Sala)],[aberto(Sala)],sala):-
	sala(Sala).	
	
deletes(abrirsala(sala),[fechado(Sala)],sala) :-
	sala(Sala),
	aula(Professor,Sala,_,_).
	
%fecharsala
can(fecharsala(sala), [at(Professor,uff),horario(N)],sala):-
	sala(Sala),
	%usa(Professor, X),
	%not(usa(Professor,Y)),
	aula(Professor,Sala,_,N).
	
adds(fecharsala(sala),[fechado(Sala)],[fechado(Sala)],sala):-
	sala(Sala).	
	
deletes(fecharsala(sala),[aberto(Sala)],sala) :-
	sala(Sala).	
	
test(P) :-
	plan([at(aline, uff), at(leonardo,uff),horario(7),fechado(a),sala(a),desligado(a,luzes)
	,desligado(a,arcondicionado), desligado(a,datashow),desligado(a,computador)]
	,[deuaula(aline),deuaula(leonardo),desligado(a,luzes),desligado(a,arcondicionado)
	, desligado(a,datashow),desligado(a,computador),fechado(a),horario(13)], sala,P).
		 
		 
	%testando o ligar luzes	 
test2(P) :-
	plan([at(aline, a),horario(7),fechado(a),sala(a)],
	     [ligado(a,luzes)], sala,P).
		 
test3(P) :-
	plan([at(aline, a),horario(7),fechado(a),sala(a)],
	     [ligado(a,arcondicionado)], sala,P).
test4(P) :-
	plan([at(aline, uff),horario(7),fechado(a),sala(a)],
	     [at(aline,uff),deuaula(aline), horario(9),fechado(a)], sala,P).
		 
		 
		 