sala(a).
sala(b).
sala(c).

professor(aline).
professor(leonardo).
professor(marcos).
professor(karina).

materia(ia,aline,7).
materia(es,leonardo,11).
materia(visualizacao,marcos,11).
materia(arquitetura,karina,9).

equipamento(datashow).
equipamento(arcondicionado).
equipamento(luzes).
equipamento(computador).

usa(aline,datashow).
usa(aline,computador).
usa(aline,luzes).
usa(leonardo,datashow).
usa(leonardo,arcondicionado).
usa(leonardo,luzes).
usa(marcos,computador).
usa(marcos,arcondicionado).
usa(karina,datashow).

temperaturaar(leonardo,15).
temperaturaar(marcos,20).

aula(aline,a,7,9).
aula(leonardo,a,11,13).
aula(karina,c,9,11).
aula(marcos,b,11,13).

naoaula(a,9,11).


%ligarLuz
can(ligarluz(Sala),[at(Professor,Sala),desligado(Sala,luzes)], sala) :- 
	aula(Professor,Sala,_,_),
	equipamento(luzes).

adds(ligarluz(Sala),[ligado(Sala,luzes)],_, sala):-
	sala(Sala).

deletes(ligarluz(Sala),[desligado(Sala,luzes)], sala):-
	sala(Sala).

	
%desligarluz
can(desligarluz(Sala),[ligado(Sala,luzes)], sala) :-
	sala(Sala),
	equipamento(luzes).
	
adds(desligarluz(Sala),[desligado(Sala,luzes)],_, sala):-
	sala(Sala).
		
deletes(desligarluz(Sala),[ligado(Sala,luzes)], sala) :-
	sala(Sala).
	
	
%ligararcondicionado
can(ligararcondicionado(Sala),[at(Professor,Sala),desligado(Sala,arcondicionado)], sala) :- 
	aula(Professor,Sala,_,_),
	equipamento(arcondicionado).
	
adds(ligararcondicionado(Sala),[ligado(Sala,arcondicionado),tempatual(Temperatura)],_, sala):-
	aula(Professor,Sala,_,_),
	equipamento(arcondicionado),
	usa(Professor,arcondicionado),
	temperaturaar(Professor,Temperatura).
	
deletes(ligararcondicionado(Sala),[desligado(Sala,arcondicionado)], sala):-
	sala(Sala).	

	
%desligararcondicionado
can(desligararcondicionado(Sala),[ligado(Sala,arcondicionado)], sala) :-
	sala(Sala),
	equipamento(arcondicionado).
	
adds(desligararcondicionado(Sala),[desligado(Sala,arcondicionado)],_, sala) :-
	sala(Sala).
		
deletes(desligararcondicionado(Sala),[ligado(Sala,arcondicionado)], sala) :-
	sala(Sala).

	
%ajustararcondicionado
can(ajustararcondicionado(Sala,Temperatura),[at(Professor,Sala),ligado(Sala,arcondicionado),tempatual(T)], sala) :-
	aula(Professor,Sala,_,_),
	equipamento(arcondicionado),
	usa(Professor,arcondicionado),
	temperaturaar(Professor,Temperatura),
	Temperatura\==T.
	
adds(ajustararcondicionado(Sala,Temperatura),[tempatual(Temperatura)],_, sala) :-
	aula(Professor,Sala,_,_),
	temperaturaar(Professor,Temperatura).

deletes(ajustararcondicionado(Sala,Temperatura),[tempatual(T)], sala) :-
	aula(Professor,Sala,_,_),
	equipamento(arcondicionado),
	usa(Professor,arcondicionado),
	temperaturaar(Professor,Temperatura),
	Temperatura\==T.


%ligarcomputador(
can(ligarcomputador(Sala),[at(Professor,Sala),desligado(Sala,computador)], sala) :- 
	aula(Professor,Sala,_,_),
	equipamento(arcondicionado).
	
adds(ligarcomputador(Sala),[ligado(Sala,computador)],_, sala):-
	sala(Sala).
	
deletes(ligarcomputador(Sala),[desligado(Sala,computador)], sala):-
	sala(Sala).	
	
	
%desligarcomputador
can(desligarcomputador(Sala),[ligado(Sala,computador)], sala) :-
	sala(Sala),
	equipamento(computador).
	
adds(desligarcomputador(Sala),[desligado(Sala,computador)],_, sala):-
	sala(Sala).
		
deletes(desligarcomputador(Sala),[ligado(Sala,computador)], sala) :-
	sala(Sala).

	
%ligardatashow
can(ligardatashow(Sala),[at(Professor,Sala),desligado(Sala,datashow)], sala) :- 
	aula(Professor,Sala,_,_),
	equipamento(datashow).
	
adds(ligardatashow(Sala),[ligado(Sala,datashow)],_, sala):-
	sala(Sala).
	
deletes(ligardatashow(Sala),[desligado(Sala,datashow)], sala):-
	sala(Sala).	
	
	
%desligardatashow
can(desligardatashow(Sala),[ligado(Sala,datashow)], sala) :-
	sala(Sala),
	equipamento(datashow).
	
adds(desligardatashow(Sala),[desligado(Sala,datashow)],_, sala):-
	sala(Sala).
		
deletes(desligardatashow(Sala),[ligado(Sala,datashow)], sala) :-
	sala(Sala).

	
%daraula
can(daraula(Professor,Sala,Materia), [at(Professor,Sala),ligado(Sala,X),horario(N),desligado(Sala,Y)],sala):-
	equipamento(X),
	usa(Professor, X),
	equipamento(Y),
	not(usa(Professor,Y)),
	aula(Professor,Sala,N,_),
	materia(Materia,Professor,N).

adds(daraula(Professor,Sala,Materia),[deuaula(Professor),horario(M)],_,sala):-
	aula(Professor,Sala,N,M),
	materia(Materia,Professor,N).
	
deletes(daraula(Professor,Sala,Materia),[horario(N)],sala) :-	
	aula(Professor,Sala,N,_),
	materia(Materia,Professor,N).

	
%passartempo
can(passartempo(Sala), [horario(N)],sala):-
	(naoaula(Sala,N,M),
	aula(_,Sala,_,N));
	(naoaula(Sala,N,M),
	aula(_,Sala,M,_)).
	
adds(passartempo(Sala),[horario(M)],_,sala):-
	naoaula(Sala,_,M).
	
deletes(passartempo(Sala),[horario(N)],sala) :-
	naoaula(Sala,N,_).

	
%abrirsala
can(abrirsala(Sala), [horario(N),fechado(Sala)],sala):-
	aula(_,Sala,N,_).
	
adds(abrirsala(Sala),[at(Professor,Sala),aberto(Sala)],_,sala):-
	aula(Professor,Sala,_,_).
	
deletes(abrirsala(Sala),[fechado(Sala)],sala) :-
	aula(_,Sala,_,_).
	
	
%fecharsala
can(fecharsala(Sala), [at(Professor,Sala),horario(N),aberto(Sala),desligado(Sala,X)],sala):-
	(naoaula(Sala,N,_),
	 equipamento(X),
	 usa(Professor,X),
	 aula(Professor,Sala,_,N));
	(professor(Professor),
	 professor(P),
	 Professor\==P,
	 equipamento(X),
	 not(usa(P,X)),
	 aula(P,Sala,N,_),
	 aula(Professor,Sala,_,N)).
	
	
adds(fecharsala(Sala),[fechado(Sala)],_,sala):-
	aula(_,Sala,_,_).	
	
deletes(fecharsala(Sala),[at(Professor,Sala),aberto(Sala)],sala) :-
	aula(Professor,Sala,_,_).
	
	
%testes de planos
	
test2(P) :-
	plan([at(aline, a),horario(7),fechado(a),sala(a),desligado(a,luzes)],
	     [ligado(a,luzes)], sala,P).
		 	 
test3(P) :-
	plan([horario(7),fechado(a),desligado(a,luzes),desligado(a,datashow),desligado(a,computador)],
		 [at(aline,a),aberto(a),ligado(a,luzes),ligado(a,datashow),ligado(a,computador)], sala,P).
		 
test4(P) :-
	plan([horario(7),fechado(a),desligado(a,luzes),desligado(a,datashow),desligado(a,computador),desligado(a,arcondicionado)],
	     [deuaula(aline), horario(9),fechado(a)], sala,P).
		 
test5(P) :-
	plan([horario(7),fechado(a),desligado(a,luzes),desligado(a,datashow),desligado(a,computador),desligado(a,arcondicionado)],
	     [deuaula(aline),horario(11),fechado(a)], sala,P).

test6(P) :-
	plan([horario(7),fechado(a),desligado(a,luzes),desligado(a,datashow),desligado(a,computador),desligado(a,arcondicionado)],
	     [horario(11),fechado(a)], sala,P).		 
		 
test7(P) :-
	plan([at(leonardo, a),ligado(a,arcondicionado),tempatual(40)],
	     [at(leonardo, a),ligado(a,arcondicionado),tempatual(15)], sala,P).
		 
		 