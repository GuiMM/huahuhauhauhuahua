sala(a).
sala(b).
sala(c).

%fechado(a).
%fechado(b).
%fechado(c).

aberto(X) :- not(fechado(X)).

%desligado(a,luzes).
%desligado(b,luzes).
%desligado(c,luzes).
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

gosta(aline,datashow).
gosta(aline,computador).
gosta(aline,luzes).
gosta(leonardo,datashow).
gosta(leonardo,arcondicionado).
%gosta(leonardo,luzes).
gosta(marcos,computador).
gosta(marcos,arcondicionado).
gosta(karina,datashow).


arcondicionado(leonardo,15).
arcondicionado(marcos,20).

aula(aline,a,7,9).
aula(leonardo,a,11,13).
aula(karina,b,9,11).
aula(marcos,b,11,13).

%ligarLuz(Sala).
can(ligarluz(Sala),[at(Professor,Sala)], sala) :- %vehicle move only within city
	sala(Sala),
	aula(Professor,Sala,_,_),
	professor(Professor),
	gosta(Professor,luzes).

adds(ligarluz(Sala),[ligado(Sala,luzes)],ligado(Sala,luzes), sala):-
	sala(Sala).
	

deletes(ligarluz(Sala),[desligado(Sala,luzes)], sala):-
	sala(Sala).
	
	
	
	
test(P) :-
	plan([at(aline, uff), at(leonardo,uff),horario(7),fechado(a),sala(a)],
	     [deuaula(aline),deuaula(leonardo),desligado(a,luzes),desligado(a,arcondicionado)
		 , desligado(a,datashow),desligado(a,computador),fechado(a),horario(13)], sala,P).
		 
		 
	%testando o ligar luzes	 
test2(P) :-
	plan([at(aline, a),horario(7),fechado(a),sala(a)],
	     [ligado(a,luzes)], sala,P).
		 


		 