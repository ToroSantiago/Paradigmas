:- include(cartas).
 
% OBJETIVOS PRELIMINARES

% valor/2: Determina el valor de una carta dada su posición en la escala de valores.

valor(carta(4,espada), 1). 
valor(carta(4,basto), 1).  
valor(carta(4,oro), 1).    
valor(carta(4,copa), 1).   

valor(carta(5,espada), 2).   
valor(carta(5,basto), 2).   
valor(carta(5,oro), 2).   
valor(carta(5,copa), 2).   

valor(carta(6,espada), 3).    
valor(carta(6,basto), 3).    
valor(carta(6,oro), 3).    
valor(carta(6,copa), 3).    

valor(carta(7,basto), 4).    
valor(carta(7,copa), 4).    

valor(carta(10,espada), 5).
valor(carta(10,basto), 5). 
valor(carta(10,oro), 5).  
valor(carta(10,copa), 5).  

valor(carta(11,espada), 6).
valor(carta(11,basto), 6). 
valor(carta(11,oro), 6).  
valor(carta(11,copa), 6).  

valor(carta(12,espada), 7).
valor(carta(12,basto), 7).  
valor(carta(12,oro), 7).
valor(carta(12,copa), 7).  

valor(carta(1,oro), 8).
valor(carta(1,copa), 8).   

valor(carta(2,espada), 9). 
valor(carta(2,basto), 9). 
valor(carta(2,oro), 9).   
valor(carta(2,copa), 9).  

valor(carta(3,espada), 10).   
valor(carta(3,basto), 10).    
valor(carta(3,oro), 10).    
valor(carta(3,copa), 10).   

valor(carta(7,oro), 11).    

valor(carta(7,espada), 12).    

valor(carta(1,basto), 13).    

valor(carta(1,espada), 14).   

% resultado: determina el resultado de enfrentar dos cartas
resultado(Carta1, Carta2, Resultado) :-
    valor(Carta1, Valor1),
    valor(Carta2, Valor2),
    comparar(Valor1, Valor2, Resultado), !.

% comparar: compara los valores de dos cartas
comparar(Valor1, Valor2, Resultado) :-
    Valor1 > Valor2,
    Resultado = gana, !.
comparar(Valor1, Valor2, Resultado) :-
    Valor1 < Valor2,
    Resultado = pierde, !.
comparar(Valor1, Valor2, Resultado) :-
    Valor1 =:= Valor2,  % usar cat
    Resultado = parda, !.



% OBJETIVOS INTERMEDIOS

envido([carta(Numero1,Palo1),carta(Numero2,Palo1),carta(_,_)],Valor) :-
	esFigura(Numero1), esFigura(Numero2), Valor is 20
	;esFigura(Numero1), not(esFigura(Numero2)), Valor is Numero2 + 20
	;not(esFigura(Numero1)), esFigura(Numero2), Valor is Numero1 + 20
	;not(esFigura(Numero1)), not(esFigura(Numero2)), Valor is Numero1 + Numero2 + 20, !.

envido([carta(_,_),carta(Numero2,Palo2),carta(Numero3,Palo2)],Valor) :-
	esFigura(Numero2), esFigura(Numero3), Valor is 20
	;esFigura(Numero2), not(esFigura(Numero3)), Valor is Numero3 + 20
	;not(esFigura(Numero2)), esFigura(Numero3), Valor is Numero2 + 20
	;not(esFigura(Numero2)), not(esFigura(Numero3)), Valor is Numero2 + Numero3 + 20, !.

envido([carta(Numero1,Palo1),carta(_,_),carta(Numero3,Palo1)],Valor) :-
	esFigura(Numero1), esFigura(Numero3), Valor is 20
	;esFigura(Numero1), not(esFigura(Numero3)), Valor is Numero3 + 20
	;not(esFigura(Numero1)), esFigura(Numero3), Valor is Numero1 + 20
	;not(esFigura(Numero1)), not(esFigura(Numero3)), Valor is Numero1 + Numero3 + 20, !.

envido([carta(Numero1,Palo),carta(Numero2,Palo)], Valor) :-				%envido condos argumentos para aceptarEnvido() con dos cartas en mesa
	esFigura(Numero1), esFigura(Numero2), Valor is 20
	;esFigura(Numero1), not(esFigura(Numero2)), Valor is Numero2 + 20
	;not(esFigura(Numero1)), esFigura(Numero2), Valor is Numero1 + 20
	;not(esFigura(Numero1)), not(esFigura(Numero2)), Valor is Numero1 + Numero2 + 20, !.

envido([carta(Numero1,Palo),carta(Numero2,Palo),carta(Numero3,Palo)],Valor) :-
	sumaMayor(Numero1,Numero2,Numero3,Suma),
	Valor is Suma + 20, !.

sumaMayor(Numero1,Numero2,Numero3,Suma) :-
	(Numero1 > Numero3,
	Numero2 > Numero3,
	Suma is Numero1 + Numero2);
	(Numero2 > Numero1,
	Numero3 > Numero1,
	Suma is Numero2 + Numero3);
	(Numero1 > Numero2,
	Numero3 > Numero2,
	Suma is Numero1 + Numero3).	
	

esFigura(Numero) :- 
	Numero =:= 10; Numero =:= 11; Numero =:= 12.


gana([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[carta(Numero4,Palo4),carta(Numero5,Palo5),carta(Numero6,Palo6)]) :-
	resultado(carta(Numero1,Palo1),carta(Numero4,Palo4),Resultado1),
	resultado(carta(Numero2,Palo2),carta(Numero5,Palo5),Resultado2),
	resultado(carta(Numero3,Palo3),carta(Numero6,Palo6),Resultado3),
	((Resultado1 == gana, Resultado2 == gana);
	(Resultado2 == gana, Resultado3 == gana);
	(Resultado1 == gana, Resultado3 == gana)), !.


mejorJugado([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[carta(Numero4,Palo4),carta(Numero5,Palo5),carta(Numero6,Palo6)],Orden) :-
	gana([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[carta(Numero4,Palo4),carta(Numero5,Palo5),carta(Numero6,Palo6)]), % 1 2 3
	Orden = [carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)]

	; gana([carta(Numero1,Palo1),carta(Numero3,Palo3),carta(Numero2,Palo2)],[carta(Numero4,Palo4),carta(Numero5,Palo5),carta(Numero6,Palo6)]), % 1 3 2
	Orden = [carta(Numero1,Palo1),carta(Numero3,Palo3),carta(Numero2,Palo2)]

	; gana([carta(Numero2,Palo2),carta(Numero3,Palo3),carta(Numero1,Palo1)],[carta(Numero4,Palo4),carta(Numero5,Palo5),carta(Numero6,Palo6)]), % 2 3 1
	Orden = [carta(Numero2,Palo2),carta(Numero3,Palo3),carta(Numero1,Palo1)]

	; gana([carta(Numero2,Palo2),carta(Numero1,Palo1),carta(Numero3,Palo3)],[carta(Numero4,Palo4),carta(Numero5,Palo5),carta(Numero6,Palo6)]), % 2 1 3
	Orden = [carta(Numero2,Palo2),carta(Numero1,Palo1),carta(Numero3,Palo3)]

	; gana([carta(Numero3,Palo3),carta(Numero2,Palo2),carta(Numero1,Palo1)],[carta(Numero4,Palo4),carta(Numero5,Palo5),carta(Numero6,Palo6)]), % 3 2 1
	Orden = [carta(Numero3,Palo3),carta(Numero2,Palo2),carta(Numero1,Palo1)]

	; gana([carta(Numero3,Palo3),carta(Numero1,Palo1),carta(Numero2,Palo2)],[carta(Numero4,Palo4),carta(Numero5,Palo5),carta(Numero6,Palo6)]), % 3 1 2
	Orden = [carta(Numero3,Palo3),carta(Numero1,Palo1),carta(Numero2,Palo2)], !.



% OBJETIVO PRINCIPAL

aceptarEnvido([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[carta(Numero4,_)]) :-
	envido([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],Valor1), % obtengo valor envido
	(
	Valor1 =:= 33    	% tengo el mayor valor, no puedo perder
	;Numero4 < 6		% no hay peligro de envido de mayor valor por parte del enemigo (asumiendo que carta4 la usa para el envido)
	;
	(esFigura(Numero4), 
	Valor1 > 27)
	), !.

aceptarEnvido([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[carta(Numero4,Palo4),carta(Numero5,Palo5)]) :-
	envido([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],Valor1),
	(envido([carta(Numero4,Palo4),carta(Numero5,Palo5)],Valor2),
	comparar(Valor1,Valor2,Resultado), !,
	Resultado == gana), !
	;aceptarEnvido([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[carta(Numero4,Palo4)]), !
	;aceptarEnvido([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[carta(Numero5,Palo5)]), !.

aceptarEnvido([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[carta(Numero4,Palo4),carta(Numero5,Palo5),carta(Numero6,Palo6)]) :-
	envido([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],Valor1),
	envido([carta(Numero4,Palo4),carta(Numero5,Palo5),carta(Numero6,Palo6)],Valor2),
	comparar(Valor1,Valor2,Resultado), !,
	Resultado == gana, !.


aceptarTruco([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[[], [carta(Numero4,Palo4)]]) :-
	.

aceptarTruco([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[[carta(Numero1,Palo1)], [carta(Numero4,Palo4)]]) :-
	.

aceptarTruco([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[[carta(Numero1,Palo1)], [carta(Numero4,Palo4),carta(Numero5,Palo5)]]) :-
	.

aceptarTruco([carta(Numero1,Palo1),carta(Numero2,Palo2),carta(Numero3,Palo3)],[[carta(Numero1,Palo1),carta(Numero2,Palo2)], [carta(Numero4,Palo4),carta(Numero5,Palo5)]]) :-
	.
