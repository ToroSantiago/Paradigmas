!classDefinition: #Refugio category: #Laboratorio3!
Object subclass: #Refugio
	instanceVariableNames: 'energia comida agua personas diasOperando capacidadPersonas salas totalOperadoresNecesarios'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Laboratorio3'!

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/27/2024 00:02:38'!
agregarPersonas: unaCantPersonas
	(self tengoEspacio: unaCantPersonas) ifTrue:[
		personas := personas + unaCantPersonas
	].! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 15:37:42'!
agregarSala: unaSala
	self personas >= (totalOperadoresNecesarios + unaSala cantOperadoresNecesarios) ifTrue:[
		salas add: unaSala.
		(unaSala class == Dormitorio) ifTrue: [
			capacidadPersonas := capacidadPersonas + unaSala capacidadExtraAgregada.
		].
		totalOperadoresNecesarios := totalOperadoresNecesarios + unaSala cantOperadoresNecesarios
	]! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/26/2024 16:39:41'!
agua
	 ^ agua! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/26/2024 16:39:11'!
comida
	^ comida! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/26/2024 21:11:55'!
diasOperando
	 ^ diasOperando! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/26/2024 23:46:27'!
energia
	 ^ energia! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/26/2024 23:18:22'!
estaVivo
	 ^energia > 0 and: comida > 0 and: agua > 0.! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 15:49:26'!
hayCisternas
	^((salas select: [:each | each class == Cisterna]) size) > 0! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 15:49:01'!
hayCocinas
	^((salas select: [:each | each class == Cocina]) size) > 0! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 15:49:18'!
hayDormitorios
	^((salas select: [:each | each class == Dormitorio]) size) > 0! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 15:50:01'!
hayGeneradores 
	^((salas select: [:each | each class == Generador]) size) > 0! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 22:16:00'!
haySuficientesOperadores
	 ^personas >= (salas inject: 0 into: [:sum :each | sum + each cantOperadoresNecesarios])! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:51:46'!
initialize
	super.
       energia := 10.
       comida := 10.
       agua := 10.
       personas := 0.
       diasOperando := 0.
	capacidadPersonas := 5.
	salas := OrderedCollection new.
	totalOperadoresNecesarios := 0! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/26/2024 23:30:07'!
mostrarEstado
	 Transcript show: 'Día ', diasOperando printString; cr.
        Transcript show: 'Energía: ', energia printString; cr.
        Transcript show: 'Comida: ', comida printString; cr.
        Transcript show: 'Agua: ', agua printString; cr.
        Transcript show: 'Personas: ', personas printString; cr.
	 Transcript show: 'Estado: ', (self estaVivo ifTrue: ['Vivo'] ifFalse: ['Muerto']); cr.
	 Transcript show: 'El refugio sobrevivió ', diasOperando printString, ' días.'; cr.
        Transcript show: '---'; cr.! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 16:34:51'!
pasarDia
	(self estaVivo and: self haySuficientesOperadores) ifTrue:[
		diasOperando := diasOperando + 1.

		self hayGeneradores  ifTrue: [
			energia := energia + self totalEnergiaGeneradaPorDia.
		].
 
		self puedoPasarElDia ifTrue: [
			energia := energia - self totalEnergiaConsumidaPorDia.
			
			self hayCocinas  ifTrue: [
				comida := comida + self totalComidaGeneradaPorDia.
			].

			self puedoComer ifTrue: [
				comida := comida - personas.
			].

			self hayCisternas  ifTrue: [
				agua := agua + self totalAguaGeneradaPorDia.
			].

			self puedoBeber ifTrue:[
				agua := agua - personas.
			].
	
		] ifFalse: [
			
			self puedoGastarEnergia ifTrue: [
				energia := energia - self totalEnergiaConsumidaPorDia.
			] ifFalse: [
				energia := 0.
			].
			
			self puedoComer ifTrue: [
				comida := comida - personas.
			] ifFalse: [
				comida := 0.
			].
		
			self puedoBeber ifTrue:[
				agua := agua - personas.
			] ifFalse: [
				agua := 0.
			].
		].
	].
       self mostrarEstado.! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/26/2024 21:12:37'!
personas
	 ^ personas! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/26/2024 22:55:24'!
puedoBeber
	 ^personas <= agua! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/26/2024 22:55:53'!
puedoComer
	 ^personas <= comida! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 16:22:04'!
puedoGastarEnergia
	 ^energia > self totalEnergiaConsumidaPorDia! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 16:22:29'!
puedoPasarElDia
	 ^ self puedoGastarEnergia
	and: self puedoComer 
	and: self puedoBeber.! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:21:11'!
salas
	 ^ salas! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/29/2024 18:13:19'!
tengoEspacio: unaCantPersonas
	 ^capacidadPersonas >= (personas + unaCantPersonas).! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 15:18:47'!
totalAguaGeneradaPorDia
	^((salas select: [:each | each class == Cisterna])
            inject: 0 into: [:sum :each | sum + each aguaGeneradaPorDia])! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 15:19:05'!
totalComidaGeneradaPorDia
	^((salas select: [:each | each class == Cocina])
            inject: 0 into: [:sum :each | sum + each comidaGeneradaPorDia])! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 22:14:11'!
totalEnergiaConsumidaPorDia
	^(1 + (salas inject: 0 into: [:sum :each | sum + each consumoEnergia]))! !

!Refugio methodsFor: 'as yet unclassified' stamp: 'AG 6/1/2024 15:19:14'!
totalEnergiaGeneradaPorDia
	^((salas select: [:each | each class == Generador])
            inject: 0 into: [:sum :each | sum + each energiaGeneradaPorDia])! !


!classDefinition: #Sala category: #Laboratorio3!
Object subclass: #Sala
	instanceVariableNames: 'consumoEnergia cantOperadoresNecesarios'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Laboratorio3'!

!Sala methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 19:48:31'!
cantOperadoresNecesarios
	self subclassResponsibility! !

!Sala methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 19:47:17'!
consumoEnergia
	self subclassResponsibility! !


!classDefinition: #Cisterna category: #Laboratorio3!
Sala subclass: #Cisterna
	instanceVariableNames: 'aguaGeneradaPorDia'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Laboratorio3'!

!Cisterna methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:03:56'!
aguaGeneradaPorDia
	^aguaGeneradaPorDia! !

!Cisterna methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:43:22'!
cantOperadoresNecesarios
	^cantOperadoresNecesarios! !

!Cisterna methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:43:40'!
consumoEnergia
	^consumoEnergia! !

!Cisterna methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:42:20'!
initialize
	cantOperadoresNecesarios := 2.
	consumoEnergia := 2.
	aguaGeneradaPorDia := 5! !


!classDefinition: #Cocina category: #Laboratorio3!
Sala subclass: #Cocina
	instanceVariableNames: 'comidaGeneradaPorDia'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Laboratorio3'!

!Cocina methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:41:21'!
cantOperadoresNecesarios
	^cantOperadoresNecesarios! !

!Cocina methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:04:13'!
comidaGeneradaPorDia
	^comidaGeneradaPorDia! !

!Cocina methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:41:12'!
consumoEnergia
	^consumoEnergia! !

!Cocina methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:41:00'!
initialize
	cantOperadoresNecesarios := 1.
	consumoEnergia := 1.
	comidaGeneradaPorDia := 5! !


!classDefinition: #Dormitorio category: #Laboratorio3!
Sala subclass: #Dormitorio
	instanceVariableNames: 'capacidadExtraAgregada'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Laboratorio3'!

!Dormitorio methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:38:57'!
cantOperadoresNecesarios
	^cantOperadoresNecesarios! !

!Dormitorio methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 20:59:42'!
capacidadExtraAgregada
	^capacidadExtraAgregada! !

!Dormitorio methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:38:47'!
consumoEnergia
	^consumoEnergia! !

!Dormitorio methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:38:37'!
initialize
	cantOperadoresNecesarios := 0.
	consumoEnergia := 1.
	capacidadExtraAgregada := 5! !


!classDefinition: #Generador category: #Laboratorio3!
Sala subclass: #Generador
	instanceVariableNames: 'energiaGeneradaPorDia'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Laboratorio3'!

!Generador methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:39:51'!
cantOperadoresNecesarios
	^cantOperadoresNecesarios! !

!Generador methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:40:03'!
consumoEnergia
	^consumoEnergia! !

!Generador methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 20:44:04'!
energiaGeneradaPorDia
	^energiaGeneradaPorDia! !

!Generador methodsFor: 'as yet unclassified' stamp: 'AG 5/30/2024 21:39:32'!
initialize
	cantOperadoresNecesarios := 2.
	consumoEnergia := 0.
	energiaGeneradaPorDia := 4! !
