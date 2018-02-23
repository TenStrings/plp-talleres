//
// Constantes
//
Bool = {
	toString: function(){
		return "Bool";
	},

	deepCopy: function(){
		return this;
	}
};


TT = {
	toString: function(){
		return "True";
	},

	deepCopy: function(){
		return this;
	}
};

FF = {
	toString: function(){
		return "False";
	},

	deepCopy: function(){
		return this;
	}
};



function Funcion(tipo1, tipo2) {
	this.dominio = tipo1;
	this.imagen = tipo2;
}

function Aplicacion(expresion1, expresion2){
	this.funcion = expresion1;
	this.parametro = expresion2;
}

function Abstraccion(variable,tipo,expresion){
	this.variableLibre = variable;
	this.tipo = tipo;
	this.expresion = expresion;
}

function VariableLibre(nombre){
	this.nombre = nombre;
}

//
// toString
//

Funcion.prototype.toString = function(){
		return '(' + this.dominio.toString() + ' -> ' + this.imagen.toString() + ')';
}


Aplicacion.prototype.toString = function(){
	return '(' + funcion.toString() + ' ' + parametro.toString() + ')';
}


Abstraccion.prototype.toString = function(){
	return '\\' + variableLibre.toString() + ':' + tipo.toString() + '.' + expresion.toString();
}



VariableLibre.prototype.toString = function(){
	return nombre;
}

//
// DeepCopy
//
function DeepCopyTrait(){}

DeepCopyTrait.prototype.deepCopy = function(){
	let newCopy = {};
	for(let property in this){
		console.log(DeepCopyTrait.prototype);
		if(DeepCopyTrait.prototype.isPrototypeOf(this[property])){
			newCopy[property] = this[property].deepCopy();
		}
	}
	newCopy.__proto__ = this.__proto__
	return newCopy;
}

Bool.__proto__ = DeepCopyTrait.prototype
TT.__proto__ = DeepCopyTrait.prototype
FF.__proto__ = DeepCopyTrait.prototype
Funcion.prototype.__proto__ = DeepCopyTrait.prototype
Aplicacion.prototype.__proto__ = DeepCopyTrait.prototype
Abstraccion.prototype.__proto__ = DeepCopyTrait.prototype
VariableLibre.prototype.__proto__ = DeepCopyTrait.prototype

//
// Ejemplos
//

BoolToBool = new Funcion(Bool, Bool);

BoolToBoolDeepCopy = BoolToBool.deepCopy();
