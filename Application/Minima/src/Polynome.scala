
/** Classe representant les polynomes
  *@constructor cree un nouvelle instance d'un polynome
  *@param coefs List[int]
  */	

class Polynome(var coefs:List[Int]) {
  



	/**
  	*Fonction qui renvoie le degre d'un polyneme, c'est-e-dire la puissance du premier coefficient
  	*@return Int
  	*/
	def degre(): Int = {
	  if (this.coefs.endsWith(List(0))) {
	    return (new Polynome(this.coefs.dropRight(1)).degre()); //Si les plus hauts coefs sont nuls, on les ignore
	  }
	  return this.coefs.size-1;
	}
  


	/**
  	*Fonction qui additionne deux polynomes
  	*@param p Polynome
  	*@return Polynome
  	*/
	def +(p:Polynome): Polynome = {
	  if (this.degre() >= p.degre()) {
		  p.coefs = p.coefs ++ (for (i <- 0 to (this.degre - p.degre)) yield 0).toList; //On rajoute des zeros au plus petit polyneme pour que les degres soient egaux
		} else {
		  this.coefs = this.coefs ++ (for (i <- 0 to (p.degre - this.degre)) yield 0).toList;
		}
		return new Polynome((for(i <- 0 to Math.max(this.degre, p.degre)) yield this.coefs(i)+p.coefs(i)).toList); //On additionne membre e membre
	}


	/**
  	*Fonction qui fait la soustraction de deux polynemes
  	*@param p Polynome
  	*@return Polynome
  	*/
	def -(p:Polynome): Polynome = {
		if (this.degre() >= p.degre()) {
		  p.coefs = p.coefs ++ (for (i <- 0 to (this.degre - p.degre)) yield 0).toList;
		} else {
		  this.coefs = this.coefs ++ (for (i <- 0 to (p.degre - this.degre)) yield 0).toList;
		}
		return new Polynome((for(i <- 0 to Math.max(this.degre, p.degre)) yield this.coefs(i)-p.coefs(i)).toList);
	}


	/**
  	*Fonction fait la multiplication d'un polyneme par un scalaire
  	*@param scal int
  	*@return Polynome
  	*/
	def multis(scal:Int): Polynome = {
		return new Polynome( this.coefs.map(x => x*scal)); //Chaque coefficient est multiplie par x
	}


	/**
  	*Fonction qui fait la multiplication de deux polynemes
  	*@param p Polynome
  	*@return Polynome
  	*/
	def *(p:Polynome): Polynome = {
		var res, temp = new Polynome(List[Int]())
		var zeros = List[Int]() //Une liste de zeros initialement vide que l'on concatene aux coefs pour augmenter le degre
		for(i <- 0 to p.degre()) {
			temp = this.multis(p.coefs(i))
			temp.coefs = zeros ++ temp.coefs //On rajoute les zeros avant la liste pour augmenter le degre du polyneme temporaire
			zeros = zeros.::(0) //On ajoute un zero dans la liste
			res = res.+(temp) //On additionne le resultat et le temporaire
		}
		return res;
	}
	
	
	/**
  	*Fonction qui fait la Derivation d'un polyneme
  	*@return Polynome
  	*/
	def deriver(): Polynome = {
		if (this.degre == 0) {
			return new Polynome(List(0));
		} else {
			var temp = new Polynome(this.coefs.drop(1)) //On enleve la constante
			val res = for (i <- 0 to temp.degre()) yield temp.coefs(i)*(i+1);
			return new Polynome(res.toList);
		}
	}
	

	/**
  	*Fonction qui calcule de l'image du polyneme pour une valeur de X
  	*@param x Double
  	*@return Double
  	*/
	def calculer(x:Double): Double = {
	  val res = for (i <- 0 to this.degre()) yield this.coefs(i) * Math.pow(x, i);
	  return res.sum;
	}
	
	/**
  	*Fonction qui calcule les racines (Seulement 1er et 2nd degre)
  	*@return List[Any]
  	*/
	def resoudre(): List[Any] = {
	  val n = this.degre();
	  val a:Double = this.coefs(n);
	  if (n>0){
	  val b:Double = this.coefs(n-1);
	  
	  this.coefs match {
	    //equation de degre 1 : ax + b = 0
	    case List(_,_) 		=> return List(new Fraction(-b.toInt,a.toInt));
	    //equation de degre 2 : axe + bx + c = 0
	    case List(_,_,_) 	=> {val c = this.coefs(n-2);
	    		   				val delta = (b*b)-(4*a*c);
		    		   				if (delta >= 0) {
						    		   return List(new Fraction((-b+Math.sqrt(delta)).toInt,(2*a).toInt), new Fraction((-b-Math.sqrt(delta)).toInt,(2*a).toInt));
						    	    } else { //Si delta est negatif, les solutions sont des nombres complexes.
						    		   return List((new Complexe(-b, Math.sqrt(delta.abs))/(2*a)), (new Complexe(-b, -Math.sqrt(delta.abs))/(2*a)))
						    	    };
	    						};
	    case _ => return Nil;
	   }
	  }
	  else{ return Nil}
	  }
	  
	
	
	/**
  	*Fonction qui affiche en tant qu'expression mathematique
  	*@return String
  	*/
	def afficher(): String = {
		if (this.coefs == List(0)) {return "0"} else {
			var res = ""
			for (i <- (0 to this.degre()).reverse) { //On affiche en premier les coefs de plus grand degre
			  var x = this.coefs(i)
			  if (x != 0) { //Ne pas afficher les coefficients nuls
				i match {
				  case 0 => res += x
				  case 1 => x match {case 1 => res += "X + "; case _ => res += x+"X + "}
				  case _ => x match {case 1 => res += "X^"+i+" + "; case _ => res += x+"X^"+i+" + "}
				}
			  }
			}
			return res;
		}
	}
}
