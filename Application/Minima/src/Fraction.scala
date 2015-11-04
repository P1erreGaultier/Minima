/** Classe representant les fractions
  *@constructor cr�e un nouvelle instance d'une fraction
  *@param numerateur Int
  *@param denominateur Int
  */	



class Fraction(var numerateur:Int, var denominateur: Int) {
  
	/**
	*Fonction qui renvoie le numerateur de la fonction
 	*@param numerateur Int
 	*@return Fraction
 	*/
	def this(numerateur:Int) = this(numerateur,1)
  
	/**
	*Fonction qui verifie si une fraction en correcte
 	*/
	def checkFraction() = {
	  if (this.denominateur==0){
	    throw new IllegalArgumentException("Le D�nominateur ne peut pas �tre nul")
	  }
	  else if (this.denominateur<0){
		numerateur   *= -1
		denominateur *= -1
	  }
	}

	def pgcdAux(a:Int,b:Int):Int = {
	  if (a>=b){
	    if(b==0){
	      return a
	    }
	    else {
	      pgcdAux(b,a%b)
	    }
	  }
	  else {pgcdAux(b,a)}
	}

	/**
	*Fonction qui renvoie le pgcd de la fraction
 	*@return Int
 	*/	
	def pgcd():Int = {
	  return pgcdAux(this.numerateur,this.denominateur)
	}
	

	/**
	*Fonction qui renvoie une repr�sentation de la fraction en String
 	*@return String
 	*/
	override def toString(): String = {
	  var res = ""
	  (this.numerateur,this.denominateur) match{
	    case(_,1) => res = this.numerateur.toString
	    case(0,_) => res = this.numerateur.toString
	    case _ => res = this.numerateur.toString+"/"+this.denominateur.toString
	  }
	  if(this.denominateur==this.numerateur){
	    res = this.numerateur.toString
	  }
	  else if (this.numerateur==(-this.denominateur)){
	    if(this.numerateur<0){
	    res = this.numerateur.toString
	    }
	    else {res = this.denominateur.toString}
	  }
	  return res
	}
	
	/**
	*Fonction qui remplace la fraction par une autre fraction
 	*@param f Fraction
 	*@return Fraction
 

	*/	
	def memeD(f:Fraction) = {
	  this.denominateur *= f.denominateur
	  f.denominateur *= this.denominateur
	}

	/**
	*Fonction qui ajoute deux fractions
 	*@param f Fraction
 	*@return Fraction
 	*/	
	def +(f:Fraction):Fraction = {
	  this.memeD(f)
	  var res = new Fraction(this.numerateur+f.numerateur,this.denominateur).pgcd
	  return (new Fraction((this.numerateur+f.numerateur)%res,this.denominateur%res))
	}
	
}
