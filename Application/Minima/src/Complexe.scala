/** Classe representant les nombres complexes
  *@constructor crée un nouvelle instance d'un nombre complexe
  *@param reel Double
  *@param imag Double
  */	

class Complexe(var reel:Double, var imag:Double) {

  def this(reel: Double) {
    this(reel, 0);
  }


/**
  *Fonction qui additionne un reel a la partie reel du complexe 
  *@param x Double
  *@return Complexe
  */
  def +(x: Double) = {
    new Complexe(this.reel + x, this.imag)
  }
  
/**
  *Fonction qui ajoute deux complexe
  *@param c Complexe
  *@return Complexe
  */
  def +(c: Complexe) = {
    new Complexe(this.reel + c.reel, this.imag + c.imag)
  }

/**
  *Fonction qui soustrait un reel a la partie reel du complexe 
  *@param x Double
  *@return   Complexe
  */    
  def -(x: Double) = {
    new Complexe(this.reel - x, this.imag)
  }

/**
  *Fonction qui soustrait un complexe a un autre
  *@param c Complexe
  *@return   Complexe
  */  
  def -(c: Complexe) = {
    new Complexe(this.reel - c.reel, this.imag - c.imag)
  }  

/**
  *Fonction qui multiplie un complexe avec un reel
  *@param x Double
  *@return Complexe
  */  
  def *(x: Double) = {
    new Complexe(this.reel * x, this.imag * x)
  }

/**
  *Fonction qui divise un complexe par un reel
  *@param x Double
  *@return   Complexe
  */  
  def /(x: Double) = {
    new Complexe(this.reel / x, this.imag / x)
  }

/**
  *Fonction qui renvoie l'inverse d'un complexe
  *@return   Complexe
  */  
  def unary_- = new Complexe(-reel, -imag);

/**
  *Fonction qui renvoie une representation du complexe sous forme de chaine de Caracteres
  *@return   String
  */
  override def toString:String = {
    if (imag > 0) {return reel + " + " + imag + "i";}
    else {if (imag < 0) {return reel + " " + imag + "i";}
    else {return reel.toString;}
    }
  }
}
