/** Classe representant les Irrationnels
  *@constructor cree un nouvelle instance d'un irrationnel : l'irrationnel vaut sqrt(nombre)
  *@param nombre Int
  */	

class Irrationnel(nombre:Int) { //nombre est le radicande : l'irrationnel vaut sqrt(nombre).


  /**
  *Fonction testant si a divise b
  *@param a Int
  *@param b Int
  *@return Boolean
  */
  def divise(a:Int, b:Int):Boolean = {
    return (b % a == 0);
  }
  
 
  /**
  *Fonction testant la primalite d'un nombre
  *@param n Int
  *@return Boolean
  */
  def premier(n:Int):Boolean = {
    if (n == 2) {return true}
    else {if (divise(2,n)) {return false}}
    var k = 3;
    while (k*k <= n) {
      if (n%k == 0) {return false}
      else {k += 2}
    }
    return true;
  }
  
  /**
  *Crible d'eratosthene : Donne la liste des nombres premiers inferieurs ou egaux e n
  *@param n Int
  *@return List[Int]
  */
  def crible(n:Int):List[Int] = {
    val res = new Range(2, n+1, 1).toList;
    return res.filter(x => premier(x));
  }
  

  /**
  *Fonction retournant le nombre d'occurences de chaque element d'une liste
  *Exemple : occurences([1,1,1,2,4]) = [(1,3),(2,1),(4,1)] 
  *@param l List[Int]
  *@return List[(Int,Int)]
  */
  def occurences(l:List[Int]):List[(Int, Int)] = {
    val res = l.map(x => (x,l.count(y => y == x)));
    return res.distinct;
  }
  
  
  /**
  *Fonction decomposant un entier en facteurs premiers
  *@return List[(Int,Int)]
  */
  def decomposer():List[(Int,Int)] = {
    if (this.nombre == 0) return List((0,0));
    var nb = this.nombre;
    var res = List[Int]();
    val premiers = crible(nb/2)
    while (!premier(nb)) {
      val diviseur:Int = premiers.find(x => divise(x, nb)).get;
      nb /= diviseur;
      res = res.+:(diviseur);
    }
    res = res.+:(nb).reverse;
    return occurences(res);
  }
  
  /**
  *Fonction pour afficher l'irrationnel
  *@return String
  */
  def afficher():String = {
    var res = "";
    decomposer().foreach(
        paire => if (divise(2, paire._2)) {res += Math.pow(paire._1, paire._2/2).toInt + " x " }
        		 else { if (paire._2 > 1) {res += Math.pow(paire._1, paire._2-1).toInt}; res += "sqrt(" + paire._1 + ") x ";}
    );
    return res.dropRight(3);
  }
  
}
