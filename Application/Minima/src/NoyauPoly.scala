/** Classe du noyau de l'application Minima
  *@constructor crée un nouvelle instance du noyau
  */	

class NoyauPoly {


	/**
  	*Fonction qui remplace les operateurs par une lettre associé
  	*@param char Char
 	*@return Char
 	*/
	def extractSymbols(char: Char): Char = char match {
	case '+' => 'p'
	case '-' => 'm'
	case '^' => 'e'
	case reste => reste
	}

	/**
  	*Fonction qui replace m par - 
  	*@param char Char
 	*@return Char
 	*/
	def remettreMoins(char: Char): Char = char match {
	case 'm' => '-'
	case reste => reste
	}

	/**
  	*Fonction qui remet tous les - dans une expression 
  	*@param expr String
 	*@return String
 	*/
	def convertMoins(expr: String): String ={
			expr.toList.map(remettreMoins(_)).mkString	
	}

	/**
  	*Fonction qui convertir tout les symboles dans une expresion
  	*@param expr String
 	*@return String
 	*/
	def convertExpression(expr: String): String ={
			(expr.toList.map(extractSymbols(_)):+"z").mkString	
	}

	/**
  	*Fonction qui verifie si une String a la bonne structure pour etre traiter comme un polynome
  	*@param inputPoly String
 	*@return Boolean
 	*/
	def checkIfPoly(inputPoly: String): Boolean ={
			if(inputPoly != ""){
				//val polyRegex = "((m?([1-9]?[0-9])?z)|(m?([1-9]?[0-9])?X((p|m)([1-9]?[0-9])?)?z)|(m?([1-9]?[0-9])?Xe([1-9]?[0-9])((m|p)([1-9]?[0-9])?Xe([1-9]?[0-9]))*((m|p)([1-9]?[0-9])?X)?((m|p)([1-9]?[0-9]))?z))".r
				val polyRegex = "((m?[1-9]?[0-9])|(m?([1-9]?[0-9])?X)|(m?([1-9]?[0-9])?Xe([1-9]?[0-9])))(((m|p)([1-9]?[0-9])?Xe([1-9]?[0-9]))*((m|p)([1-9]?[0-9])?X)*((m|p)([1-9]?[0-9]))*)*z".r
						val output = polyRegex.findFirstIn(convertExpression(inputPoly))
						if(output != None){
							if(inputPoly.length > 0 && output.get.length == convertExpression(inputPoly).length) 
							{
								return true
							} else {return false}}
						else return false
			}
			else{return false}
	}

	/**
  	*Fonction qui extrait une liste de coefficient a partir d'une chaine polynomiale
  	*@param input String
 	*@return List[int]
 	*/
	def extractCoef(input: String): List[Int] ={
			val coefRegex = "((p?|m)[1-9]?[0-9]X)|((p|m)[1-9]?[0-9])?z".r
					val coefString = coefRegex.findAllIn(convertExpression(input)).toList.mkString
					val justCoefRegex = "-?[1-9]?[0-9]".r
					return justCoefRegex.findAllIn(convertMoins(coefString)).toList.map(_.toInt)
	}
	

	/**
  	*Fonction qui extrait une liste d'exposant a partir d'une chaine polynomiale
  	*@param input String
 	*@return List[int]
 	*/
	def extractExpo(input: String): List[Int]  ={
			val expoRegex = "(e[1-9]?[0-9](p|m))|(e[1-9]?[0-9])z".r
					val expoString = expoRegex.findAllIn(convertExpression(input)).toList.mkString
					val justExpoRegex = "[1-9]?[0-9]".r
					return justExpoRegex.findAllIn(expoString).toList.map(_.toInt)
	}

	/**
  	*Fonction qui crée un polynome sous forme de Liste a partir d'une Chaine
  	*@param input String
 	*@return List[int]
 	*/
	def makePoly(input: String): List[Int] = {

			var tmp = List[Int]()
					for(i<-0 to extractExpo(normaliser(input)).max){
						tmp = 0::tmp
					}
			var tab = tmp.toArray
					val regex = "m?[1-9]?[0-9]Xe[1-9]?[0-9]".r
					val section = regex.findAllIn(convertExpression(normaliser(input))).toList
					section.foreach(e => tab.update(extractExpo(e)(0), tab(extractExpo(e)(0))+extractCoef(e)(0))) 

					return tab.toList
	}

	/**
  	*Fonction qui retourne vrai si le polynome est nul
  	*@param poly List[int]
 	*@return Boolean
 	*/
	def egalZero(poly: List[Int]): Boolean = {
			var cpt = 0
					poly.foreach(e => if(e !=0){cpt = cpt+1})
					if(cpt>0)return false
							else return true
	}

	/**
  	*Fonction qui renvoie le polynome sous forme de string a partir d'une liste
  	*@param poly List[int]
 	*@return String
 	*/
	def prettyPrint(poly: List[Int]): String ={

			if(egalZero(poly)){
				return "0"
			}
			return sauterPlus(prettyPrintAux(poly.reverse, poly.length))
	}

	/**
  	*Fonction qui enleve les "+" d'une chaine
  	*@param s String
 	*@return String
 	*/	
	def sauterPlus(s : String): String ={
			if(s.toList.head == '+'){
				return s.toList.drop(1).mkString
			}
			else return s
	}


	def prettyPrintAux(poly: List[Int], indice: Int): String = (poly,indice) match{
	case (a::tail,indice) => cleanAffich(a,indice-1)+prettyPrintAux(tail,indice-1)
	case reste => ""
	}


	def cleanAffich(coef : Int, expo : Int) : String ={
			if(coef == 0){
				return ""
			}
			if(coef == 1){
				if(expo == 1){
					return "+X"
				}
				if(expo == 0){
					return "+"+coef
				}
				else return "+X^"+expo
			}
			if(coef == -1){
				if(expo == 1){
					return "-X"
				}
				if(expo == 0){
					return coef.toString
				}
				else return "-X^"+expo
			}
			if(coef > 1){
				if(expo == 1){
					return "+"+coef+"X"
				}
				if(expo == 0){
					return "+"+coef
				}
				else return "+"+coef+"X^"+expo
			}
			else{
				if(expo == 1){
					return coef+"X"
				}
				if(expo == 0){
					return coef.toString
				}
				else return coef+"X^"+expo
			}

	}

	/**
  	*Fonction qui ajoute un espace avant et apres la list de char
  	*@param char List[Char]
 	*@return List[Char]
 	*/
	def addSpace(charL : List[Char]) : List[Char] = List(' ') ::: charL ::: List(' ')


			def cherche1(charC : Char): List[Char]  = charC match{
			case ' ' | '+' | '-' | '*' | '/'=> return List(charC,'1')
			case _ => return List(charC)
	}

	/**
  	*Fonction qui rajoute des coefficient de 1 devant les X qui n'en on pas
	*@param valu Int  	
	*@param charL List[Char]
	*@param ret List[Char]
 	*@return List[Char]
 	*/
	def put1(valu : Int ,charL : List[Char],ret : List[Char]): List[Char] = {
			if (valu > charL.length-2){ return ret.tail}
			else{ charL(valu+1) match {
			case 'X'=> put1(valu +1,charL,ret:::(cherche1(charL(valu))))
			case _=> put1(valu +1,charL,ret:::List(charL(valu)))
			}
			}
	}


	/**
  	*Fonction qui rajoute des exposant de 1 apres les X qui n'en on pas
	*@param valu Int  	
	*@param charL List[Char]
	*@param ret List[Char]
 	*@return List[Char]
 	*/
	def putexp1(valu : Int ,charL : List[Char],ret : List[Char]): List[Char] = {
			if (valu > charL.length-2){ return ret.tail}
			else{ 
				if (charL(valu) == 'X' && charL(valu+1) != '^'){putexp1(valu +1,charL,ret:::(List(charL(valu),'^','1')))}
				else{putexp1(valu +1,charL,ret:::(List(charL(valu))))}
			}
	}

	/**
  	*Fonction qui rajoute des exposant 0 apres les reels
	*@param valu Int  	
	*@param charL List[Char]
	*@param ret List[Char]
 	*@return List[Char]
 	*/
	def putexp0(valu : Int ,charL : List[Char],ret : List[Char]): List[Char] = {
			if (valu > charL.length-2){ return ret.tail}
			else{ 
				if(charL(valu).getType == 9 && charL(valu+1).getType != 9 && charL(valu+1) != 'X' && charL(valu-1)!= '^'&& charL(valu-1).getType!= 9 || charL(valu).getType == 9 && charL(valu-1).getType == 9 && charL(valu+1).getType != 9 && charL(valu+1) != 'X')
 {putexp0(valu +1,charL,ret:::(List(charL(valu),'X','^','0')))}
				else{putexp0(valu +1,charL,ret:::(List(charL(valu))))}
			}
	}


	/**
  	*Fonction qui normalise une chaine de caracteres de type polynome.
  	*@param charL String
 	*@return String
 	*/
	def normaliser(charL : String):String = (putexp0(0,addSpace(putexp1(0,addSpace(put1(0,addSpace(charL.toList),List())),List())),List())).mkString

	
	/**
  	*Fonction qui develope les X en fonction de leurs coefficients
  	*@param s String
 	*@return String
 	*/	
	def developper(s : String): String = {
	var expr = "0"
			val poly = makePoly(s).toArray
			for(e <- 0 to poly.length-1){
				expr = expr+"+"+poly(e)+"*("+devperAux(e)+")"
			}
	return expr
	}

	def devperAux(nb : Int): String = nb match{
	case 0 => "1"
	case _ => "X*"+devperAux(nb-1)
	}

}
