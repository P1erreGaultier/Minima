class Noyau {

    

def addSpace(charL : List[Char]) : List[Char] = List(' ') ::: charL ::: List(' ')


def cherche1(charC : Char): List[Char]  = charC match{
	case ' ' | '+' | '-' | '*' | '/'=> return List(charC,'1')
	case _ => return List(charC)
	}


def put1(valu : Int ,charL : List[Char],ret : List[Char]): List[Char] = {
	if (valu > charL.length-2){ return ret.tail}
	else{ charL(valu+1) match {
		case 'X'=> put1(valu +1,charL,ret:::(cherche1(charL(valu))))
		case _=> put1(valu +1,charL,ret:::List(charL(valu)))
		}
	}
}


def putexp1(valu : Int ,charL : List[Char],ret : List[Char]): List[Char] = {
	if (valu > charL.length-2){ return ret.tail}
	else{ 
		if (charL(valu) == 'X' && charL(valu+1) != '^'){putexp1(valu +1,charL,ret:::(List(charL(valu),'^','1')))}
		else{putexp1(valu +1,charL,ret:::(List(charL(valu))))}
		}
}

def putexp0(valu : Int ,charL : List[Char],ret : List[Char]): List[Char] = {
	if (valu > charL.length-2){ return ret.tail}
	else{ 
		if (charL(valu).getType == 9 && charL(valu+1).getType !=9 && charL(valu+1) != 'X'&& charL(valu-1) != '^'){putexp0(valu +1,charL,ret:::(List(charL(valu),'X','^','0')))}
		else{putexp0(valu +1,charL,ret:::(List(charL(valu))))}
		}
}



// Transforme la chaine de caract�re en polynome normalis�
def normaliser(charL : String):String = (putexp0(0,addSpace(putexp1(0,addSpace(put1(0,addSpace(charL.toList),List())),List())),List())).mkString





def extractSymbols(char: Char): Char = char match {

case '+' => 'p'
case '-' => 'm'
case '^' => 'e'
case reste => reste
}

def remettreMoins(char: Char): Char = char match {

case 'm' => '-'
case reste => reste
}

def convertMoins(expr: String): String ={
expr.toList.map(remettreMoins(_)).mkString
}

def convertExpression(expr: String): String ={
(expr.toList.map(extractSymbols(_)):+"z").mkString
}

// V�rifie si le "normaliser" est un bon polynome
def checkIfPoly(inputPoly: String): Boolean ={
if(inputPoly != ""){
val polyRegex = "((m?([1-9]?[0-9])?z)|(m?([1-9]?[0-9])?X((p|m)([1-9]?[0-9])?)?z)|(m?([1-9]?[0-9])?Xe([1-9]?[0-9])((m|p)([1-9]?[0-9])?Xe([1-9]?[0-9]))*((m|p)([1-9]?[0-9])?X)?((m|p)([1-9]?[0-9]))?z))".r
val output = polyRegex.findFirstIn(convertExpression(inputPoly))
if(inputPoly.length > 0 && output.get.length == convertExpression(inputPoly).length)
{
return true
} else {return false}
}
else{return false}
}

def extractCoef(input: String): List[Int] ={
val coefRegex = "((p?|m)[1-9]?[0-9]X)|((p|m)[1-9]?[0-9])?z".r
val coefString = coefRegex.findAllIn(convertExpression(input)).toList.mkString
val justCoefRegex = "-?[1-9]?[0-9]".r
return justCoefRegex.findAllIn(convertMoins(coefString)).toList.map(_.toInt)
}

def extractExpo(input: String): List[Int] ={
val expoRegex = "(e[1-9]?[0-9](p|m))|(e[1-9]?[0-9])z".r
val expoString = expoRegex.findAllIn(convertExpression(input)).toList.mkString
val justExpoRegex = "[1-9]?[0-9]".r
return justExpoRegex.findAllIn(expoString).toList.map(_.toInt)
}

//Transforme le poly en liste
def makePoly(input: String): List[Int] = {
var output = List[Int]()
var indice = 0
for(i <- 0 to 3){
if(extractExpo(input).contains(i)){
output = extractCoef(input).reverse(indice)::output
indice = indice +1
}
else{output = 0::output}
}
return output.reverse
}

def prettyPrint(poly: List[Int]): String ={
return sauterPlus(prettyPrintAux(poly.reverse, poly.length))
}

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

def main(args: Array[String]): Unit = {

println(checkIfPoly("-3X^2+2"))
println(checkIfPoly("4X^3+2X^2+8"))
println(checkIfPoly("3X+2"))
println(checkIfPoly("-3X"))
println(checkIfPoly("-3"))
println(checkIfPoly("4"))
println(checkIfPoly("coucou"))
println(checkIfPoly("-3XXX"))
println(checkIfPoly("+-3X"))
println(checkIfPoly("+-"))
println(extractExpo("4X^3+2X^2-8X^0"))
println(extractCoef("4X^3+2X^2-8X^0"))
println(makePoly("4X^3+2X^2-8X^0"))
println(prettyPrint(makePoly("4X^3+2X^2-8X^0")))
println(normaliser("X+3"))
}
}
