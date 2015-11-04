
object Minima {


	def choixOperation(s: String): Int = s match {

	case "1" => 1
	case "2" => 2
	case "3" => 3
	case "4" => 4
	case "quitter" => 5
	case _ => println("Veuillez recommencer"); choixOperation(Console.readLine)

	}

	def choixSuite(s: String): Int = s match {

	case "1" => 1
	case "2" => 2
	case "3" => 3
	case "quitter" => 4
	case _ => println("Veuillez recommencer"); choixOperation(Console.readLine)

	}

	def traitement(poly1:List[Int],poly2:List[Int],op: Int): String =op match{

	case 1 => ((new NoyauPoly).prettyPrint((new Polynome(poly1)+(new Polynome(poly2))).coefs))
	case 2 => ((new NoyauPoly).prettyPrint((new Polynome(poly1)*(new Polynome(poly2))).coefs))
	case 3 => ((new NoyauPoly).prettyPrint((new Polynome(poly1)-new Polynome(poly2)).coefs))
	case 4 => ((new NoyauPoly).prettyPrint((new Polynome(poly1).deriver).coefs))
	}

	def main(args: Array[String]): Unit = {

			var quitter = false
					val noyau = new NoyauPoly
					var poly1 = List[Int]()
					var poly2 = List[Int]()
					var result = List[Int]()


					while(!quitter){

						println("Bienvenue sur le mini-calculateur formel Minima !")
						println("Pour quitter a tout moment tapez 'quitter'")
						println("")


						// saisie poly 1
						println("Veuillez rentrer le premier polynome :")
						var sortie1 = false
						while(!sortie1){
							var entre1 = Console.readLine

									if(noyau.checkIfPoly(entre1)){
										poly1 = noyau.makePoly(entre1)
												sortie1 = true
									}
									else{
										if(entre1.equals("quitter")) {sys.exit}
										else{ println("Erreur de saisie, veuillez recommencer !")}
									}
						}


						/// choix operation
						println("Quelle operation voulez vous effectuer ? (taper le nombre correspondant)")
						println("1-Addition")
						println("2-Multiplication")
						println("3-Soustraction")
						println("4-Derivee")
						var op = choixOperation(Console.readLine)

						if(op==5){sys.exit}

						// saisie poly 2
						if(op!=4){
							println("Veuillez rentrer le deuxieme polynome :")
							var sortie2 = false
							while(!sortie2){
								var entre2 = Console.readLine

										if(noyau.checkIfPoly(entre2)){
											poly2 = noyau.makePoly(entre2)
													sortie2 = true
										}
										else{
											if(entre2.equals("quitter")) sys.exit
											else println("Erreur de saisie, veuillez recommencer !")
										}
							}
						}

						println("Resultat : ")
						println(traitement(poly1,poly2,op))

						var choix = 0

						while(choix !=1){

							println("Que voulez vous faire ? (taper le nombre correspondant)")
							println("1-Recommencer")
							println("2-Afficher les racines")
							println("3-Resoudre")
							println("quitter")

							choix = choixSuite(Console.readLine)


							if(choix==4){sys.exit}

							//traitement
							if(choix==2){
								println("Racines :")
								var racine = new Polynome(noyau.makePoly(traitement(poly1,poly2,op))).resoudre
								racine.length match {
								case (0) => println("Il n'y a pas de solutions ou il n'est pas possible de les calculer")
								case (1) => println("La solution est : " + racine(0))
								case (2) => println("Les solutions sont : " + racine(0) + " " + racine(1))
								case (3) => println("Les solutions sont : " + racine(0) + " " + racine(1) + " " + racine(2))
								case (4) => println("Les solutions sont : " + racine(0) + " " + racine(1) + " " + racine(2) + " " + racine(3))
								}

							}
							if(choix==3){
								println("Entrez votre valeur de X :")
								var x = Console.readLine
								if (x.length==1 && (x.toList(0).getType == 9 || x.toList(0).getType == 20)){

									println("L'image pour la valeur de X que vous avez donne est : " +  new Polynome(noyau.makePoly(traitement(poly1,poly2,op))).calculer(x.toDouble))
								}
								else {
									println("L'entree X n'est pas valide ou il n'y pas de polynome auquel appliquer X")
								}

							}

						}
					}
	}

}