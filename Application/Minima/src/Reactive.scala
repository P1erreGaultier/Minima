import swing._
import event._
import javax.swing._


object Reactive extends SimpleSwingApplication {
  

  def top = new MainFrame {
    
//    background= new java.awt.Color(150,10,232) 
	  centerOnScreen()
          
    // Definition des differents composants de la fenetre
    title = "Minima le calculateur formel trop genial"
    
     val button = new Button {
      text = "Calculer"
    }
    
    object unPolynome extends TextField { columns = 10
        }

    val boutracine = new Button {
      text = "Obtenir racines"
    }
    
    val boutCourbe = new Button {
      text = "Voir courbe associee"
    }
    
    object autrePoly extends TextField { columns = 10
    }
    
    val operationSelect = new ComboBox(List("Addition","Soustraction","Multiplication","Derivation")) { }
    
    object reponse extends TextField { 
      editable_=(false)
      columns = 10
      border = new javax.swing.border.LineBorder(java.awt.Color.BLACK)
    }
    
     val rep  = new Label {
       text = ""      
     }
     
     val image = new TextField{
       editable_=(false)
       columns = 10
       text = "Entrer une valeur de X" 
     }
     
     val boutImage = new Button{
       text = "Calculer image"
     }
     
          
//     val p        = new ImagePanel( 23, 23 )
//     p.imagePath  = ""
//     p.contents ++= Seq.tabulate( p.rows * p.columns )( i => new Label( (i + 1).toString ))
    
     
    // Definition des differentes zones de la fenetre
     
    // Zone oe se se situe la liste d'operation et les Area pour donner les polynomes  
    val gridP = new GridPanel(1,3) {
      contents += unPolynome
      contents += operationSelect
      contents +=autrePoly
      
      border = Swing.EmptyBorder(30, 30, 20, 30)
    }
    
    // Zone oe se situe le Bouton
    val calcul = new BoxPanel(Orientation.Vertical){
      contents += button
    		  
      border = Swing.EmptyBorder(0, 30, 20, 40)
    }
    
    // Zone oe s'affichera le resultat
    val resultat = new BoxPanel(Orientation.Vertical){
      contents += reponse
    
      border = Swing.EmptyBorder(0, 30, 30, 30)
    }
    
    // Zone bouton racines & courbe
    val racourbe = new GridPanel(2,2){
      xLayoutAlignment=0.5
      contents += boutracine
      contents += boutCourbe
      contents += image
      contents += boutImage
      
      border = Swing.EmptyBorder(0, 30, 20, 30)

    }

    
    
    // Association des zones 
    	contents = new BoxPanel(Orientation.Vertical) {
    	contents += gridP
    	contents += calcul
    	contents += resultat
    	contents += racourbe
//      contents += p
    }
     
    

    var i = false  
    var j = false
    var v = true
    
    // Creation du noyau
    var n1 = new NoyauPoly()
    // On transforme le Polynome en List
//    var p1 = new Polynome(n1.makePoly(n1.normaliser(unPolynome.text)))
    // On affiche la List
//    println (n1.prettyPrint(p1.coefs))
     
//    var p2 = new Polynome(n1.makePoly(n1.normaliser(autrePoly.text)))
//     println (n1.prettyPrint(p2.coefs))
    
//     println (n1.prettyPrint(p2.add(p1).coefs))
    
    def identifyOperation(op:String, p1:Polynome, p2:Polynome){
    	op match{
    	case ("Addition") => reponse.text_=(n1.prettyPrint(p2.+(p1).coefs))
    	case ("Soustraction") => reponse.text_=(n1.prettyPrint(p1.-(p2).coefs))
    	case ("Multiplication") => reponse.text_=(n1.prettyPrint(p2.*(p1).coefs))
    	case ("Derivation") =>  reponse.text_=(n1.prettyPrint(p1.deriver().coefs))
    	}
    }
    
    
    def fraction(d:Double): String = {
      var res =""
      for(i <- 0 to 500){
        for(j <- 1 to 500){
          if(i/j==d){
            res = i+"/"+j
          }
        }
      }
       return res
    }
    
    
    listenTo(unPolynome,button,autrePoly,operationSelect.selection,boutCourbe,boutracine,boutImage)    
    reactions += {
      case EditDone(`autrePoly`) => j = true
      case EditDone(`unPolynome`) => i = true  
      
      case ButtonClicked(`boutCourbe`) =>
        if(reponse.text!=""){
        new Courbe(new fonction(n1.developper(reponse.text)))
        }
        else {
          Dialog.showMessage(rep,"Il faut un polynome")
        }
        
      case ButtonClicked(`boutImage`) =>
        if (image.text!="" && reponse.text!="" && n1.checkIfPoly(image.text)==true){
        val p4 = new Polynome (n1.makePoly(reponse.text))
        val x = image.text.toDouble
        Dialog.showMessage(rep, "L'image pour la valeur de X que vous avez donne est : " + p4.calculer(x))
        }
        else {
          Dialog.showMessage(rep,"L'entree X n'est pas valide ou il n'y pas de polynome auquel appliquer X")
        }
      
      case ButtonClicked(`boutracine`) => 
        val p3 = new Polynome (n1.makePoly(reponse.text))
        var racine = p3.resoudre
        racine.length match {
          case (0) => Dialog.showMessage(rep,"Il n'y a pas de solutions ou il n'est pas possible de les calculer")
          case (1) => Dialog.showMessage(rep,"La solution est : " + racine(0).toString)
          case (2) => Dialog.showMessage(rep,"Les solutions sont : " + racine(0).toString + " et " + racine(1).toString)
          case (3) => Dialog.showMessage(rep,"Les solutions sont : " + racine(0).toString + " et " + racine(1).toString + " et " + racine(2).toString)
          case (4) => Dialog.showMessage(rep,"Les solutions sont : " + racine(0).toString + " et " + racine(1).toString + " et " + racine(2).toString + " et " + racine(3).toString)
        }
        
        
      case SelectionChanged(`operationSelect`) =>
        val selected = operationSelect.selection.item
        if (selected=="Derivation"){
          autrePoly.editable_=(false)
          autrePoly.text_=("")
        }
        else {
          autrePoly.editable_=(true)
        }
        
        
      case ButtonClicked(`button`) =>
        val selected = operationSelect.selection.item
        if (i==j==true && unPolynome.text!="" && autrePoly.text!="") {
          if(n1.checkIfPoly(unPolynome.text) == true && n1.checkIfPoly(autrePoly.text) == true){// Appel des fonctions du noyau pour verifier que c'est bien un polynome qui est entre
            image.editable_=(true)
            image.text=""
            var p1 = new Polynome(n1.makePoly(unPolynome.text))
            var p2 = new Polynome(n1.makePoly(autrePoly.text))
            identifyOperation(selected,p1,p2)
          }
          else {
            Dialog.showMessage(rep,"Vous avez fait un erreur de syntaxe")
          }
        }
        else if (i==true && n1.checkIfPoly(unPolynome.text) == true && selected=="Derivation"){
        	image.editable_=(true)
            image.text=""
        	var p1 = new Polynome(n1.makePoly(unPolynome.text))
        	identifyOperation(selected,p1,p1)
        }
        else { Dialog.showMessage(rep,"Il faut saisir deux polynomes");
        }
    }
  }
}






