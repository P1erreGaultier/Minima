import javax.swing.*;
import java.awt.*;


@SuppressWarnings("serial")
public class Courbe extends JFrame
{


	//declaration du panel d'affichage des courbes
	private AffichageCourbe monAffichage;
	private fonction fonc;

	public Courbe(fonction fonction)
	{
		this.fonc = fonction;
		this.initialise();
		this.setVisible(true);
	}

	private void initialise()
	{
		//mise en place d'un container
		Container Content = this.getContentPane();
		int width = 500;
		int heigh = 530;
		setBounds(0,0,width, heigh);
		this.setResizable(false);
		//initialisation des constantes de dimensionnement "affichage"
		int X0 = 0;
		int Y0 = 0;
		int WidthC = 500;
		int HeighC = 500;

		//mise en place du panel
		monAffichage = new AffichageCourbe(X0,Y0,WidthC,HeighC,fonc);
		Content.add(monAffichage);
				
	}


}

