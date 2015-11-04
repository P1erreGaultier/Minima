import javax.swing.*;
import javax.swing.border.*;
import java.awt.event.*;
import java.awt.*;


public class AffichageCourbe extends JPanel implements MouseListener, MouseMotionListener
{

	private static final long serialVersionUID = -238150595685307518L;
	public int nombrePoints = 500;
	public double pointA = -10.0;
	public double pointB = 10.0;
	private CourbeMath maCourbe;
	private int pointX[];
	private int pointY[];
	private int pointCurseur = 0;
	private int xMaxRPixel;
	private int xMinPixel;
	private int yMaxRPixel;
	private int yMinPixel;
	private fonction fonc;


	public AffichageCourbe(int X0 , int Y0 , int Width , int Heigh,fonction fonc)
	{
		this.fonc= fonc;
		addMouseListener(this);
		addMouseMotionListener(this);
		//initialisation des variables de dimensions
		xMinPixel = X0;
		yMinPixel = Y0;
		xMaxRPixel = Width;
		yMaxRPixel = Heigh;

		setBorder(new LineBorder(new Color(0, 0, 0)));
		setBackground(new Color(200, 200, 200));
		setBounds(xMinPixel,yMinPixel,xMaxRPixel+xMinPixel+20,yMaxRPixel+yMinPixel+20);
		maCourbe = new CourbeMath(nombrePoints,pointA,pointB,fonc);
		miseEnValeur();
	}

	public void paintComponent( Graphics monGraphe )
	{
		super.paintComponent(monGraphe);
		afficher(monGraphe);
	}

	private void afficher(Graphics monGraphe)
	{
		

		monGraphe.setColor(new Color(51, 0, 255));
		monGraphe.drawPolyline(pointX,pointY,nombrePoints);
		
		
		/*lignes sur la courbes*/
		monGraphe.setColor(new Color(0,150,0));
		monGraphe.drawLine(pointX[pointCurseur],yMinPixel-20,pointX[pointCurseur],yMaxRPixel+yMinPixel+20);
		monGraphe.drawLine(xMinPixel-20,pointY[pointCurseur],xMaxRPixel+yMinPixel+20,pointY[pointCurseur]);
		/*repere*/
		monGraphe.setColor(Color.BLACK);
		monGraphe.drawLine(xMaxRPixel/2,yMaxRPixel,xMaxRPixel/2,0);// courbe y
		
		int origine = -10;
		for(int i=1; i<500; i++){
			//if(maCourbe.mesPointsY[i]==0.) { origine = i;}
			//if (maCourbe.mesPointsY[i] == 0){System.out.println(maCourbe.mesPointsX[i]);}
			if(maCourbe.mesPointsY[i-1]>0 && maCourbe.mesPointsY[i]<0
					||maCourbe.mesPointsY[i-1]<0 && maCourbe.mesPointsY[i]>0
					||maCourbe.mesPointsY[i]==0)
			{ origine = i;}
		}
			if (origine == -10){
				if (maCourbe.mesPointsY[1] > 0){
					origine=(int) (250 - maCourbe.yMin()*nombrePoints/2/maCourbe.getPointB());
				}
				else{
					origine=(int) (250 + maCourbe.yMax()*nombrePoints/2/maCourbe.getPointB());
					
				}
				
			}
		monGraphe.drawLine(0,500 - origine,xMaxRPixel,500 - origine);// courbe x
		
		monGraphe.drawString("("+(float)maCourbe.mesPointsX[pointCurseur]+";"+ (float)maCourbe.mesPointsY[pointCurseur]+")",pointX[pointCurseur]+10,pointY[pointCurseur]-11);
		

	}

	public void miseAJour()
	{
		pointCurseur = 0;
		maCourbe = new CourbeMath(nombrePoints,pointA,pointB,fonc);
		miseEnValeur();
		repaint();
	}

	private void miseEnValeur()
	{
		pointX = new int[nombrePoints];
		pointY = new int[nombrePoints];
		
		int decalage = yMaxRPixel/2-(int)((maCourbe.yMax()-maCourbe.mesPointsY[250])*yMaxRPixel/(maCourbe.yMax()-maCourbe.yMin()))+yMinPixel;
		
		decalage=(int) (decalage-maCourbe.mesPointsY[250]);

		for( int i = 0 ; i < nombrePoints ; i++)
		{
			pointX[i] = (int)(xMinPixel + ((i*xMaxRPixel) / (nombrePoints-1)));
			pointY[i] = decalage+(int)( ( maCourbe.yMax() - maCourbe.mesPointsY[i] ) * yMaxRPixel / ( maCourbe.yMax() - maCourbe.yMin() ) ) + yMinPixel;
		}
	}

	public void mousePressed(MouseEvent e)
	{
		pointCurseur = recherchepointCurseur(e.getX());
		repaint();
	}

	public void mouseDragged(MouseEvent e)
	{
		pointCurseur = recherchepointCurseur(e.getX());
		repaint();
	}

	public void mouseClicked(MouseEvent e){}

	public void mouseReleased(MouseEvent e){}

	public void mouseEntered(MouseEvent e){}

	public void mouseExited(MouseEvent e){}

	public void mouseMoved(MouseEvent e){}


	private int recherchepointCurseur( int monpointX )
	{
		for( int i = 0 ; i < nombrePoints ; i++) if( pointX[i] >= monpointX ) return i;
		return nombrePoints-1;
	}

}
