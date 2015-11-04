public class CourbeMath
{
	public double mesPointsY[];
	public double mesPointsX[];
	public int nombrePoints;
	private double pas;
	private double pointA;
	private double pointB;
	private fonction fonc;

	public CourbeMath(int Nb , double A , double B,fonction Fonc)
	{
		nombrePoints = Nb;
		fonc = Fonc;
		setPointA(A);
		pointB = B;
		discretisation();
		calculPoints();
	}

	private void discretisation()
	{
		pas = (pointB - getPointA() ) / nombrePoints;
	}

	private void calculPoints()
	{
		mesPointsY = new double[nombrePoints];
		mesPointsX = new double[nombrePoints];

		for(int i = 0 ; i < nombrePoints ; i++)
		{
			mesPointsX[i] = getPointA()+(i*pas);
			mesPointsY[i] = fonc.valeurY(getPointA()+(i*pas));
		}
	}

	public double yMax()
	{
		double MonMax = mesPointsY[0];
		for(int i = 1 ; i < nombrePoints ; i++) if( mesPointsY[i] > MonMax ) MonMax = mesPointsY[i];
		return MonMax;
	}

	public double yMin()
	{
		double MonMin = mesPointsY[0];
		for(int i = 1 ; i < nombrePoints ; i++) if( mesPointsY[i] < MonMin ) MonMin = mesPointsY[i];
		return MonMin;
	}

	public double getPointA() {
		return pointA;
	}

	public void setPointA(double pointA) {
		this.pointA = pointA;
	}

	public double getPointB() {
		return pointB;
	}

} 
