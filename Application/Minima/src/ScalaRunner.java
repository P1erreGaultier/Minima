import java.io.IOException;
import java.util.Scanner;


public class ScalaRunner {


	public static void main(String[] args) throws IOException {
		System.out.println("Bienvenue sur le mini-calculateur formel Minima !");
		System.out.println("Pour le mode console, tapez 1");
		System.out.println("Pour le mode Graphique, tapez 2");

		Scanner S = new Scanner(System.in);
		String entre = S.nextLine();
					if (entre.equals("1")){
						Minima.main(args);
						}
					if (entre.equals("2")){
						Reactive.main(args);
					}
						
					else{
						System.out.println(entre);
						System.out.println(entre.getClass());

						System.exit(0);
					}
						
				
				}
				
				

  


}
