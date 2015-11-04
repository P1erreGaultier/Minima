import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class fonction {

	private String fonc;
	private double res;
	private String fonc2;

	public fonction(String fonc) {
		this.fonc = fonc;
	}

	public double valeurY(Double X)
	{

		res = 0.;
		String XX = String.valueOf(X);
		fonc2 = fonc.replaceAll("X", XX);
		ScriptEngineManager mgr = new ScriptEngineManager();
		ScriptEngine engine = mgr.getEngineByName("JavaScript");
		String myJSCode = this.fonc2;//+"+"+String.valueOf(decalage);
		
		try {
			
			res = (Double) ((engine.eval(myJSCode)));
			/* if(X==0){System.out.println(res);}
if(res>250.){res = 0.;}*/

		} catch (ScriptException e) {
			e.printStackTrace();
			System.out.println(e.getMessage());
		}
		return res;
	}

}