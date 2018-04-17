package srcr1;

import java.util.HashMap;

import se.sics.jasper.Query;
import se.sics.jasper.SICStus;
import se.sics.jasper.SPException;

public class srcr {

	
	
	//Java Object to Interact with SICStus virtual Machine
	SICStus sp;
	//Initialize SICStus virtual machine
	public void loadSICStus() throws SPException {
	sp = new SICStus();
	}
	//Load SICStus script
	public void loadSICStusScrpt(String pathToFile) throws SPException
	{
	sp.load(pathToFile);
	}
	

	public static void main(String [] args) throws NoSuchMethodException, InterruptedException, Exception
	{
		SICStus sp = new SICStus();
		
		sp.load("C:\\Users\\Admin-PC\\Downloads\\SRCR\\tp2.pl");
	
		String queryS = "demoC(utente(1,1,1,1) e utente(1,1,1,1),R).";
		
		HashMap map = new HashMap();
		Query query = sp.openPrologQuery(queryS,map);
		while (query.nextSolution()) {
		System.out.println(map.toString());
		}
		query.close();

		
		
	}

	
	
	
}