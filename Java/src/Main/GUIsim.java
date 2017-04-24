import java.util.Random;
import javax.swing.*;
import java.util.ArrayList;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import javax.imageio.*;
import com.ericsson.opt.erlang.OptErlangRangeExcetion;
import com.ericsson.opt.erlang.OptErlangPid;
import Communication.JavaErlangCommunication;


public class GUIsim extends JPanel
{
	private static JFrame simulation;
	private static Background background;
	private static ArrayList<Unit> unitList;
	public static int xBound;
	public static int yBound;
	public static final int winX = 0;
	public static final int winY = 0;

	public GUIsim(int winW, int winH) {		
	}


	/**
	 * Running the program.
	 * @param args input from commandline.
	 */
	public static void runSimulation() throws InterruptedException {
		createUnitGraphics();
		background.validate();
		background.repaint();
		while(true) {
			ArrayList erlangList = javaErlangCommunicator.recievePos();
			if(erlangList == null) {
				break;
			}
			updateUnitGraphics(erlangList);
			background.validate();
			background.repaint();
		}
	}

	public static boolean isOutOfBounds(int xpos, int ypos) {
		if(xpos < 0 || xpos > xBound || ypos < 0 || ypos > yBound)
			return true;
		return false;
	}
		
	public static void initializeGUI() {
		backgroud  = new Background();
		background.image = javaErlangCommunicator.getMapImage();
		xBound     = background.image.getWidth(null);
		yBound     = background.image.getHeight(null);
		createUnitGraphics();
	}


	public static void createUnitGraphics() {
		int x,y,sickness,PID;
		while(erlangList != null) {
			ArrayList erlangList = javaErlangCommunicator.recievePos();
			//Wait for communication.
		}
		for(int i = 0; i < erlangList.size(); i++) {
			ArrayList unit = (ArrayList) erlangList.get(i);
			y = (Integer) unit.get(3);
			x = (Integer) unit.get(2);
			sickness = (Integer) unit.get(1);
			PID = (OptErlangPid) unit.get(0);
			Unit person = new Unit(PID, sickness, x, y);
			background.addUnit(person);
		}
	}

	public static void updateUnitGraphics(ArrayList erlangList) {
		int x,y,sickness,PID;
		ArrayList<Unit> updateList;
		for(int i = 0; i < erlangList.size(); i++) {
			ArrayList unit = (ArrayList) erlangList.get(i);
			y = (Integer) unit.get(3);
			x = (Integer) unit.get(2);
			sickness = (Integer) unit.get(1);
			PID = (OptErlangPid) unit.get(0);
			Unit person = new Unit(PID, sickness, x, y);
			updateList.add(person);
		}
		background.setUnitList(updateList);
	}

	public static void createAndShowGUI() {
		initializeGUI();
		JFrame frame = new JFrame("Project Snowfox");
		frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		frame.getContentPane().add(background);
		frame.setSize(background.image.getWidth(null), background.image.getHeight(null));
		frame.setVisible(true);
	}

 /**
 * The main class that will start and drive the program.
 * @author Project Snowfox
 */
	public static void main(String []args) throws InterruptedException, OtpErlangRangeException {
		//Command for creating bufferedimage of map
		createAndShowGUI();
		runSimulation();
	}
}
