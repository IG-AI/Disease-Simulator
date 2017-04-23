import java.util.Random;
import javax.swing.*;
import java.util.ArrayList;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import javax.imageio.*;




/**
 * The main class that will start and drive the program.
 * @author Project Snowfox
 */
public class GUIsim extends JPanel
{
	private static String mapname;
	private static JFrame simulation;
	private static Background background;
	private static ArrayList<Unit> unitList;
	public static int xBound;
	public static int yBound;
	public static final int winX = 0;
	public static final int winY = 0;

	public GUIsim(int winW, int winH) {		
	}

	public static void runSimulation() throws InterruptedException {
		int i = 0;
		while(i < 1000) {
			Random random = new Random();
			for(Unit person : background.units) {
				if(isOutOfBounds(person.x + 1, person.y + 1)) {
					person.moveUnit(random.nextInt(xBound), random.nextInt(yBound), 1);
				}
				else {
					person.moveUnit(person.x + 1, person.y + 1, random.nextInt(2));
				}
				background.validate();
				background.repaint();
				Thread.sleep(1);
			}
			++i;
		}
	}

	public static boolean isOutOfBounds(int xpos, int ypos) {
		if(xpos < 0 || xpos > xBound || ypos < 0 || ypos > yBound)
			return true;
		return false;
	}
		
	public static void initializeGUI() {
		background = new Background(mapname, winX, winY);
		xBound     = background.image.getWidth(null);
		yBound     = background.image.getHeight(null);
		createUnitGraphics(xBound, yBound);
	}


	public static void createUnitGraphics(int winW, int winH) {
		int x,y,sickness,PID;
		Random random = new Random();
		for(int i = 0; i < 10; i++) {
			x = random.nextInt(winW);
			y = random.nextInt(winH);
			sickness = 1;
			PID = 600;
			Unit person = new Unit(PID, sickness, x, y);
			background.addUnit(person);
		}
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
	 * Running the program.
	 * @param args input from commandline.
	 */
	public static void runBackground() {
		background = new Background(mapname, winX, winY);
		JFrame frame = new JFrame("");
		frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		frame.getContentPane().add(background);
		frame.setSize(background.image.getWidth(null),background.image.getHeight(null));
		frame.setVisible(true);
	}
	
	public static void main(String []args) throws InterruptedException {
		//Command for creating bufferedimage of map
		mapname = args[0];
		createAndShowGUI();
		runSimulation();
	}
}
