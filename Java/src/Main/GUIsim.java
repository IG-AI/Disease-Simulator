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
	private ArrayList<Unit> unitList;
	public static final int winX = 0;
	public static final int winY = 0;

	public GUIsim(int winW, int winH) {
		unitList = createUnitGraphics(winW,winH);
		JPanel panel = new JPanel();
		for(Unit unit : unitList) {
			panel.add(unit);
		}
		setLayout(new BorderLayout());
		while(true) {
			for(Unit person : this.unitList) {
				person.moveUnit(person.x+1 , person.y+1, person.status);
			}
			panel.repaint();
		}
	}

	public void paintComponent(Graphics g) {
		for(Unit unit: unitList) {
			unit.paintComponent(g);
		}
	}
	/**
	 * Running the program.
	 * @param args input from commandline.
	 */
	public static void main(String []args) throws InterruptedException {
		//Command for creating bufferedimage of map
		Background map = new Background(args[0], winX, winY);
		JFrame f = new JFrame("Project-Snowfox");
		int winW = map.getXdim();
		int winH = map.getYdim();
		f.setSize(winW, winH);
		f.add(map);
		//f.add(new GUIsim(winW,winH));
		f.setExtendedState(JFrame.MAXIMIZED_BOTH);
		f.setUndecorated(false);
		f.setVisible(true);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	public ArrayList<Unit> createUnitGraphics(int winW, int winH) {
		ArrayList<Unit> newUnitList = new ArrayList<Unit>();
		int x,y,sickness,PID;
		Random random = new Random();
		for(int i = 0; i < 10; i++) {
			x = random.nextInt(winW);
			y = random.nextInt(winH);
			sickness = 1;
			PID = 600;
			Unit person = new Unit(PID, sickness, x, y);
			person.paint();
			newUnitList.add(person);
		}
		return newUnitList;
	}
}
