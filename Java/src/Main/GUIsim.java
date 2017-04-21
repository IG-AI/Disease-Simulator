import javax.swing.*;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import javax.imageio.*;

public class GUIsim extends JPanel
{
	public static final int winX = 0;
	public static final int winY = 0;

	public static void main(String []args)
		throws InterruptedException
	{
		Background map = new Background(args[0], winX, winY);
		JFrame f = new JFrame("Project-Snowfox");
		f.setContentPane(map);
		f.setSize(map.getWidth(), map.getHeight());
		f.setExtendedState(JFrame.MAXIMIZED_BOTH);
		f.setUndecorated(true);
		f.setVisible(true);
		System.out.println("Här finns: " + f.getComponentCount());
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		int i = 5;
		Unit person;
		person = new Unit(60, 0, winX + i, winY + i);
		f.add(person);
		while(true) {
			person.moveUnit(winX + i, winY + i,0);
			Thread.sleep(500);
			i+=3;
			person.moveUnit(winX + i,winY + i,1);
			Thread.sleep(500);
			i+=3;
			System.out.println(f.getComponentCount());
		}
	}
}
