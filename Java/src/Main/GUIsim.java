import javax.swing.*;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import javax.imageio.*;

public class GUIsim extends JPanel
{
	public static BufferedImage image;
	public static String map;
	public static final int windowPosX = 0;
	public static final int windowPosY = 0;


	public GUIsim()
	{
		super();
		try
		{
			image = ImageIO.read(new File(map));
		}
		catch (IOException e)
		{
			//Not handled.
		}
	}

	@Override
	public void paintComponent(Graphics g)
	{
		g.drawImage(image, windowPosX, windowPosY, null);
	}

	public static void main(String []args)
		throws InterruptedException
	{
		JFrame f = new JFrame("Window");
		map = args[0];
		f.getContentPane().add(new GUIsim());
		f.setSize(image.getWidth(), image.getHeight());
 		f.setExtendedState(JFrame.MAXIMIZED_BOTH);
		f.setUndecorated(true);
		f.setVisible(true);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		int i = 5;
		Unit person;
		person = new Unit(60, 0, windowPosX + i, windowPosY + i);
		f.add(person);
		while(true) {
			person.moveUnit(windowPosX + i,windowPosY + i,0);
			Thread.sleep(500);
			i+=3;
			
			person.moveUnit(windowPosX, windowPosY, 2);

			person.moveUnit(windowPosX + i,windowPosY + i,1);
			Thread.sleep(500);
			i+=3;
			
			person.moveUnit(windowPosX, windowPosY, 2);
		}
	}
}
