import javax.swing.*;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import javax.imageio.*;

public class Converter extends JPanel
{
	public static BufferedImage image;
	public static String map;

	public Converter ()
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

	public void paintComponent(Graphics g)
	{
		g.drawImage(image, 0, 0, null);
		repaint();
	}

	public static void main(String [] args)
	{
		JFrame f = new JFrame("Window");
		map = args[0];
		f.getContentPane().add(new Converter());
		f.setSize(image.getWidth(), image.getHeight());
		f.setVisible(true);
		f.setDefualtCloseOperation(JFrame.EXIT_ON_CLOSE);
		Unit peepl = new Unit(6332, 0, 1, 1);
		Unit peeplsick = new Unit(6444, 1, 2, 2);	
		peepl.moveUnit(20,20,0);
		peeplsick.moveUnit(40,40,0);
	}
}
