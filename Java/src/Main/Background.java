package Main;

import javax.swing.*;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import javax.imageio.*;

public class Background extends JPanel {

	public static BufferedImage image;
	public static String map;
	public static int windowPosX;
	public static int windowPosY;

	public Background(String name, int winX, int winY)
	{
		super();
		map = name;
		windowPosX = winX;
		windowPosY = winY;
		try
		{
			image = ImageIO.read(new File(map));
		}
		catch(IOException e)
		{
			//Not handlet.			
		}
	}

	@Override
	public void paintComponent(Graphics g)
	{
		g.drawImage(image, windowPosX, windowPosY, null);
	}
}
