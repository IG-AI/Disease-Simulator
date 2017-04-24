import java.util.ArrayList;
import javax.swing.*;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import javax.imageio.*;

public class Background extends JPanel {
	public static Image image;
	public static String map;
	public static int windowPosX;
	public static int windowPosY;
	public ArrayList<Unit> units = new ArrayList<>();

	public Background(String name, int winX, int winY)
	{
		super();
	}

	public void addUnit(Unit unit) {
		units.add(unit);
		repaint();
	}

	public void setUnitList(ArrayList<Unit> unitList) {
		units = unitList;
	}

	@Override
	public void paintComponent(Graphics g)
	{
		g.drawImage(image, windowPosX, windowPosY, null);
		if(!units.isEmpty()) {
			for(Unit unit : units) {
				unit.paintComponent(g);
			}
		}
	}
}
