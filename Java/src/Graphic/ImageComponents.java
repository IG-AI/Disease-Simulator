package Graphic;

import java.util.ArrayList;
import javax.swing.*;
import java.awt.*;

/**
 * The ImageComponents class of the program, that will handle the background.
 * @author Project Snowfox
 */
public class ImageComponents extends JPanel {
	private Image image;
	public static int windowPosX;
	public static int windowPosY;
	public ArrayList<Unit> unitList = new ArrayList<Unit>();


	/**
	 * Constructor for the ImageComponents class.
	 * @param winX x-position as an int.
	 * @param winY y-position as an int.
	 */
	public ImageComponents(int winX, int winY)
	{
		super();
	}


	/**
	 * Adds a Unit.
	 * @param unit as a Unit class
	 */
	public void addUnit(Unit unit) {
		unitList.add(unit);
		repaint();
	}


	/**
	 * Placing a list of Units in the variable unitList.
	 * @param unitList a ArrayList with all Units.
	 */
	public void setUnitList(ArrayList<Unit> unitList) {
		this.unitList = unitList;
	}


	/**
	 * Creating the dimension for the background.
	 * @return the new dimension as a Dimension class.
	 */
	@Override
	public Dimension getPreferredSize() {
		return (new Dimension(image.getWidth(null), image.getHeight(null)));
	}


	/**
	 * Setting the image
	 * @param image as a Image class.
	 */
	public void setImage(Image image) {
		this.image = image;
	}


	/**
	 * Returning the height of the image.
	 * @return the height of the image as an int.
	 */
	public int getHeight() {
		return image.getHeight(null);
	}


	/**
	 * Returning the width of the image.
	 * @return the width of the image as an int.
	 */
	public int getWidth() {
		return image.getWidth(null);
	}


	/**
	 * Painting the components.
	 * @param g as a Graphics object.
	 */
	@Override
	public void paintComponent(Graphics g)
	{
		g.drawImage(image, windowPosX, windowPosY, null);
		if(!unitList.isEmpty()) {
			int i = 0, unitListSize = unitList.size();
			while (i < unitListSize) {
				Unit unit = unitList.get(i);
				unit.paintComponent(g);
				i++;
			}
		}
	}
}