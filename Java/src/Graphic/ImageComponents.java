package Graphic;

import java.util.ArrayList;
import javax.swing.*;
import java.awt.*;

/**
 * The ImageComponents class of the program, that will handle the background.
 * @author Project Snowfox
 */
public class ImageComponents extends JPanel {
	private Image image = null;
	public static final int windowPosX = 0;
	public static final int windowPosY = 0;
	public static ArrayList<Unit> unitList = new ArrayList<>();
	public static int sickUnits;
	public static int vaccinatedUnits;
	public static int ticks;
	public static int tempSickUnits;
	public static int tempVacciantedUnits;


	/**
	 * Constructor for the ImageComponents class.
	 */
	public ImageComponents()
	{
		super();
	}


	/**
	 * Adds a Unit.
	 *
	 * @param unit as a Unit class
	 */
	public void addUnit(Unit unit) {
		unitList.add(unit);
	}


	/**
	 * Placing a list of Units in the variable unitList.
	 *
	 * @param unitList a ArrayList with all Units.
	 */
	protected void setUnitList(ArrayList<Unit> unitList) {
		ImageComponents.unitList = unitList;
	}


	/**
	 * Creating the dimension for the background.
	 *
	 * @return the new dimension as a Dimension class.
	 */
	@Override
	public Dimension getPreferredSize() {
		return (new Dimension(image.getWidth(null), image.getHeight(null)));
	}


	/**
	 * Setting the image
	 *
	 * @param image as a Image class.
	 */
	protected void setImage(Image image) {
		this.image = image;
	}


	/**
	 * Returning the height of the image.
	 *
	 * @return the height of the image as an int.
	 */
	public int getHeight() {
		return image.getHeight(null);
	}


	/**
	 * Returning the width of the image.
	 *
	 * @return the width of the image as an int.
	 */
	public int getWidth() {
		return image.getWidth(null);
	}


	/**
	 * Painting the components.
	 *
	 * @param g as a Graphics object.
	 */
	@Override
	public void paintComponent(Graphics g)
	{

		g.drawImage(image, windowPosX, windowPosY, null);
		if(!unitList.isEmpty()) {
			vaccinatedUnits = 0;
			sickUnits = 0;
			int i = 0, unitListSize = unitList.size();
			while (i < unitListSize) {
				Unit unit = unitList.get(i);
				unit.paintComponent(g);
				if (unit.status == 1) {
					sickUnits++;
				} else if (unit.status == 2) {
					vaccinatedUnits++;
				}
				i++;
			}
		}
		tempSickUnits = sickUnits;
		tempVacciantedUnits = vaccinatedUnits;
		ticks++;
	}
}
