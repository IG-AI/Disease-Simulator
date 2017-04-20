package Main;

import javax.swing.*;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import java.util.ArrayList;
import javax.imageio.*;
import Communication.JavaErlangCommunication;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;

/**
 * The main class that will start and drive the program.
 * @author Project Snowfox
 */
public class GUIsim extends JPanel
{
	public static BufferedImage image;
	public static String map;
	public static final int windowPosX = 0;
	public static final int windowPosY = 0;

	/**
	 * Constructor to GUIsim. It'll try to read in a location of a picture, to load as a background.
	 */
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

	/**
	 * Drawing the background with the loaded picture and with start position as zero.
	 * @param g a graphics object.
	 */
	@Override
	public void paintComponent(Graphics g)
	{
		g.drawImage(image, windowPosX, windowPosY, null);
	}

	/**
	 * Running the program.
	 * @param args input from commandline.
	 */
	public static void main(String []args) throws InterruptedException, OtpErlangRangeException {
		JavaErlangCommunication javaErlangCommunicator = new JavaErlangCommunication();
		map = javaErlangCommunicator.getMapName();
		JFrame f = new JFrame("Window");
		f.getContentPane().add(new GUIsim());
		f.setSize(image.getWidth(), image.getHeight());
		f.setExtendedState(JFrame.MAXIMIZED_BOTH);
		f.setUndecorated(false);
		f.setVisible(true);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		ArrayList<Unit> personList = new ArrayList<Unit>();
		ArrayList unitList;
		unitList = javaErlangCommunicator.receivePos();
		for(int i = 0; i < unitList.size(); i++) {
			ArrayList unit = (ArrayList) unitList.get(i);
			OtpErlangPid PID = (OtpErlangPid) unit.get(0);
			int sickness = (Integer) unit.get(1);
			int x = (Integer) unit.get(2);
			int y = (Integer) unit.get(3);
			Unit person = new Unit(PID, sickness, x, y);
			person.paint();
			personList.add(person);
		}

		while (true) {
			unitList = javaErlangCommunicator.receivePos();
			if (unitList == null) {
				break;
			}
			for(Unit person : personList) {
				for (int i = 0; i < unitList.size(); i++) {
					ArrayList unit = (ArrayList) unitList.get(i);
					int sickness = (Integer) unit.get(1);
					int x = (Integer) unit.get(2);
					int y = (Integer) unit.get(3);
					person.moveUnit(x, y, sickness);
				}
			}
		}
	}
}
