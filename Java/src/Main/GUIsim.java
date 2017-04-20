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


	public static void main(String []args) throws InterruptedException, OtpErlangRangeException {
		JavaErlangCommunication javaErlangCommunicator = new JavaErlangCommunication();
		map = javaErlangCommunicator.getMapName();
		JFrame f = new JFrame("Window");
		f.getContentPane().add(new GUIsim());
		f.setSize(image.getWidth(), image.getHeight());
		f.setExtendedState(JFrame.MAXIMIZED_BOTH);
		f.setUndecorated(true);
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
