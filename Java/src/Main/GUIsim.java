package Main;

import javax.swing.*;
import java.util.ArrayList;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;

import Communication.JavaErlangCommunication;


/**
 * The main class that will start and drive the program.
 * @author Project Snowfox
 */
public class GUIsim extends JPanel
{
	public static final int winX = 0;
	public static final int winY = 0;

	/**
	 * Running the program.
	 * @param args input from commandline.
	 */
	public static void main(String []args) throws InterruptedException, OtpErlangRangeException {
		//Initialize communication between SIMengine and GUIengine
		JavaErlangCommunication javaErlangCommunicator = new JavaErlangCommunication();
		//Command for creating bufferedimage of map
		Background map = new Background(javaErlangCommunicator.getMapName(), winX, winY);
		JFrame f = new JFrame("Project-Snowfox");
		f.setContentPane(map);
		f.setSize(map.getWidth(), map.getHeight());
		f.setExtendedState(JFrame.MAXIMIZED_BOTH);
		f.setUndecorated(false);
		f.setVisible(true);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		ArrayList<Unit> personList = new ArrayList<Unit>();
		ArrayList unitList = javaErlangCommunicator.receivePos();
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
