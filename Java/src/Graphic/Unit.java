package Graphic;

import javax.swing.*;
import java.awt.*;
import com.ericsson.otp.erlang.*;
import java.lang.Math.*;

/**
 * The Unit class of the program, that will represent the people in the simulation.
 * @author Project Snowfox
 */
public class Unit extends JComponent
{
    public OtpErlangPid PID;
    public int status;
    public int x;
    public int y;
    private static final Color INFECTED = Color.RED;
    private static final Color HEALTHY  = Color.GREEN;
    private static final Color ERROR = new Color(145, 187, 255);


	/**
	 * Constructor for a Unit.
	 *
	 * @param pid the Units' PID as a OtpErlangPid.
	 * @param sickness the sickness status as a int.
	 * @param posx the x-position of the Unit as a int.
	 * @param posy the y-position of the Unit as a int.
	 */
	public Unit(OtpErlangPid pid, int sickness, int posx, int posy) {
		PID = pid;
		status = sickness;
		x = posx;
		y = posy;
	}


	/**
	 * Repainting the new position and status of a Unit.
	 *
	 * @param newx the new x-position of the Unit as a int.
	 * @param newy the new y-position of the Unit as a int.
	 * @param sickness the new sickness status of the Unit as a int.
	 */
	public void moveUnit(int newx, int newy, int sickness) {
		status = sickness;
		x = newx;
		y = newy;
	}


	/**
	 * Painting a Unit, based on it's status.
	 *
	 * @param g a graphic object
	 */
	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		if(status == 1) {
			g.setColor(INFECTED);
		}
		else if(status == 0) {
			g.setColor(HEALTHY);
		}
		else {
			g.setColor(ERROR);
		}
		int diameter = 7;
		g.fillOval(x-(diameter%2)-(diameter/2), y- 3, diameter, diameter);
	}
}
