package Main;

import com.ericsson.otp.erlang.OtpErlangPid;

import javax.swing.*;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import javax.imageio.*;


public class Unit extends JPanel
{
	public int PID;
	public int status;
	public int x;
	public int y;
	public static final Color INFECTED = Color.RED;
	public static final Color HEALTHY  = Color.GREEN;
	public static final Color TRANSPARENT = new Color(255,255,255,255);

	public Unit(int pid, int sickness, int posx, int posy) {
		PID = pid;
		status = sickness;
		x = posx;
		y = posy;
	}

	public void moveUnit(int newx, int newy, int sickness) {
		status = sickness;
		x = newx;
		y = newy;
		repaint();
	}

	public int pid()
	{
		return PID;
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		if(status == 0) {
			g.setColor(INFECTED);
		}
		else if(status ==1) {
			g.setColor(HEALTHY);
		}
		else
			g.setColor(TRANSPARENT);
		g.fillOval(x,y,9,9);
	}
}


