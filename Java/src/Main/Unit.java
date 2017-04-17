

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
		repaint(x,y,newx,newy);
	}

	public int pid()
	{
		return PID;
	}

	@Override
	protected void paintComponent(Graphics g) {
		if(status == 0) {
			g.setColor(Color.RED);
		}
		else {
			g.setColor(Color.GREEN);
		}
		g.fillOval(x,y,3,3);
	}
}


