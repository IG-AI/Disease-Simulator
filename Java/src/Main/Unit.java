import javax.swing.*;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import javax.imageio.*;


public class Unit extends JComponent
{
	public int PID;
	public int status;
	public int x;
	public int y;
	public static final Color INFECTED = Color.RED;
	public static final Color HEALTHY  = Color.GREEN;

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
		super.paintComponent(g);
		if(status == 0) {
			g.setColor(INFECTED);
		}
		else {
			g.setColor(HEALTHY);
		}
		g.fillOval(x,y,3,3);
	}
}


