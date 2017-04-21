package Other;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class CityMap extends JComponent implements ActionListener{
	
    public static final Color CITY = new Color(000,000,000);
    public static final Color BUILDING = new Color(255,255,255);
    public static final Color HOSPITAL = new Color(000,191,255);
    public static final Color PERSON = new Color(000,255,000);
    public static final Color INFECTEDPERSON = new Color(255,000,000);
	
    
    private int personX = 350;
    private int personY = 350;
    
    private int infectedPersonX = 400;
    private int infectedPersonY = 350;
	private int personSpeedX = 5;
    private int personSpeedY = 5;

	public static void main(String[] args) {
		JFrame window = new JFrame("Snowfox_City");
		CityMap simulation = new CityMap();
		window.add(simulation);
		window.pack();
		window.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		window.setLocationRelativeTo(null);
		window.setVisible(true);
		
		Timer t = new Timer(100, simulation);
		t.start();
	}

	public Dimension getPreferredSize() {
		return new Dimension(800, 600);
	}
	
	@Override
	protected void paintComponent(Graphics g) {
		//CITY BACKGROUND
		g.setColor(CITY);
		g.fillRect(0, 0, 800, 600);
		
		//HOSPITAL
		g.setColor(HOSPITAL);
		g.fillRect(200, 410, 65, 45);

		//BUILDING
		g.setColor(BUILDING);
		g.fillRect(300, 300, 30, 30);
		
		//Draw a person
		g.setColor(PERSON);
		g.fillOval(personX, personY, 5, 5);
		
		//INFECTED PERSON
		g.setColor(INFECTEDPERSON);
		g.fillOval(infectedPersonX, infectedPersonY, 5, 5);
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		personX = personX + personSpeedX;
		personY = personY + personSpeedY;

		if (personY >=600 - 5) {
			personSpeedY = -5;
		}

		if (personX >= 800 - 5) {
			personSpeedX = -5;
		}

		if (personY <= 0+5) {
			personSpeedY = 5;
		}

		if (personX <= 0+5) {
			personSpeedX = 5;
		}
		repaint();
	}
}
