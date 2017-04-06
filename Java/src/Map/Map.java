package Map;

import javax.swing.*;
import java.awt.*;

public class Map extends JPanel {

    public static final Color CITY = new Color(000,000,000);
    public static final Color BUILDING = new Color(255,255,255);
    public static final Color HOSPITAL = new Color(000,191,255);
    public static final Color PERSON = new Color(000,255,000);
    public static final Color INFECTEDPERSON = new Color(255,000,000);


    public static final Color[] TERRAIN = {
        CITY,
        BUILDING,
        HOSPITAL
    };
    

    public static final Color[] STATES = {
        PERSON,
        INFECTEDPERSON
    };




    public static final int NUM_ROWS = 30;
    public static final int NUM_COLS = 30;

    public static final int PREFERRED_GRID_SIZE_PIXELS = 10;


    private final Color[][] terrainGrid;

    public Map(){
        this.terrainGrid = new Color[NUM_ROWS][NUM_COLS];
        

        for (int x = 0; x < NUM_ROWS; x++){
            for (int y = 0; y < NUM_COLS; y++)
                this.terrainGrid [x][y] = CITY;
        }

        int selectedWidth = NUM_COLS * PREFERRED_GRID_SIZE_PIXELS;
        int selectedHeight = NUM_ROWS * PREFERRED_GRID_SIZE_PIXELS;
        setPreferredSize(new Dimension(selectedWidth, selectedHeight));
    }

    @Override
    public void paintComponent(Graphics g) {
        // Important to call super class method
        super.paintComponent(g);
        // Clear the board
        g.clearRect(0, 0, getWidth(), getHeight());
        // Draw the grid
        int rectWidth = getWidth() / NUM_COLS;
        int rectHeight = getHeight() / NUM_ROWS;

        for (int i = 0; i < NUM_ROWS; i++) {
            for (int j = 0; j < NUM_COLS; j++) {
                // Upper left corner of this terrain rect
                int x = i * rectWidth;
                int y = j * rectHeight;
                Color terrainColor = terrainGrid[i][j];
                g.setColor(terrainColor);
                g.fillRect(x, y, rectWidth, rectHeight);
            }
        }
    }


    public void paintCitizens(Graphics g) {
        g.setColor(PERSON);
        g.fillOval(80, 80, 30, 30);

    }



    public static void main(String[] args) {
        // http://docs.oracle.com/javase/tutorial/uiswing/concurrency/initial.html
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                JFrame frame = new JFrame("Game");
                Map map = new Map();
                frame.add(map);
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                frame.pack();
                frame.setVisible(true);
            }
        });
    }
}