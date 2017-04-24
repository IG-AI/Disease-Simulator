import java.awt.*;
import java.awt.event.*;
import java.util.Random;
import javax.swing.*;
import javax.imageio.ImageIO;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Animation extends JPanel {

    World world;
    private int REFRESH_RATE = 15;
    private List<Individual> individuals; 

    private DrawCanvas canvas;
    private int mapHeight;
    private int mapWidth;

    public Animation(int indNum, int indSize, String mapName) {
        world = new World(mapName);
        mapHeight = world.y_max();
        mapWidth = world.x_max();    
        int radius = indSize;
        individuals = new ArrayList<Individual>();
        for(int i = 0; i < indNum; i++){
            individuals.add(new Individual(radius, Color.GREEN, world));
        }
   
        //get canvas to draw on
        canvas = new DrawCanvas();
        this.setLayout(new BorderLayout());
        this.add(canvas, BorderLayout.CENTER);

        //start animation
        animatorStart();
    }
   

    //Run the animations
    public void animatorStart() {
        // Run the animations in new thread.
        Thread aniThread = new Thread() {
                public void run() {
                    while (true) {
                        
                        animatorUpdate();
                        //paint stuff
                        repaint();
                        // Delay and give other thread a chance
                        try {
                            Thread.sleep(1000 / REFRESH_RATE);
                        } catch (InterruptedException e) {}
                    }
                }
            };
        aniThread.start();//will run run();
    }
   

    class DrawCanvas extends JPanel {
        @Override
        public void paintComponent(Graphics g) {
            super.paintComponent(g);    // Paint background
           
            Image img = world.get_map(); //draw image first so it get behind stuff
            g.drawImage(img, 0, 0, this);

            //paint all individuals
            for(int i = 0; i < individuals.size(); i++){
                individuals.get(i).draw(g);
            }
            
        }

        //set size of window
        @Override
        public Dimension getPreferredSize() {
            return (new Dimension(mapHeight, mapWidth));
        }
    }

        //what to do overy "tick"
    public void animatorUpdate() {
        for (int i = 0; i < individuals.size(); i++){
            individuals.get(i).move();
        }
    }
}
