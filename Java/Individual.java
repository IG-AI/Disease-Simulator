import java.awt.*;
import java.util.Random;

public class Individual {
    Movement movement;
    int x;
    int y;
    int radius;
    private Color color;

    public Individual(int radius, Color color, World world) {

      movement = new Movement(radius, world);
      this.x = movement.x();
      this.y = movement.y();
      this.radius = radius;
      this.color = color;
   }
   

   public void move() {
       movement.move();
       this.x = movement.x();
       this.y = movement.y();
   }

    public int x(){
        return x;
    }
    public int y(){
        return y;
    }
    public int r(){
        return radius;
    }
    public Color c(){
        return color;
    }
   
   /** Draw itself using the given graphics context. */
    public void draw(Graphics g) {
        g.setColor(color);
        g.fillOval((int)(x - radius), (int)(y - radius), (int)(2 * radius), (int)(2 * radius));
    }
}
