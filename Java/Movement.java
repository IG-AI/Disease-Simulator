import java.util.Random;

public class Movement{
    World world;
    private int radius;
    int xpos;
    int ypos;
    private int xmov;
    private int ymov;
    private int ymax;
    private int xmax;
    
    public Movement(int radius, World world){
        this.radius = radius;
        this.world = world;
        this.xmax = world.x_max();
        this.ymax = world.y_max();
        Random rand = new Random();
        while(true){
            this.xpos = rand.nextInt(xmax - radius * 2 - 20) + radius + 10;
            this.ypos = rand.nextInt(ymax - radius * 2 - 20) + radius + 10;
            if(!world.is_wall(xpos, ypos)){
                break;
            }
        }
        this.xmov = rand.nextInt(2)+3;
        this.ymov = rand.nextInt(2)+3;
        }

    public void move(){
        if((xpos + radius + xmov > xmax) || (xpos - radius + xmov < 0) || (world.is_wall(xpos + radius + xmov, ypos)) ||(world.is_wall(xpos - radius + xmov, ypos))){
            xmov = -xmov;
        }
        if((ypos + radius + ymov > ymax) || (ypos - radius + ymov < 0) || (world.is_wall(xpos,ypos + radius + ymov)) ||(world.is_wall(xpos, ypos - radius + ymov))){
            ymov = -ymov;
        }
        while(true){
            xpos += xmov;            
            ypos += ymov;
            if(!world.is_wall(xpos, ypos)){
                break;
            }
        }
    }
    public int x(){
        return xpos;
    }
    public int y(){
        return ypos;
    }


}
