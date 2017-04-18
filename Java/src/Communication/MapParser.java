package Communication;


import java.awt.image.BufferedImage;
import java.awt.Color;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import java.util.*;


public class MapParser {

    //vars that wil hold information so we can pass it
    Map<Integer, ArrayList> walls = new HashMap<Integer, ArrayList>();
    Map<Integer, ArrayList> hospital = new HashMap<Integer, ArrayList>();
    int width;
    int height;

    //Parse the provided map
    public MapParser(String map)  throws IOException {

        //Open image
        File file = new File(map);
        BufferedImage image = ImageIO.read(file);

        //get height and width
        width = image.getWidth();
        height = image.getHeight();

        //go over every column
        for(Integer x = 0; x < width; x++){

            //these lists will keep track of pixels for every row
            ArrayList<Integer> wall_y = new ArrayList<Integer>();
            ArrayList<Integer> hospital_y = new ArrayList<Integer>();

            //and in every column go over every row..
            for(Integer y = 0; y < height; y++){

                //we use the Color class because it's easy to use..
                Color color = new Color(image.getRGB(x, y));

                if(is_accessible(color)){
                    //do nothing
                }else if(is_hospital(color)){//if it's hospital colors
                    hospital_y.add(y);
                }else{//this allow us to paint the map in pretty colors! =)
                    wall_y.add(y);
                }
            }

            //Add new pixels to hospital
            if(!hospital_y.isEmpty()){
                hospital.put(x, hospital_y);
            }

            //Add new pixels to walls
            if(!wall_y.isEmpty()){
                walls.put(x, wall_y);
            }
        }
    }

    //matches black
    private boolean is_accessible(Color color){
        return ((color.getRed() == 0) && (color.getGreen() == 0) && (color.getBlue() == 0));
    }

    //matches the purpleish color
    private boolean is_hospital(Color color){
        return ((color.getRed() == 155) && (color.getGreen() == 83) && (color.getBlue() == 111));
    }

    public Map<Integer, ArrayList> get_hospital(){
        return hospital;
    }

    public Map<Integer, ArrayList> get_walls(){
        return walls;
    }

    public int get_height(){
        return height;
    }

    public int get_width(){
        return width;
    }
}
