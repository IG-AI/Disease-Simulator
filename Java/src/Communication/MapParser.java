package Communication;


import java.awt.image.BufferedImage;
import java.awt.Image;
import java.awt.Color;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import java.util.*;

/**
 * Provides functionality to process a map (bitmap image).
 * @author Project Snowfox
 */
public class MapParser {

    /* vars that will hold information so we can pass it.
       walls will hold all the pixels of walls in a HashMap
       with the X-coordinates as keys for an ArrayList containing
       the Y-coordinates.
       hospital does the same things as walls but for hospital pixels.
    */
    Map<Integer, ArrayList> walls = new HashMap<>();
    Map<Integer, ArrayList> hospital = new HashMap<>();
    int width;
    int height;
    Image img;


    /**
     * Constructor to parse the provided map.
     *
     * @param map the name of the map as a String
     */
    public MapParser(String map)  throws IOException {

        //Open image (the map)
        File file = new File(map);
        BufferedImage image = ImageIO.read(file);
        this.img = ImageIO.read(file);
        //Get height and width of the image
        width = image.getWidth();
        height = image.getHeight();

        //go over every column
        for(Integer x = 0; x < width; x++){

            //these lists will keep track of pixels for every row
            ArrayList<Integer> wall_y = new ArrayList<>();
            ArrayList<Integer> hospital_y = new ArrayList<>();

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


    /**
     * Matches black color.
     * If a pixel is black, it should be possible to walk on it.
     *
     * @param color the color to check
     *
     * @return true if the color is black, else false.
     */
    private boolean is_accessible(Color color){
        return ((color.getRed() == 0) && (color.getGreen() == 0) && (color.getBlue() == 0));
    }


    /**
     * Matches the purpleish color of hospitals.
     * If a pixel is of the hospital color, it should be possible to walk on it.
     * Furthermore we might want to go to it if we're sick.
     *
     * @param color the color to check
     *
     * @return true if the color is the one of the hospital, else false.
     */
    private boolean is_hospital(Color color){
        return ((color.getRed() == 155) && (color.getGreen() == 83) && (color.getBlue() == 111));
    }


    /**
     * The method used to get all pixels where a hospital exist.
     * The HashMap provided will have the x-coordinate as keys
     * for ArrayLists containing the y-coordinates of the pixels.
     * A square hospital 3x3 pixels might look like;
     * |4| -> [12, 13, 14]
     * |5| -> [12, 13, 14]
     * |6| -> [13, 14, 12]
     *
     * NOTE: The individual ordering of the pixels in
     * the ArrayList is not defined.
     * NOTE: The top left corner of the picture has the coordinate X:0 and Y:0
     *
     * @return a HashMap containing the pixels of hospitals
     */
    public Map<Integer, ArrayList> get_hospital(){
        return hospital;
    }


    /**
     * The method used to get all pixels where walls exist.
     * The HashMap provided will have the x-coordinate as keys
     * for ArrayLists containing the y-coordinates of the pixels.
     * An L-shaped wall with that is two pixels thick might look like;
     *
     * |2| -> [9, 10]
     * |3| -> [10, 9]
     * |4| -> [9, 10]
     * |5| -> [9, 10]
     * |6| -> [9, 10, 11, 12, 13]
     * |7| -> [13, 11, 10, 9, 12]
     *
     * NOTE: The individual ordering of the pixels in
     * the ArrayList is not defined.
     * NOTE: The top left corner of the picture has the coordinate X:0 and Y:0
     *
     * @return a HashMap containing the pixels of hospitals
     */
    public Map<Integer, ArrayList> get_walls(){
        return walls;
    }


    /**
     * Get the height of the map.
     *
     * @return The height in pixels of the map
     */
    public int get_height(){
        return height;
    }


    /**
     * Get the width of the map.
     *
     * @return The width in pixels of the map
     */
    public int get_width(){
        return width;
    }

    public Image get_map(){
        return img;
    }
}
