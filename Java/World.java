import java.io.IOException;
import java.awt.Image;
import java.util.*;

public class World{
    int maxX;
    int maxY;
    int minX = 0;
    int minY = 0;
    MapParser map;
    Map<Integer, ArrayList> walls;
    
    public World(String str){
        try{
            map = new MapParser(str);
        }catch (IOException e){}
        this.maxX = map.get_width();
        this.maxY = map.get_height();
        this.walls = map.get_walls();
    }

    public void set(int x, int y){
        this.maxX = x;
        this.maxY = y;
    }
    public int x_max(){
        return maxX;
    }

    public int y_max(){
        return maxY;
    }

    public boolean is_wall(int x, int y){
        if(walls.containsKey(x)){
            return walls.get(x).contains(y);
        }else{
            return false;
        }
    }

    public Image get_map(){
        return map.get_map();
    }
}
