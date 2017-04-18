package Communication;

import java.util.*;
import java.io.IOException;
public class MapReader {
    public static void main(String args[])  throws IOException {

        MapParser map = new MapParser("map_one.bmp");

        Map<Integer, ArrayList> walls = map.get_walls();

        for (Map.Entry<Integer, ArrayList> entry : walls.entrySet()) {
            Integer key = entry.getKey();
            System.out.print(key+":");


            ArrayList value = entry.getValue();
            for(int i = 0; i < value.size(); i++) {
                System.out.print(value.get(i)+",");
            }


            /*
              ArrayList<Integer> value = entry.getValue();
              for(Integer val: value) System.out.print(val+",");
            */


            System.out.println("");

        }
    }
}