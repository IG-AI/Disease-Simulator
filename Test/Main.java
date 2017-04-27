import java.io.IOException;

public class Main{
    public static void main(String args[]){
	MapParser map = null;
	try{
	    map = new MapParser("small_map.bmp");
	}catch(IOException e){
	}

	System.out.println(map.get_walls());

    }
} 
