
package main.java.bufferedimage;
import java.lang.String;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.*;
import javax.imageio.ImageIO;

public class converter {



    public static BufferedImage convertToARGB(String image)
    {
        BufferedImage newImage = null;
				try {
					newImage = ImageIO.read(new File(image));
				} catch (IOException e) {
				}
        Graphics2D g = newImage.createGraphics();
        g.drawImage(newImage, newImage.getWidth(), newImage.getHeight(), null);
        g.dispose();
        return newImage;
    }

		public static void main(String[] args) {
			BufferedImage f = convertToARGB(args[0]);
			System.out.println(f.getWidth());
			System.out.println(f.getHeight());
		}
}

