
package main.bufferedimage;
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
				BufferedImage nIn = new BufferedImage(newImage.getWidth(), newImage.getHeight(), BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = nIn.createGraphics();
        g.drawImage(newImage, 0, 0, null);
        g.dispose();
        return newImage;
    }

		public static void main(String[] args) {
			BufferedImage f = convertToARGB(args[0]);
			System.out.println(f.getWidth());
			System.out.println(f.getHeight());
		}
}

