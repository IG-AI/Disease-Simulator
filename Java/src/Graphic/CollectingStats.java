package Graphic;


import Menu.GUIForm;

import java.io.*;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.file.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * The CollectngStats class of the program, save data to a text file.
 * @author Project Snowfox
 */

public class CollectingStats {

    private static final DateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    private static String formatDate;

    public static void getDate() throws IOException {

        Date date = new Date();

        formatDate = "** End values simulation: " + sdf.format(date) + " **\n";

        }


    public static void addEndStats(int numberOfUnits) throws IOException {

        URL url = GUIForm.class.getProtectionDomain().getCodeSource().getLocation();
        String folderPath = URLDecoder.decode(url.getFile(), "UTF-8");
        folderPath = folderPath.replace("Java/bin/", "");
        folderPath = folderPath + "logs/";
        String checkFile = folderPath + "java_data_log.log";
        Path path = FileSystems.getDefault().getPath(folderPath);
        Path filePath = FileSystems.getDefault().getPath(checkFile);

        if(!Files.exists(path))
        {
            File file = new File(folderPath);
            if(file.mkdir()){
                System.out.print("Log folder created\n");
            }
            else
            {
                System.out.print("Log could not be created\n");
            }
        }

        PrintWriter writer = null;

        if(!Files.exists(filePath)){
            writer = new PrintWriter(checkFile, "UTF-8");
            File file = new File(checkFile);
            if(file.exists()){
                System.out.print("Log file created\n");
            }
            else{
                System.out.print("Log file could not be created\n");
            }
        }

        String aliveUnits = "Live individuals: \n" + String.valueOf(numberOfUnits) + "\n";
        String vaccinatedUnits ="Vaccinated individuals: \n" + String.valueOf(ImageComponents.tempVacciantedUnits) + "\n";
        String sickUnits = "Sick individuals: \n" + String.valueOf(ImageComponents.tempSickUnits) + "\n";
        String deadUnits = "Dead individuals: \n" + String.valueOf((InfoDisplay.startNumberOfUnits) - (numberOfUnits)) + "\n";
        String ticks = "Ticks elapsed: \n" + String.valueOf(ImageComponents.ticks) + "\n";

        if(Files.exists(filePath)) {

            Files.write(Paths.get(checkFile), "************************************************\n".getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatDate.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), "************************************************\n\n".getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), aliveUnits.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), sickUnits.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), deadUnits.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), vaccinatedUnits.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), ticks.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), "\n\n".getBytes(), StandardOpenOption.APPEND);

        }
    }

}
