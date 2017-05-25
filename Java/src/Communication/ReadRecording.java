package Communication;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.ArrayList;

public class ReadRecording {
    public static ArrayList<ArrayList> simulationList;


    /**
     * Loading a record file so it can be played in playback mode.
     *
     * @param fileName the name if the record file you want to play.
     * @throws IOException
     */
    public ReadRecording(String fileName) throws IOException {
        BufferedReader reader = new BufferedReader(
                new InputStreamReader(
                        new FileInputStream("recordings/" + fileName + ".record"),
                        Charset.forName("UTF-8")));
        int index = -1;
        int characterIndex;
        Integer PID = null;
        int status;
        StringBuilder stringPosX = new StringBuilder();
        StringBuilder stringPosY = new StringBuilder();
        ArrayList<Integer> unitList = new ArrayList<>();
        while ((characterIndex = reader.read()) != -1) {
            char character = (char) characterIndex;
            if (character == 'S') {
                index = 4;
            }
            switch (index) {
                case -1:
                    if (character != '[') {
                        throw new IOException("Wrong type of file!");
                    } else {
                        index = 0;
                    }
                case 0:
                    if (character == ',') {
                        unitList.add(0, PID);
                        index = 1;
                    }
                case 1:
                    if (character != ',') {
                        status = (int) character;
                        unitList.add(1, status);
                    } else {
                        index = 2;
                    }
                case 2:
                    if (character != ',') {
                        stringPosX.append(character);
                    } else {
                        unitList.add(2, Integer.parseInt(stringPosX.toString()));
                        index = 3;
                    }
                case 3:
                    if (character != '}') {
                        stringPosY.append(character);
                    }
                    else {
                        unitList.add(3, Integer.parseInt(stringPosY.toString()));
                        index = 4;
                    }
                case 4:
                    if (character == ',') {
                        simulationList.add(unitList);
                        index = 0;

                    }
                    else {
                        simulationList.add(null);
                        break;
                    }

            }
        }
    }
}
