package Communication;

import java.awt.*;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.ArrayList;

public class ReadRecording {
    public static ArrayList<ArrayList<ArrayList<Object>>> simulationList = new ArrayList<>();
    public static String mapName;
    private static MapParser map = null;


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
        String index = "mapRead";
        int stopFlag = 0;
        int commaCount = 0;
        int characterIndex;
        Integer PID = null;
        Integer status;
        StringBuilder stringBuilderMapName = new StringBuilder();
        StringBuilder stringPosX = new StringBuilder();
        StringBuilder stringPosY = new StringBuilder();
        ArrayList<Object> unitSingleList = new ArrayList<>();
        ArrayList<ArrayList<Object>> unitList = new ArrayList<>();
        while ((characterIndex = reader.read()) != -1) {
            char character = (char) characterIndex;
            if (character != '\n' && character != ' ' && stopFlag == 0) {
                if (character == '<') {
                    stopFlag = 1;
                    index = "pidRead";
                }
                switch (index) {
                    case "mapRead":
                        if (character != '[') {
                            stringBuilderMapName.append(character);
                            break;
                        } else {
                            stringBuilderMapName.deleteCharAt(0);
                            stringBuilderMapName.deleteCharAt(stringBuilderMapName.length() - 1);
                            mapName = "data/" + (stringBuilderMapName.toString() + ".bmp");
                            break;
                        }
                    case "pidRead":
                        if (character == ',') {
                            unitSingleList.add(PID);
                            index = "statusRead";
                            break;
                        }
                    case "statusRead":
                        if (isInt(character)) {
                            status = Character.getNumericValue(character);
                            unitSingleList.add(status);
                            index = "posXRead";
                            break;
                        }
                        else {
                            break;
                        }
                    case "posXRead":
                        if (isInt(character)) {
                            stringPosX.append(character);
                            break;
                        }
                        if (character == ',') {
                            commaCount++;
                            if (commaCount == 2) {
                                unitSingleList.add(Integer.parseInt(stringPosX.toString()));
                                commaCount = 0;
                                index = "posYRead";
                                break;
                            }
                            else {
                                break;
                            }
                        }
                    case "posYRead":
                        if (character != '}') {
                            stringPosY.append(character);
                            break;
                        } else {
                            unitSingleList.add(Integer.parseInt(stringPosY.toString()));
                            index = "ednRead";
                            break;
                        }
                    case "ednRead":
                        if (character == ',') {
                            unitList.add(cloneList(unitSingleList));
                            unitSingleList.clear();
                            stringPosX.setLength(0);
                            stringPosY.setLength(0);
                            index = "pidRead";
                            break;

                        }
                        else if (character == ']') {
                            unitList.add(cloneList(unitSingleList));
                            simulationList.add(cloneListList(unitList));
                            unitList.clear();
                            unitSingleList.clear();
                            stringPosX.setLength(0);
                            stringPosY.setLength(0);
                            index = "pidRead";
                            break;
                        }

                }
            }
            else {
                if (character == '>') {
                    stopFlag = 0;
                }
            }
        }
        map = new MapParser(mapName);
    }

    /**
     * Returns the map to use for playback.
     *
     * @return the name and path to the requested map
     */
    public static Image getMapImage(){
        return map.get_map();
    }

    private boolean isInt(char c) {
        if (c == '0') {
            return true;
        }
        if (c == '1') {
            return true;
        }
        if (c == '2') {
            return true;
        }
        if (c == '3') {
            return true;
        }
        if (c == '4') {
            return true;
        }
        if (c == '5') {
            return true;
        }
        if (c == '6') {
            return true;
        }
        if (c == '7') {
            return true;
        }
        if (c == '8') {
            return true;
        }
        if (c == '9') {
            return true;
        }
        return false;
    }

    private static ArrayList<Object> cloneList(ArrayList<Object> list) {
        ArrayList<Object> clone = new ArrayList<>(list.size());
        clone.addAll(list);
        return clone;
    }

    private static ArrayList<ArrayList<Object>> cloneListList(ArrayList<ArrayList<Object>> list) {
        ArrayList<ArrayList<Object>> clone = new ArrayList<>(list.size());
        for (ArrayList<Object> anlist : list) {
            ArrayList<Object> clonedList = cloneList(anlist);
            clone.add(clonedList);
        }
        return clone;
    }
}
