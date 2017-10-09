package Graphic;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;

import com.ericsson.otp.erlang.*;
import java.lang.System;
import javax.imageio.ImageIO;
import java.io.File;
import java.io.IOException;

import Main.Main;
import Communication.JavaErlangCommunication;
import Communication.ReadRecording;

/**
 * The GraphicDisplay class of the program, the will draw the graphics.
 * @author Project Snowfox
 */

public class GraphicDisplay extends JPanel
{
    private static JavaErlangCommunication javaErlangCommunicator = Main.javaErlangCommunicator;
    static ImageComponents imageComponent = null;
    private static InfoDisplay infoInfoDisplay;
    private static JFrame simulation;
    private static int maxNumberOfUnits;
    static int xBound;
    static int yBound;
    public static final int winX = 0;
    public static final int winY = 0;
    protected static final long frequency = 10;

    /**
     * Constructor for the GraphicDisplay class.
     */
    public GraphicDisplay() {
    }


    /**
     * Start running the simulation.
     *
     * @throws InterruptedException    thrown when a thread is waiting, sleeping, or otherwise occupied, and the thread is interrupted, either before or during the activity.
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void runSimulation() throws IOException, InterruptedException, OtpErlangRangeException {
        CollectingStats.getDate();
        if(Main.javaErlangCommunicator.simulationDone){
            System.out.println("Graphic simulation skipped");
        }else{
          createAndShowGUI();
          ArrayList erlangList = JavaErlangCommunication.recievePos();
          maxNumberOfUnits = erlangList.size();
          int numberOfUnits = maxNumberOfUnits;
          infoInfoDisplay = new InfoDisplay(maxNumberOfUnits, numberOfUnits, simulation);
          imageComponent.validate();
          imageComponent.repaint();
          long startTime, stopTime, finishedTime, sleep, zero, second;
          zero = 0;
          second = 1000;
          while (true) {
              startTime = System.currentTimeMillis();
              erlangList = JavaErlangCommunication.recievePos();
              if (erlangList == null) {
                  CollectingStats.addEndStats(numberOfUnits);
                  System.out.println("Simulation done.");
                  InfoDisplay.updateLabel(numberOfUnits);
                  break;
              }
              updateUnitGraphics(erlangList);
              numberOfUnits = erlangList.size();
              imageComponent.validate();
              imageComponent.repaint();
              InfoDisplay.updateLabel(numberOfUnits);
              stopTime = System.currentTimeMillis();
              finishedTime = stopTime - startTime;
              sleep = Math.max(((second / frequency) - finishedTime), zero);
              Thread.sleep(sleep);
            }
        }
    }

    /**
     * Start running the simulation in playback mode.
     *
     * @throws OtpErlangRangeException
     * @throws IOException
     * @throws InterruptedException
     */
    public static void runPlayBack() throws OtpErlangRangeException, IOException, InterruptedException {
        int i = 1;
        CollectingStats.getDate();
        createAndShowGUIinPlayback();
        ArrayList erlangList = ReadRecording.simulationList.get(i);
        maxNumberOfUnits = erlangList.size();
        int numberOfUnits = maxNumberOfUnits;
        infoInfoDisplay = new InfoDisplay(maxNumberOfUnits, numberOfUnits, simulation);
        imageComponent.validate();
        imageComponent.repaint();
        while (true) {
            if ((ReadRecording.simulationList.size() - 1) < i) {
                CollectingStats.addEndStats(numberOfUnits);
                System.out.println("Simulation done.");
                InfoDisplay.updateLabel(numberOfUnits);
                break;
            }
            erlangList = ReadRecording.simulationList.get(i);
            updateUnitGraphics(erlangList);
            numberOfUnits = erlangList.size();
            imageComponent.validate();
            imageComponent.repaint();
            InfoDisplay.updateLabel(numberOfUnits);
            i++;
            Thread.sleep(100);
        }
    }


    /**
     * Checks if a coordination is out of bound.
     *
     * @param xpos x-position as a int.
     * @param ypos y-position as a int.
     * @return true if the coordination is out of bounds, otherwise returns false.
     */
    public static boolean isOutOfBounds(int xpos, int ypos) {
        return xpos < 0 || xpos > xBound || ypos < 0 || ypos > yBound;
    }


    /**
     * Starting the GUI.
     *
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void initializeGUI() throws OtpErlangRangeException {
        while (imageComponent == null) {
            imageComponent = new ImageComponents();
        }
        imageComponent.setImage(javaErlangCommunicator.getMapImage());
        xBound = imageComponent.getWidth();
        yBound = imageComponent.getHeight();
        imageComponent.setSize(xBound, yBound);
        createUnitGraphics();
    }

    /**
     * Starting the GUI for playback.
     *
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void initializeGUIinPlayback() throws OtpErlangRangeException {
        while (imageComponent == null) {
            imageComponent = new ImageComponents();
        }
        imageComponent.setImage(ReadRecording.getMapImage());
        xBound = imageComponent.getWidth();
        yBound = imageComponent.getHeight();
        imageComponent.setSize(xBound, yBound);
        createUnitGraphicsForPlayback();
    }

    /**
     * Creates the Units for the simulation.
     *
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void createUnitGraphics() throws OtpErlangRangeException {
        int x, y, sickness;
        OtpErlangPid PID;
        ArrayList erlangList = null;
        while (erlangList == null) {
            erlangList = JavaErlangCommunication.recievePos();
            //Wait for communication.
        }
        for (Object anErlangList : erlangList) {
            ArrayList unit = (ArrayList) anErlangList;
            y = (Integer) unit.get(3);
            x = (Integer) unit.get(2);
            sickness = (Integer) unit.get(1);
            PID = (OtpErlangPid) unit.get(0);
            Unit person = new Unit(PID, sickness, x, y);
            imageComponent.addUnit(person);
        }
    }


    /**
     * Creates the Units for the playback.
     */
    public static void createUnitGraphicsForPlayback() {
        int x, y, sickness;
        ArrayList recList = ReadRecording.simulationList.get(0);
        for (Object anRecList : recList) {
            ArrayList unit = (ArrayList) anRecList;
            y = (Integer) unit.get(3);
            x = (Integer) unit.get(2);
            sickness = (Integer) unit.get(1);
            Unit person = new Unit(null, sickness, x, y);
            imageComponent.addUnit(person);
        }
    }


    /**
     * Updating the unitList position and status.
     *
     * @param erlangList a list with a Units' position, status and PID.
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void updateUnitGraphics(ArrayList erlangList) throws OtpErlangRangeException {
        int x, y, sickness;
        OtpErlangPid PID;
        ArrayList<Unit> updateList = new ArrayList<>();
        for (Object anErlangList : erlangList) {
            ArrayList unit = (ArrayList) anErlangList;
            y = (Integer) unit.get(3);
            x = (Integer) unit.get(2);
            sickness = (Integer) unit.get(1);
            PID = (OtpErlangPid) unit.get(0);
            Unit person = new Unit(PID, sickness, x, y);
            updateList.add(person);
        }
        imageComponent.setUnitList(updateList);
    }


    protected static void setIconImage(JFrame frame) throws IOException {
        try {
            frame.setIconImage(ImageIO.read(new File("data/icon.png")));
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }


    /**
     * Displaying the GUI.
     *
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void createAndShowGUI() throws OtpErlangRangeException, IOException {
        simulation = new JFrame("Project Snowfox");
        initializeGUI();
        setIconImage(simulation);
        simulation.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        simulation.getContentPane().add(imageComponent);
        simulation.pack();
        simulation.setResizable(false);
        simulation.setVisible(true);
    }

    /**
     * Displaying the GUI in playback.
     *
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void createAndShowGUIinPlayback() throws OtpErlangRangeException, IOException {
        simulation = new JFrame("Project Snowfox");
        initializeGUIinPlayback();
        setIconImage(simulation);
        simulation.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        simulation.getContentPane().add(imageComponent);
        simulation.pack();
        simulation.setResizable(false);
        simulation.setVisible(true);
    }
}