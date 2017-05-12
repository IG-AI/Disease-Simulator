package Graphic;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

/**
 * The GraphicDisplay class for use under profiling of the program, the will draw the graphics.
 * @author Project Snowfox
 */
public class GraphicDisplayProfiling extends GraphicDisplay {
    public GraphicDisplayProfiling() {
        super();
    }


    /**
     * Start running the simulation.
     *
     * @throws InterruptedException thrown when a thread is waiting, sleeping, or otherwise occupied, and the thread is interrupted, either before or during the activity.
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void runSimulationForProfiling() throws InterruptedException, OtpErlangRangeException {
        createUnitGraphicsForProfiling();
        imageComponent.validate();
        imageComponent.repaint();
        long startTime, stopTime, finishedTime, sleep, zero, second;
        zero = 0;
        second = 1000;
        while(true) {
            startTime = System.currentTimeMillis();
            ArrayList erlangList = listHandler();
            if(erlangList == null) {
                System.out.println("Simulation done.");
                break;
            }
            updateUnitGraphics(erlangList);
            imageComponent.validate();
            imageComponent.repaint();
            stopTime = System.currentTimeMillis();
            finishedTime = stopTime - startTime;
            sleep = Math.max(((second/frequency)-finishedTime), zero);
            Thread.sleep(sleep);
        }
    }


    /**
     * Starting the GUI.
     *
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void initializeProfilingGUI() throws OtpErlangRangeException, IOException {
        while (imageComponent == null) {
            imageComponent = new ImageComponents();
        }
        imageComponent.setImage(ImageIO.read(new File("data/map_two.bmp")));
        xBound = imageComponent.getWidth();
        yBound = imageComponent.getHeight();
        imageComponent.setSize(xBound, yBound);
        createUnitGraphicsForProfiling();
    }


    private static ArrayList<Object> listHandler() {
        ArrayList<Object> unitArrayList = new ArrayList<>();
        for (int i = 0; i < 2500; i++) {
            Unit unit = new Unit(null, 0, i, i);
            unitArrayList.add(unit);
        }
        return unitArrayList;
    }


    /**
     * Creates the Units for the simulation.
     *
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void createUnitGraphicsForProfiling() throws OtpErlangRangeException {
        int x, y, sickness;
        OtpErlangPid PID;
        ArrayList<Object> erlangList = null;
        while (erlangList == null) {
            erlangList = listHandler();
        }
        for (Object anErlangList : erlangList) {
            Unit unit = (Unit) anErlangList;
            y = unit.y;
            x = unit.x;
            sickness = unit.status;
            PID = unit.PID;
            Unit person = new Unit(PID, sickness, x, y);
            imageComponent.addUnit(person);
        }
    }


    /**
     * Creates the Units for the simulation.
     *
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void createAndShowProfilingGUI() throws OtpErlangRangeException, IOException {
        JFrame frame = new JFrame("Project Snowfox");
        initializeProfilingGUI();
        setIconImage(frame);
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.getContentPane().add(imageComponent);
        //frame.setSize(xBound, yBound);
        frame.pack();
        frame.setVisible(true);
    }


    /**
     * Updating the unitList position and status.
     * @param erlangList a list with a Units' position, status and PID.
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void updateUnitGraphics(ArrayList erlangList) throws OtpErlangRangeException {
        int x,y,sickness;
        OtpErlangPid PID;
        ArrayList<Unit> updateList = new ArrayList<>();
        for (Object anErlangList : erlangList) {
            Unit unit = (Unit) anErlangList;
            y = unit.y;
            x = unit.x;
            sickness = unit.status;
            PID = unit.PID;
            Unit person = new Unit(PID, sickness, x, y);
            updateList.add(person);
        }
        imageComponent.setUnitList(updateList);
    }

}
