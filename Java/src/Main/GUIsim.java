package Main;

import java.util.Random;
import javax.swing.*;
import java.util.ArrayList;
import java.io.*;
import java.awt.*;
import java.awt.image.*;
import javax.imageio.*;
//import com.ericsson.otp.erlang.OptErlangRangeException;
//import com.ericsson.otp.erlang.OptErlangPid;
import com.ericsson.otp.erlang.*;
import Communication.JavaErlangCommunication;
import java.lang.System;
import java.lang.Math.*;


public class GUIsim extends JPanel
{
    private static JavaErlangCommunication javaErlangCommunicator = null;
	private static JFrame simulation;
	private static Background background = null;
	private static ArrayList<Unit> unitList;
	public static int xBound;
	public static int yBound;
	public static final int winX = 0;
	public static final int winY = 0;
    private static long frequency = 10;
    
	public GUIsim(int winW, int winH) {		
	}


	/**
	 * Running the program.
	 * @param args input from commandline.
	 */
    public static void runSimulation() throws InterruptedException, OtpErlangRangeException {
		createUnitGraphics();
		background.validate();
		background.repaint();
		long startTime, stopTime, finishedTime, sleep, zero, second;
		zero = 0;
		second = 1000;
		while(true) {
		    startTime = System.currentTimeMillis();
			ArrayList erlangList = javaErlangCommunicator.recievePos();
			if(erlangList == null) {
			    System.out.println("Simulation done.");
				break;
			}
			updateUnitGraphics(erlangList);
			background.validate();
			background.repaint();
			stopTime = System.currentTimeMillis();
			finishedTime = stopTime - startTime;
			sleep = Math.max(((second/frequency)-finishedTime), zero);
       			Thread.sleep(sleep);
		}
	}

	public static boolean isOutOfBounds(int xpos, int ypos) {
		if(xpos < 0 || xpos > xBound || ypos < 0 || ypos > yBound)
			return true;
		return false;
	}
		
	public static void initializeGUI() throws OtpErlangRangeException {
	    while(background == null){
		background = new Background(winX, winY);
	    }
	    background.image = javaErlangCommunicator.getMapImage();
	    xBound     = background.image.getWidth(null);
	    yBound     = background.image.getHeight(null);
	    background.setSize(xBound,yBound);
	    createUnitGraphics();
	}


	public static void createUnitGraphics() throws OtpErlangRangeException {
	    int x,y,sickness;
	    OtpErlangPid PID;
		ArrayList erlangList = null;
		while(erlangList == null) {
			erlangList = javaErlangCommunicator.recievePos();
			//Wait for communication.
		}
		for(int i = 0; i < erlangList.size(); i++) {
			ArrayList unit = (ArrayList) erlangList.get(i);
			y = (Integer) unit.get(3);
			x = (Integer) unit.get(2);
			sickness = (Integer) unit.get(1);
			PID = (OtpErlangPid) unit.get(0);
			Unit person = new Unit(PID, sickness, x, y);
			background.addUnit(person);
		}
	}

	public static void updateUnitGraphics(ArrayList erlangList) throws OtpErlangRangeException {
	    int x,y,sickness;
	    OtpErlangPid PID;
	    ArrayList<Unit> updateList = new ArrayList<Unit>();
		for(int i = 0; i < erlangList.size(); i++) {
			ArrayList unit = (ArrayList) erlangList.get(i);
			y = (Integer) unit.get(3);
			x = (Integer) unit.get(2);
			sickness = (Integer) unit.get(1);
			PID = (OtpErlangPid) unit.get(0);
			Unit person = new Unit(PID, sickness, x, y);
			updateList.add(person);
		}
		background.setUnitList(updateList);
	}

	public static void createAndShowGUI() throws OtpErlangRangeException {
		initializeGUI();
		JFrame frame = new JFrame("Project Snowfox");
		frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		frame.getContentPane().add(background);
		//frame.setSize(xBound, yBound);
		frame.pack();
		frame.setVisible(true);
	}

 /**
 * The main class that will start and drive the program.
 * @author Project Snowfox
 */
	public static void main(String []args) throws InterruptedException, OtpErlangRangeException {
	    javaErlangCommunicator = new JavaErlangCommunication();
	    //Command for creating bufferedimage of map
	    createAndShowGUI();
	    runSimulation();
	}
}
