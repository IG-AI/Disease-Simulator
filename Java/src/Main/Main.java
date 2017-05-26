package Main;

import Graphic.GraphicDisplay;
import com.ericsson.otp.erlang.OtpErlangRangeException;

import Communication.JavaErlangCommunication;
import Communication.ReadRecording;

import java.io.IOException;

/**
 * The main class that will start and drive the program.
 * @author Project Snowfox
 */
public class Main {
    public static JavaErlangCommunication javaErlangCommunicator;
    public static ReadRecording readRecording;


    /**
     * The main method.
     *
     * @param args command line input
     *
     * @throws InterruptedException thrown when a thread is waiting, sleeping, or otherwise occupied, and the thread is interrupted, either before or during the activity.
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void main(String []args) throws InterruptedException, OtpErlangRangeException, IOException {
        if (args[0].equals("playback")) {
            readRecording = new ReadRecording(args[1]);
            GraphicDisplay.runPlayBack();
        }
        else {
            javaErlangCommunicator = new JavaErlangCommunication();
            GraphicDisplay.runSimulation();
        }

    }
}
