package Main;

import Communication.JavaErlangCommunication;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import java.io.IOException;

import Graphic.GraphicDisplayProfiling;

/**
 * The main class that will start and drive the program.
 * @author Project Snowfox
 */
public class MainProfiling {
    public static JavaErlangCommunication javaErlangCommunicator;


    /**
     * The main method to use under profiling.
     *
     * @param args command line input
     *
     * @throws InterruptedException thrown when a thread is waiting, sleeping, or otherwise occupied, and the thread is interrupted, either before or during the activity.
     * @throws OtpErlangRangeException thrown when an attempt is made to create an Erlang term with data that is out of range for the term in question.
     */
    public static void main(String []args) throws InterruptedException, OtpErlangRangeException, IOException {
        GraphicDisplayProfiling.createAndShowProfilingGUI();
        GraphicDisplayProfiling.runSimulationForProfiling();
    }
}