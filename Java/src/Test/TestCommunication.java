package Test;

import com.ericsson.otp.erlang.*;
import java.util.*;
import Communication.JavaErlangCommunication;

/**
 * Provides simple tests for communication.
 */
public class TestCommunication {
    /*Testing that the communication bewteen Java and Erlang works correct*/
    public static void main(String[] args) throws OtpErlangRangeException {
	//Create the object for communication
        JavaErlangCommunication javaErlangCommunicator = new JavaErlangCommunication();

	//Loop continuously to display updated positions
        while(true){
            ArrayList receive = javaErlangCommunicator.receivePos();
            if(receive != null){
                System.out.println("Asking for new positions");
                System.out.println(receive);
            }else{
                System.out.println("Simulation ended");
                break;
            }
        }
    }
}
