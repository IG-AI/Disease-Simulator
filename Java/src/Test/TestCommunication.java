package Test;

import com.ericsson.otp.erlang.*;
import java.util.*;
import Communication.JavaErlangCommunication;

public class TestCommunication {
    /*Testing that the communication bewteen Java and Erlang works correct*/
    public static void main(String[] args) throws OtpErlangRangeException {
        JavaErlangCommunication javaErlangCommunicator = new JavaErlangCommunication();

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