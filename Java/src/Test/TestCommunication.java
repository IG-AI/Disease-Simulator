package Test;

import com.ericsson.otp.erlang.*;
import java.util.*;
import Communication.JavaErlangCommunication;

public class TestCommunication {
    public static void main(String[] args) throws OtpErlangRangeException {
        JavaErlangCommunication javaErlangCommunicator = new JavaErlangCommunication();

        while(true){
            ArrayList receive = javaErlangCommunicator.receive();
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