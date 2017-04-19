import com.ericsson.otp.erlang.*;
import java.util.*;
import java.io.*;
import java.lang.Thread.*;

public class Main {
    public static void main(String[] args) {
        //Vars we wanna use..
        OtpErlangPid lastPid = null;
        OtpErlangPid myPid = null;
        OtpMbox myOtpMbox = null;
        OtpNode myOtpNode = null;
        
        try {
            //Register the server as a node.
            myOtpNode = new OtpNode("java_server@localhost");

            //Set the cookie! Omnomnom! =)
            myOtpNode.setCookie("secret");

            //Set up a mailbox
            myOtpMbox = myOtpNode.createMbox("java_server");

            //Create a Pid
            myPid = myOtpNode.createPid();

            //Loop until we get a request from which we can get a Pid.
            //With error handling that should be the case..
            //Now we just crash if we don't get what we want..
            while (lastPid == null) {

                //We get something.. hopefully a tuple
                OtpErlangTuple tuple = (OtpErlangTuple) myOtpMbox.receive();
                //Second argument of tuple is a message!
                OtpErlangAtom dispatch = (OtpErlangAtom) tuple.elementAt(1);

                if(dispatch.atomValue().equals("ping")) { //Yay we got a ping!
                    //Get the pid (first element of tuple) from the sending process
                    lastPid = (OtpErlangPid) tuple.elementAt(0);
                    System.out.println("Got connection from Erlang!");

                    //Put together a response and send it..
                    OtpErlangAtom pong = new OtpErlangAtom("pong");
                    OtpErlangObject[] sendArray = {myPid, pong};
                    OtpErlangTuple send = new OtpErlangTuple(sendArray);
                    myOtpMbox.send(lastPid, send);
                }
            }

            //Time for the next message..
            //Hopefully a request for a map..
            while (true) { 
                System.out.println("Waiting for request.");
                OtpErlangTuple tuple = (OtpErlangTuple) myOtpMbox.receive();
                OtpErlangAtom dispatch = (OtpErlangAtom) tuple.elementAt(1);

                
                if(dispatch.atomValue().equals("map_please")) { //Someone requesting map!
                    
                    System.out.println("Got map request from Erlang!");

                    //Get the requested map name from the message
                    String requested_map = tuple.elementAt(2).toString();
                    
                    //Remove unwanted chars (Erlang strings will be wrapped in "")
                    requested_map = requested_map.replaceAll("[^A-Za-z0-9_.-]", "");
                    System.out.println("Requested map: "+requested_map);

                    //Create a new map object of the wanted map
                    MapParser map = new MapParser(requested_map);

                    //Get some information about the map
                    OtpErlangInt map_height = new OtpErlangInt(map.get_height());
                    OtpErlangInt map_width = new OtpErlangInt(map.get_width());

                    //Get more infomration about the map
                    //But we gotta format this data so we call areaToErlang()
                    OtpErlangMap map_walls = areaToErlang(map.get_walls());
                    OtpErlangMap map_hospital = areaToErlang(map.get_hospital());

                    //put together a responce    
                    OtpErlangAtom message = new OtpErlangAtom("here_is_map");
                    OtpErlangObject[] sendArray = {myPid, message, map_width, map_height,
                                                   map_walls, map_hospital};
                    OtpErlangTuple send = new OtpErlangTuple(sendArray);
                    //And send it..
                    myOtpMbox.send(lastPid, send);

                    break; //We're done with map..
                    
                }else{ //Someone not asking for map! =O
                    System.out.println("Got unknown request");
                }
            }

	    while(true){
		OtpErlangAtom message = new OtpErlangAtom("ready_for_positions");
		myOtpMbox.send(lastPid, message);
                System.out.println("Waiting for positions.");
                OtpErlangTuple tuple = (OtpErlangTuple) myOtpMbox.receive();
                OtpErlangAtom dispatch = (OtpErlangAtom) tuple.elementAt(0);
		if(dispatch.atomValue().equals("updated_positions")) {
		    OtpErlangList new_positions = (OtpErlangList) tuple.elementAt(1);
		    Iterator itr = new_positions.iterator();
		    while(itr.hasNext()){
			OtpErlangTuple individual = (OtpErlangTuple) itr.next();
			OtpErlangPid pid = (OtpErlangPid) individual.elementAt(0);
			int sickness = ((OtpErlangLong) individual.elementAt(1)).intValue();
			int x = ((OtpErlangLong) individual.elementAt(2)).intValue();
			int y = ((OtpErlangLong) individual.elementAt(3)).intValue();
			System.out.println("PID: "+pid+" Sickness: "+sickness+" X: "+x+" Y: "+y);
		    }

		    //System.out.println("ASD" + new_positions);
		}else if(dispatch.atomValue().equals("simulation_done")){
		    System.out.println("Simulation done");
		    break;
		}else{
		    System.out.println("Got unknown request");
		    break;
		}
		Thread.sleep(1000);
	    }
            
        } catch (Exception e) {  //If we come here something is wrong =(
            e.printStackTrace();
        }
        
    }


    //Method used to format map data before we send it back..
    private static OtpErlangMap areaToErlang(Map<Integer, ArrayList> mapArea){

        //This map is not a map but a Map (the datatype)..
        OtpErlangMap map_objects = new OtpErlangMap();

        //Loop through the HashMap
        //And convert it to Erlang Objects
        for(Map.Entry<Integer, ArrayList> entry : mapArea.entrySet()){
            
            Integer key = entry.getKey();
            OtpErlangInt map_key = new OtpErlangInt(key);


            ArrayList value = entry.getValue();
            OtpErlangInt[] erl_int_array = new OtpErlangInt[value.size()];
                        
            for(int i = 0; i < value.size(); i++){
                OtpErlangInt val = new OtpErlangInt((Integer)value.get(i));
                erl_int_array[i] = val;
            }

            OtpErlangList erl_y_vals = new OtpErlangList(erl_int_array);
            map_objects.put(map_key, erl_y_vals);
        }
                        
        return map_objects; 
    }
}