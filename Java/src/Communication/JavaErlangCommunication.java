
package Communication;



import com.ericsson.otp.erlang.*;
import java.io.IOException;
import java.util.*;
import java.awt.*;

/**
 * Provides functionality for connecting to an Erlang process
 * @author Project Snowfox
 */
public class JavaErlangCommunication {
    //Vars we want to use.

    public boolean simulationDone = false;
    public static OtpErlangPid erlangPid = null;
    public OtpErlangPid myPid = null;
    public static OtpMbox myOtpMbox = null;
    public OtpNode myOtpNode = null;
    private String mapName = null;
    private MapParser map = null;


    /**
     * Create the communication object.
     * During creation it will wait for a connection from the Erlang process
     * and parse the map Erlang is requesting.
     */
    public JavaErlangCommunication() throws RuntimeException {

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
            while (erlangPid == null) {
                System.out.println("Waiting for Erlang connection");
                //We get something.. hopefully a tuple
                OtpErlangTuple tuple = (OtpErlangTuple) myOtpMbox.receive();
                //Second argument of tuple is a message!

                OtpErlangAtom dispatch = (OtpErlangAtom) tuple.elementAt(1);

                if(dispatch.atomValue().equals("ping")) { //Yay we got a ping!
                    //Get the pid (first element of tuple) from the sending process
                    erlangPid = (OtpErlangPid) tuple.elementAt(0);
                    System.out.println("Got connection from Erlang!");

                    //Put together a response and send it..
                    OtpErlangAtom pong = new OtpErlangAtom("pong");
                    OtpErlangObject[] sendArray = {myPid, pong};
                    OtpErlangTuple send = new OtpErlangTuple(sendArray);
                    myOtpMbox.send(erlangPid, send);
                }
            }

            //Time for the next message..
            //Hopefully a request for a map..
            while (true) {
                System.out.println("Waiting for request.");
                OtpErlangTuple tuple = (OtpErlangTuple) myOtpMbox.receive();
                OtpErlangAtom dispatch = (OtpErlangAtom) tuple.elementAt(
1);


                if(dispatch.atomValue().equals("map_please")) { //Someone requesting map!

                    System.out.println("Got map request from Erlang!");

                    //Get the requested map name from the message
                    String requested_map = tuple.elementAt(2).toString();

                    //Remove unwanted chars (Erlang strings will be wrapped in "")
                    requested_map = requested_map.replaceAll("[^A-Za-z0-9_.-]", "");
                    mapName = "data/" + requested_map;
                    System.out.println("Requested map: "+requested_map);

                    //Create a new map object of the wanted map
                    map = new MapParser(mapName);


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
                    myOtpMbox.send(erlangPid, send);

                    while(true) {

                        OtpErlangTuple tuple2 = (OtpErlangTuple) myOtpMbox.receive();
                        int size = tuple2.elements().length;
                        if(size == 1){
                            OtpErlangAtom dispatch2 = (OtpErlangAtom) tuple2.elementAt(0);
                            if(dispatch2.atomValue().equals("set_up_for_requests")) {
                                System.out.println("Setting up for requests.");
                                break;
                            }else if(dispatch2.atomValue().equals("simulation_done")){
                                this.simulationDone = true;
                                break;
                            }
                        }
                    }
                    
                    break; //We're done with map..

                }else{ //Someone not asking for map! =O
                    System.out.println("Got unknown request");
                }
            }


        } catch (RuntimeException | IOException | OtpErlangDecodeException | OtpErlangExit e) {  //If we come here something is wrong =(
            e.printStackTrace();
        }

    }


    //Method used to format map data before we send it back..
    private static OtpErlangMap areaToErlang(Map<Integer, ArrayList> mapArea) {

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


    /**
     * Let the user request new positions from the Erlang process.
     *
     * @return false or an ArrayList containing several ArrayLists. These in turn
     * consist of [IndividualPid :: OtpErlangPid, Sickness :: int, X-coord :: int, Y-coord :: int]
     */
    public static ArrayList recievePos() throws OtpErlangRangeException {

        //System.out.println("Waiting for positions");
        OtpErlangAtom message = new OtpErlangAtom("ready_for_positions");
        myOtpMbox.send(erlangPid, message); //tell Erlang we're ready for new positions
        OtpErlangTuple erlangTuple = null;

        try {
            erlangTuple = (OtpErlangTuple) myOtpMbox.receive(); //wait for message from Erlang
        } catch (OtpErlangExit | OtpErlangDecodeException otpErlangExit) {
            otpErlangExit.printStackTrace();
        }

        return convertPosErlangJava(erlangTuple); //convert and return the requested array
    }


    private static ArrayList convertPosErlangJava(OtpErlangTuple tuple) throws OtpErlangRangeException {

        ArrayList<ArrayList> list = new ArrayList<>();
        OtpErlangAtom dispatch = (OtpErlangAtom) tuple.elementAt(0);
        if (dispatch.atomValue().equals("updated_positions")) { //check that the message will contain positions
            OtpErlangList new_positions = (OtpErlangList) tuple.elementAt(1);
            for (Object new_position : new_positions) {
                ArrayList<Object> listpos = new ArrayList<>(); //create a new ArrayList every loop so it's unique.
                //convert the information about an individual from Erlang data types to Java datatupes.
                OtpErlangTuple individual = (OtpErlangTuple) new_position;
                OtpErlangPid pid = (OtpErlangPid) individual.elementAt(0);
                int sickness = ((OtpErlangLong) individual.elementAt(1)).intValue();
                int x = ((OtpErlangLong) individual.elementAt(2)).intValue();
                int y = ((OtpErlangLong) individual.elementAt(3)).intValue();

                //add all information to an ArrayList
                listpos.add(pid);
                listpos.add(sickness);
                listpos.add(x);
                listpos.add(y);
                list.add(listpos);
            }
            return list;
        }
        else if (dispatch.atomValue().equals("simulation_done")) { //if the simulation is done.
            return null;
        }
        else {//if the message from Erlang contain some other message
            return null;
        }
    }


    /**
     * Returns the map to use.
     *
     * @return the name and path to the requested map
     */
    public String getMapName() {
        return mapName;
    }

    public Image getMapImage(){
        return map.get_map();
    }
}
