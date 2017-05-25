package Menu;

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.NumberFormatter;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.*;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class GUIForm extends JFrame {
    private JButton exitButton;
    private JButton startButton;
    private JPanel mainFrame;
    private JComboBox<String> mapBox;
    private JSpinner numberOfIndividualsSpinner;
    private JSpinner infectionProbabilitySpinner;
    private JSpinner rangeOfDiseaseSpinner;
    private JSpinner numberOfHealthSpinner;
    private JSpinner numberOfTicksSpinner;
    private JSpinner numberOfInfectedSpinner;
    private JSpinner vaccinatedIndividualsSpinner;
    private javax.swing.JLabel mapInfo;
    private JComboBox<String> endBox;
    private JComboBox<String> moveBox;
    private JCheckBox vaccinationCheckBox;
    private JComboBox<String> recordingBox;
    private JComboBox<String> recordedFileBox;
    private JComboBox<String> randomBox;
    private JButton readmeButton;
    private String vaccinationStatus = "on";
    private String currentMap;
    private String currentMove;
    private String currentEnd;
    private String currentRandom;
    private String currentRecording;
    private String currentRecordingFile;
    private List<String> defMap = new ArrayList<>();
    private List<String> defRec = new ArrayList<>();

    /**
     * Starting the Menu-GUI.
     */

    public static void main(String[] args) throws IOException {
        GUIForm mainFrame = new GUIForm();
        setIconImage(mainFrame);
    }

    /**
     * Setting up the JSpinners, sets default values and limits the input
     */

    private void spinnerSetup() {

        SpinnerNumberModel probability = new SpinnerNumberModel(1.00, 0.00, 1.00, 0.01);
        infectionProbabilitySpinner.setModel(probability);
        JFormattedTextField probabilityNumbers = ((JSpinner.NumberEditor) infectionProbabilitySpinner.getEditor()).getTextField();
        ((NumberFormatter) probabilityNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel individuals = new SpinnerNumberModel(1000, 1, 50000, 1);
        numberOfIndividualsSpinner.setModel(individuals);
        JFormattedTextField individualNumbers = ((JSpinner.NumberEditor) numberOfIndividualsSpinner.getEditor()).getTextField();
        ((NumberFormatter) individualNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel ticks = new SpinnerNumberModel(5000, -1, 1000000000, 1);
        numberOfTicksSpinner.setModel(ticks);
        JFormattedTextField ticksNumbers = ((JSpinner.NumberEditor) numberOfTicksSpinner.getEditor()).getTextField();
        ((NumberFormatter) ticksNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel health = new SpinnerNumberModel(150, 0, 1000000000, 1);
        numberOfHealthSpinner.setModel(health);
        JFormattedTextField healthlNumbers = ((JSpinner.NumberEditor) numberOfHealthSpinner.getEditor()).getTextField();
        ((NumberFormatter) healthlNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel infected = new SpinnerNumberModel(100, 0, 50000, 1);
        numberOfInfectedSpinner.setModel(infected);
        JFormattedTextField infectedNumbers = ((JSpinner.NumberEditor) numberOfInfectedSpinner.getEditor()).getTextField();
        ((NumberFormatter) infectedNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel range = new SpinnerNumberModel(3, 0, 1000, 1);
        rangeOfDiseaseSpinner.setModel(range);
        JFormattedTextField rangeNumbers = ((JSpinner.NumberEditor) rangeOfDiseaseSpinner.getEditor()).getTextField();
        ((NumberFormatter) rangeNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel vaccinated = new SpinnerNumberModel(100, 0, 50000, 1);
        vaccinatedIndividualsSpinner.setModel(vaccinated);
        JFormattedTextField vaccinatedNumbers = ((JSpinner.NumberEditor) vaccinatedIndividualsSpinner.getEditor()).getTextField();
        ((NumberFormatter) vaccinatedNumbers.getFormatter()).setAllowsInvalid(false);

    }

    /**
     * Setting up the JComboBoxes
     */

    private void setModeBox() {

        currentMove = "bounce";
        currentEnd = "dead";
        currentRecording = "play";
        currentRandom = "auto";

        endBox.addItem("dead");
        endBox.addItem("ticks");
        endBox.addItem("infected");
        moveBox.addItem("bounce");
        moveBox.addItem("bounce_random");
        moveBox.addItem("path");
        recordingBox.addItem("play");
        recordingBox.addItem("play_and_rec");
        recordingBox.addItem("rec");
        recordingBox.addItem("playback");
        recordingBox.addItem("bg");
        randomBox.addItem("auto");
        randomBox.addItem("manual");

        endBox.setToolTipText("The simulation will stop when either all individuals are healthy, all individuals are dead or TICKS have been depleted");
        moveBox.setToolTipText("Uses bouncing behaviour");
        recordingBox.setToolTipText("Play the simulation with no recording");
        randomBox.setToolTipText("Each simulation with the same parameters will be different");
    }

    /**
     * Reads the data folder and adds the .bmp files to the JComboBox
     *
     * @throws UnsupportedEncodingException
     */

    private void getMapFiles() throws UnsupportedEncodingException {

        URL url = GUIForm.class.getProtectionDomain().getCodeSource().getLocation();
        String folderPath = URLDecoder.decode(url.getFile(), "UTF-8");
        folderPath = folderPath.replace("Project-snowfox.jar", "");
        File directory = new File(folderPath + "data");

        File[] files = directory.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return name.toLowerCase().endsWith(".bmp");
            }
        });

        if (files != null) {
            for (File file : files) {
                if (file.isFile()) {
                    defMap.add(file.getName().replace(".bmp", ""));
                    mapBox.addItem(file.getName().replace(".bmp", ""));
                    currentMap = defMap.get(0); //Sets the default map to first one in the JComboBox
                }
            }
        }
    }

    private void getRecordingFiles() throws UnsupportedEncodingException {

        URL url = GUIForm.class.getProtectionDomain().getCodeSource().getLocation();
        String folderPath = URLDecoder.decode(url.getFile(), "UTF-8");
        folderPath = folderPath.replace("Project-snowfox.jar", "");
        File directory = new File(folderPath + "recordings");

        File[] files = directory.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return name.toLowerCase().endsWith(".record");
            }
        });

        if (files != null && !recordingBox.getSelectedItem().toString().equals("playback")) {
            for (File file : files){
                if (file.isFile()){
                    defRec.add(file.getName().replace(".record",""));
                    recordedFileBox.addItem(file.getName().replace(".record",""));
                    currentRecordingFile = defRec.get(0); //Sets the default map to first one in the JComboBox
                }
            }
        }
        else {
            recordedFileBox.addItem("None");
        }
    }

    /**
     * Opens the readme-file
     *
     * @throws IOException
     */

    private void openReadme() throws IOException {

        if(!Desktop.isDesktopSupported()){
            mapInfo.setText("OS not supported");
        }
        else{
            URL url = GUIForm.class.getProtectionDomain().getCodeSource().getLocation();
            String folderPath = URLDecoder.decode(url.getFile(), "UTF-8");
            folderPath = folderPath.replace("Project-snowfox.jar", "README.md");

            File directory = new File(folderPath);
            Desktop desktop = Desktop.getDesktop();
            if(folderPath.endsWith("README.md")) {
                desktop.open(directory);
            }
                else {
                mapInfo.setText("Readme not found");
            }
        }
    }


    /**
     * Runs a bash-script containing "make run" plus the values read by the menu
     *
     * @throws IOException
     * @throws InterruptedException
     */

    private void run() throws IOException, InterruptedException {

        URL url = GUIForm.class.getProtectionDomain().getCodeSource().getLocation();
        String folderPath = URLDecoder.decode(url.getFile(), "UTF-8");
        folderPath = folderPath.replace("Project-snowfox.jar", "");
        File directory = new File(folderPath);


        String inputIndividuals = "IND=" + numberOfIndividualsSpinner.getValue();
        String inputInfected = "INF=" + numberOfInfectedSpinner.getValue();
        String inputTicks = "TICKS=" + numberOfTicksSpinner.getValue();
        String inputHealth = "LIFE=" + numberOfHealthSpinner.getValue();
        String inputRange = "RANGE=" + rangeOfDiseaseSpinner.getValue();
        String inputInfectionProbability = "PROB=" + infectionProbabilitySpinner.getValue();
        String inputVaccinated = "VAC=" + vaccinatedIndividualsSpinner.getValue();


        String getOS = System.getProperty("os.name");

        if(Objects.equals(getOS, "Linux")){


            String[] ecommand = new String[]{ "gnome-terminal","-x","make", "erun", inputIndividuals, inputVaccinated,
                    inputInfected, inputTicks, inputRange, inputInfectionProbability, inputHealth, "MAP=" + currentMap,
                    "END=" + currentEnd,"MOVE=" + currentMove, "TVAC=" + vaccinationStatus, "RAND=" + currentRandom,
                    "REC=" + currentRecording};

            Process proc = new ProcessBuilder(ecommand).directory(directory).start();

            String[] jcommand = new String[]{"gnome-terminal", "-x", "make" ,"jrun", "REC=" + currentRecording, "RECFIL=" + currentRecordingFile};

            Process jproc = new ProcessBuilder(jcommand).directory(directory).start();

        }

        else{
            mapInfo.setText("OS not supported");
        }
    }

    /**
     * Starts the menu
     *
     * @throws UnsupportedEncodingException
     */

    private GUIForm() throws UnsupportedEncodingException {
        super("Project Snowfox");
        setContentPane(mainFrame);
        pack();
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        getMapFiles();
        getRecordingFiles();
        spinnerSetup();
        setModeBox();

        numberOfIndividualsSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                int compareInd = (int) numberOfIndividualsSpinner.getValue();
                int compareInf = (int) numberOfInfectedSpinner.getValue();
                int compareVac = (int) vaccinatedIndividualsSpinner.getValue();
                if((compareInf + compareVac) > compareInd){
                    if(compareInf <= compareInd){
                        vaccinatedIndividualsSpinner.setValue(compareInd - compareInf);
                    }
                    else if (compareInf > compareInd && compareInd > compareVac){
                        vaccinatedIndividualsSpinner.setValue(0);
                        numberOfInfectedSpinner.setValue(compareInd);
                    }
                    else if (compareInf > compareInd && compareVac > compareInd){
                        vaccinatedIndividualsSpinner.setValue(0);
                        numberOfInfectedSpinner.setValue(compareInd);
                    }
                }

            }
        });

        numberOfInfectedSpinner.addChangeListener(new ChangeListener() {

            @Override
            public void stateChanged(ChangeEvent e) {

                int compareInd = (int) numberOfIndividualsSpinner.getValue();
                int compareInf = (int) numberOfInfectedSpinner.getValue();
                int compareVac = (int) vaccinatedIndividualsSpinner.getValue();
                if((compareInf + compareVac) > compareInd) {
                    numberOfIndividualsSpinner.setValue(compareInf + compareVac);
                }

            }
        });

        vaccinatedIndividualsSpinner.addChangeListener(new ChangeListener() {

            @Override
            public void stateChanged(ChangeEvent e) {

                int compareInd = (int) numberOfIndividualsSpinner.getValue();
                int compareInf = (int) numberOfInfectedSpinner.getValue();
                int compareVac = (int) vaccinatedIndividualsSpinner.getValue();
                if((compareInf + compareVac) > compareInd && compareVac !=0) {
                    numberOfIndividualsSpinner.setValue(compareInf + compareVac);
                }

            }
        });

        vaccinationCheckBox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                if(e.getStateChange() == ItemEvent.SELECTED){
                    vaccinationStatus = "on";
                }
                if(e.getStateChange() == ItemEvent.DESELECTED){
                    vaccinationStatus = "off";
                }
            }
        });

        endBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox)e.getSource();
                currentEnd = (String)cb.getSelectedItem();
                if(Objects.equals(currentEnd, "dead")){
                    endBox.setToolTipText("The simulation will stop when either all individuals are healthy, all individuals are dead or TICKS have been depleted.");
                }
                if(Objects.equals(currentEnd, "ticks")){
                    endBox.setToolTipText("The simulation will only stop when TICKS is depleted. Note that if this option is chosen and TICKS are set to a negative value the simulation will run indefinitely.");
                }
                if(Objects.equals(currentEnd, "infected")){
                    endBox.setToolTipText("The simulation will stop when either all individuals are healthy, all individuals are infected or TICKS have been depleted.");
                }
            }
        });

        randomBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox)e.getSource();
                currentRandom = (String)cb.getSelectedItem();
                if(Objects.equals(currentRandom, "auto")){
                    randomBox.setToolTipText("Each simulation with the same parameters will be different");
                }
                if(Objects.equals(currentRandom, "manual")){
                    randomBox.setToolTipText("Use a predetermined seed resulting in identical simulations if the same parameters are use");
                }
            }
        });

        recordingBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox)e.getSource();
                currentRecording = (String)cb.getSelectedItem();
                if(Objects.equals(currentRecording, "play")){
                    recordingBox.setToolTipText("Play the simulation with no recording");
                }
                if(Objects.equals(currentRecording, "play_and_rec")) {
                    recordingBox.setToolTipText("Play the simulation and record it.");
                }
                if(Objects.equals(currentRecording, "rec")){
                    recordingBox.setToolTipText("Only record the simulation, nothing will be displayed");
                }
                if(Objects.equals(currentRecording, "playback")) {
                    recordingBox.setToolTipText("Play recording");
                }
                if(Objects.equals(currentRecording, "bg")){
                    recordingBox.setToolTipText("Run the simulation in the background, nothing will be recorded nor displayed");
                }
            }
        });

        moveBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox)e.getSource();
                currentMove = (String)cb.getSelectedItem();
                if(Objects.equals(currentMove, "bounce")){
                    moveBox.setToolTipText("Uses bouncing behaviour");
                }
                if(Objects.equals(currentMove, "bounce_random")){
                    moveBox.setToolTipText("Uses bouncing behaviour");
                }
                if(Objects.equals(currentMove, "path")){
                    moveBox.setToolTipText("Use A* for pathfinding **Very slow to start**");
                }
            }
        });

        mapBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox)e.getSource();
                currentMap = (String)cb.getSelectedItem();
                mapInfo.setText("Select a map:");
            }
        });
        readmeButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    openReadme();
                } catch (IOException e1) {
                    e1.printStackTrace();
                }
            }
        });

        recordedFileBox.addActionListener(new ActionListener() {
             @Override
             public void actionPerformed(ActionEvent e) {
                 JComboBox cb = (JComboBox)e.getSource();
                 currentRecording = (String)cb.getSelectedItem();
                 mapInfo.setText("Select a recording:");
             }
        });

        exitButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                System.exit(0);
            }
        });
        startButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if(!defMap.isEmpty()){
                    try {
                        run();
                    } catch (IOException | InterruptedException e1) {
                        e1.printStackTrace();
                    }
                }
                else{
                    mapInfo.setText("No map, no start");
                }
            }
        });
        setVisible(true);
    }

    private static void setIconImage(JFrame frame) throws IOException {
        try {
            frame.setIconImage(ImageIO.read(new File("data/icon.png")));
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

}