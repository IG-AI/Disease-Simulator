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
import java.nio.file.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.List;
import java.util.concurrent.TimeUnit;

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
    private static final DateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");

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
        currentRecordingFile = "playback not selected";

        recordedFileBox.addItem("playback not selected");
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

        String temp = Arrays.toString(files);


        if (files != null) {
            for (File file : files){
                if (file.isFile()){
                    defRec.add(file.getName().replace(".record",""));
                    recordedFileBox.addItem(file.getName().replace(".record",""));
                    currentRecordingFile = defRec.get(0); //Sets the default map to first one in the JComboBox
                }
            }

        }

        if(!directory.exists() || Objects.equals(temp, "[]")){
            recordedFileBox.removeAllItems();
            recordedFileBox.addItem("no recording");
            currentRecordingFile = "no recording";
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
     * If text-file is not created, create text and save start data else just save start data
     *
     * @throws IOException
     */

    private void createAndSaveText() throws IOException {

        URL url = GUIForm.class.getProtectionDomain().getCodeSource().getLocation();
        String folderPath = URLDecoder.decode(url.getFile(), "UTF-8");
        folderPath = folderPath.replace("Project-snowfox.jar", "");
        folderPath = folderPath + "logs/";
        String checkFile = folderPath + "java_data_log.log";
        Path path = FileSystems.getDefault().getPath(folderPath);

        Path filePath = FileSystems.getDefault().getPath(checkFile);

        if(!Files.exists(path))
        {
            File file = new File(folderPath);
            if(file.mkdir()){
                System.out.print("log folder created\n");
            }
            else
            {
                System.out.print("log could not be created\n");
            }
        }
        Date date = new Date();

        PrintWriter writer = null;

        if(!Files.exists(filePath)){
            writer = new PrintWriter(checkFile, "UTF-8");
            File file = new File(checkFile);
            if(file.exists()){
                System.out.print("log file created\n");
            }
            else{
                System.out.print("log file could not be created\n");
            }
        }
        if(Files.exists(filePath)) {

            String newline = "\n";

            String formatDate = "* Start values simulation: " + sdf.format(date) + " *\n";
            String formatInd = "Number of individuals: \n" + numberOfIndividualsSpinner.getValue() + newline;
            String formatInf = "Infected at start: \n" + numberOfInfectedSpinner.getValue() + newline;
            String formatVac = "Vaccinated individuals: \n" + vaccinatedIndividualsSpinner.getValue() + newline;
            String formatTicks = "Number of ticks: \n" + numberOfTicksSpinner.getValue() + newline;
            String formatHealth = "Health: \n" + numberOfHealthSpinner.getValue() + newline;
            String formatDisrange = "Disease range: \n" + rangeOfDiseaseSpinner.getValue() + newline;
            String formatProb = "Infection probability: \n" + infectionProbabilitySpinner.getValue() + newline;
            String formatEnd = "End condition: \n" + currentEnd + newline;
            String formatMove = "Movement: \n" + currentMove + newline;
            String formatRec = "Recording: \n" + currentRecording + newline;
            String formatRand = "Random: \n" + currentRandom + newline;
            String formatVacAtHospitals = "Vaccination in hospitals: \n" + vaccinationStatus + newline;
            String formatMap = "Selected map: \n" + currentMap + newline + newline;

            Files.write(Paths.get(checkFile), "\n************************************************\n".getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatDate.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), "************************************************\n\n".getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatInd.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatInf.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatVac.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatTicks.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatHealth.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatDisrange.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatProb.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatEnd.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatMove.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatRec.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatRand.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatVacAtHospitals.getBytes(), StandardOpenOption.APPEND);
            Files.write(Paths.get(checkFile), formatMap.getBytes(), StandardOpenOption.APPEND);

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

        ArrayList<String> ecommand;
        ArrayList<String> jcommand;

        jcommand = new ArrayList<>();
        ecommand = new ArrayList<>();

        if(Objects.equals(currentRecordingFile,"no recording") && Objects.equals(currentRecording, "playback")){
            mapInfo.setText("Recording is required");
        }

        if(Objects.equals(currentRecording, "playback") && !Objects.equals(currentRecordingFile, "no recording"))

            if(Objects.equals(getOS,"Linux")){

                jcommand.add("gnome-terminal");
                jcommand.add("-x");
                jcommand.add("make");
                jcommand.add("jrun");
                jcommand.add("REC=" + currentRecording);
                jcommand.add("RECFIL=" + currentRecordingFile);

                Process jproc = new ProcessBuilder(jcommand).directory(directory).start();

            }


            else if(Objects.equals(getOS, "Mac OS X")){

                String macJCommand = "tell application \"terminal\" to do script \"cd " + directory + ";make jrun " +
                        "REC=" + currentRecording + " RECFIL=" + currentRecordingFile + "\"";

                Process jproc = new ProcessBuilder("osascript", "-e", macJCommand).start();

            }

            else{
                mapInfo.setText("OS not supported");
            }

        else if(!Objects.equals(currentRecording, "playback")){

            if (Objects.equals(getOS, "Linux")) {

                jcommand.add("gnome-terminal");
                jcommand.add("-x");
                jcommand.add("make");
                jcommand.add("jrun");
                ecommand.add("gnome-terminal");
                ecommand.add("-x");
                ecommand.add("make");
                ecommand.add("erun");
                ecommand.add(inputIndividuals);
                ecommand.add(inputVaccinated);
                ecommand.add(inputInfected);
                ecommand.add(inputTicks);
                ecommand.add(inputRange);
                ecommand.add(inputInfectionProbability);
                ecommand.add(inputHealth);
                ecommand.add("MAP=" + currentMap);
                ecommand.add("END=" + currentEnd);
                ecommand.add("MOVE=" + currentMove);
                ecommand.add("TVAC=" + vaccinationStatus);
                ecommand.add("RAND=" + currentRandom);
                ecommand.add("REC=" + currentRecording);

                Process proc = new ProcessBuilder(ecommand).directory(directory).start();

                Process jproc = new ProcessBuilder(jcommand).directory(directory).start();

            }

            else if(Objects.equals(getOS, "Mac OS X")){

                String macECommand = "tell application \"terminal\" to do script \"cd " + directory + ";make run "
                        + inputIndividuals + " "
                        + inputVaccinated + " "
                        + inputInfected + " "
                        + inputTicks + " "
                        + inputRange + " "
                        + inputInfectionProbability + " "
                        + inputHealth + " "
                        + "MAP=" + currentMap + " "
                        + "END=" + currentEnd + " "
                        + "MOVE=" + currentMove + " "
                        + "TVAC=" + vaccinationStatus + " "
                        + "RAND=" + currentRandom + " "
                        + "REC=" + currentRecording + "\"";

                Process eproc = new ProcessBuilder("osascript","-e", macECommand).start();

                TimeUnit.MILLISECONDS.sleep(200);

                String macJCommand = "tell application \"terminal\" to do script \"cd " + directory + ";make jrun\"";

                Process jproc = new ProcessBuilder("osascript","-e", macJCommand).start();

            }

            else{
                mapInfo.setText("OS not supported");
            }


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
                    recordedFileBox.removeAllItems();
                    recordedFileBox.addItem("playback not selected");
                    currentRecordingFile = "playback not selected";
                }
                if(Objects.equals(currentRecording, "play_and_rec")) {
                    recordingBox.setToolTipText("Play the simulation and record it.");
                    recordedFileBox.removeAllItems();
                    recordedFileBox.addItem("playback not selected");
                    currentRecordingFile = "playback not selected";
                }
                if(Objects.equals(currentRecording, "rec")){
                    recordingBox.setToolTipText("Only record the simulation, nothing will be displayed");
                    recordedFileBox.removeAllItems();
                    recordedFileBox.addItem("playback not selected");
                    currentRecordingFile = "playback not selected";
                }
                if(Objects.equals(currentRecording, "playback")) {
                    recordingBox.setToolTipText("Play recording, **NOTE** all the parameters selected won't change the recorded simulation");
                    try {
                        recordedFileBox.removeAllItems();
                        getRecordingFiles();
                    } catch (UnsupportedEncodingException e1) {
                        e1.printStackTrace();
                    }
                }
                if(Objects.equals(currentRecording, "bg")){
                    recordingBox.setToolTipText("Run the simulation in the background, nothing will be recorded nor displayed");
                    recordedFileBox.removeAllItems();
                    recordedFileBox.addItem("playback not selected");
                    currentRecordingFile = "playback not selected";
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
                 currentRecordingFile = (String)cb.getSelectedItem();
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
                        createAndSaveText();
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