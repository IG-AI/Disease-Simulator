package Menu;

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
    private JComboBox<String> randomBox;
    private JButton readmeButton;
    private String vaccinationStatus = "on";
    private String currentMap;
    private String currentMove;
    private String currentEnd;
    private String currentRandom;
    private String currentRecording;
    private List<String> defMap = new ArrayList<>();

    /**
     * Starting the Menu-GUI.
     *
     */

    public static void main(String[] args) throws UnsupportedEncodingException {
        GUIForm mainFrame = new GUIForm();
    }

    /**
     * Setting up the JSpinners, sets default values and limits the input
     *
     */

    private void spinnerSetup(){

        SpinnerNumberModel probability = new SpinnerNumberModel(1.00,0.00,1.00,0.01 );
        infectionProbabilitySpinner.setModel(probability);
        JFormattedTextField probabilityNumbers = ((JSpinner.NumberEditor) infectionProbabilitySpinner.getEditor()).getTextField();
        ((NumberFormatter) probabilityNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel individuals = new SpinnerNumberModel(1000,0,50000,1);
        numberOfIndividualsSpinner.setModel(individuals);
        JFormattedTextField individualNumbers = ((JSpinner.NumberEditor) numberOfIndividualsSpinner.getEditor()).getTextField();
        ((NumberFormatter) individualNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel ticks = new SpinnerNumberModel(5000, -1, 1000000000, 1);
        numberOfTicksSpinner.setModel(ticks);
        JFormattedTextField ticksNumbers = ((JSpinner.NumberEditor) numberOfTicksSpinner.getEditor()).getTextField();
        ((NumberFormatter) ticksNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel health = new SpinnerNumberModel(150,0,1000000000,1);
        numberOfHealthSpinner.setModel(health);
        JFormattedTextField healthlNumbers = ((JSpinner.NumberEditor) numberOfHealthSpinner.getEditor()).getTextField();
        ((NumberFormatter) healthlNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel infected = new SpinnerNumberModel(100,0,50000,1);
        numberOfInfectedSpinner.setModel(infected);
        JFormattedTextField infectedNumbers = ((JSpinner.NumberEditor) numberOfInfectedSpinner.getEditor()).getTextField();
        ((NumberFormatter) infectedNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel range = new SpinnerNumberModel(3,0,1000,1);
        rangeOfDiseaseSpinner.setModel(range);
        JFormattedTextField rangeNumbers = ((JSpinner.NumberEditor) rangeOfDiseaseSpinner.getEditor()).getTextField();
        ((NumberFormatter) rangeNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel vaccinated = new SpinnerNumberModel(100,0,50000,1);
        vaccinatedIndividualsSpinner.setModel(vaccinated);
        JFormattedTextField vaccinatedNumbers = ((JSpinner.NumberEditor) vaccinatedIndividualsSpinner.getEditor()).getTextField();
        ((NumberFormatter) vaccinatedNumbers.getFormatter()).setAllowsInvalid(false);

    }

    /**
     * Setting up the JComboBoxes
     *
     */

    private void setModeBox(){

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
        recordingBox.addItem("bg");
        randomBox.addItem("auto");
        randomBox.addItem("manual");

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
            for (File file : files){
                if (file.isFile()){
                    defMap.add(file.getName().replace(".bmp",""));
                    mapBox.addItem(file.getName().replace(".bmp",""));
                    currentMap = defMap.get(0); //Sets the default map to first one in the JComboBox
                    }
                }
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
            desktop.open(directory);
        }


    }

    /**
     * Adjusts the input so the total number of infected and vaccinated don't exceeds number of individuals
     *
     */

    private void adjustInput(){
        int compareInd = (int) numberOfIndividualsSpinner.getValue();
        int compareInf = (int) numberOfInfectedSpinner.getValue();
        int compareVac = (int) vaccinatedIndividualsSpinner.getValue();
        if((compareInf + compareVac) > compareInd){
            numberOfIndividualsSpinner.setValue(compareInf + compareVac);
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

            String[] ecommand = new String[]{ "gnome-terminal","-x","make", "run", inputIndividuals, inputVaccinated,
                    inputInfected, inputTicks, inputRange, inputInfectionProbability, inputHealth, "MAP=" + currentMap,
                    "END=" + currentEnd,"MOVE=" + currentMove, "TVAC=" + vaccinationStatus, "RAND=" + currentRandom,
                    "REC=" + currentRecording};
            Process proc = new ProcessBuilder(ecommand).directory(directory).start();
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
        spinnerSetup();
        setModeBox();

        numberOfIndividualsSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                adjustInput();
            }
        });

        numberOfInfectedSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                adjustInput();
            }
        });

        vaccinatedIndividualsSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                adjustInput();
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
            }
        });

        randomBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox)e.getSource();
                currentRandom = (String)cb.getSelectedItem();
            }
        });

        recordingBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox)e.getSource();
                currentRecording = (String)cb.getSelectedItem();
            }
        });

        moveBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox)e.getSource();
                currentMove = (String)cb.getSelectedItem();
            }
        });

        mapBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox)e.getSource();
                currentMap = (String)cb.getSelectedItem();
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

}