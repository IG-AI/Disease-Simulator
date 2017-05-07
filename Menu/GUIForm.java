package Menu;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.NumberFormatter;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.List;

public class GUIForm extends JFrame {
    private JButton exitButton;
    private JButton startButton;
    private JPanel mainFrame;
    private JComboBox<String> comboBox;
    private JSpinner numberOfIndividualsSpinner;
    private JSpinner infectionProbabilitySpinner;
    private JSpinner rangeOfDiseaseSpinner;
    private JSpinner numberOfHealthSpinner;
    private JSpinner numberOfTicsSpinner;
    private JSpinner numberOfInfectedSpinner;
    private javax.swing.JLabel mapInfo;
    private String currentMap;
    private List<String> defMap = new ArrayList<>();
    private int compareInd;
    private int compareInf;

    /**
     * Starting the Menu-GUI.
     */

    public static void main(String[] args) throws UnsupportedEncodingException {
        GUIForm mainFrame = new GUIForm();
    }

    private void spinnerSetup(){
        SpinnerNumberModel probability = new SpinnerNumberModel(1.00,0.00,1.00,0.01 );
        infectionProbabilitySpinner.setModel(probability);
        JFormattedTextField probabilityNumbers = ((JSpinner.NumberEditor) infectionProbabilitySpinner.getEditor()).getTextField();
        ((NumberFormatter) probabilityNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel individuals = new SpinnerNumberModel(300,0,10000,1);
        numberOfIndividualsSpinner.setModel(individuals);
        JFormattedTextField individualNumbers = ((JSpinner.NumberEditor) numberOfIndividualsSpinner.getEditor()).getTextField();
        ((NumberFormatter) individualNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel tics = new SpinnerNumberModel(300, 0, 1000000, 1);
        numberOfTicsSpinner.setModel(tics);
        JFormattedTextField ticsNumbers = ((JSpinner.NumberEditor) numberOfTicsSpinner.getEditor()).getTextField();
        ((NumberFormatter) ticsNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel health = new SpinnerNumberModel(50,0,10000,1);
        numberOfHealthSpinner.setModel(health);
        JFormattedTextField healthlNumbers = ((JSpinner.NumberEditor) numberOfHealthSpinner.getEditor()).getTextField();
        ((NumberFormatter) healthlNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel infected = new SpinnerNumberModel(25,0,10000,1);
        numberOfInfectedSpinner.setModel(infected);
        JFormattedTextField infectedNumbers = ((JSpinner.NumberEditor) numberOfInfectedSpinner.getEditor()).getTextField();
        ((NumberFormatter) infectedNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel range = new SpinnerNumberModel(10,0,1000,1);
        rangeOfDiseaseSpinner.setModel(range);
        JFormattedTextField rangeNumbers = ((JSpinner.NumberEditor) rangeOfDiseaseSpinner.getEditor()).getTextField();
        ((NumberFormatter) rangeNumbers.getFormatter()).setAllowsInvalid(false);
    }

    private void getMapFiles() throws UnsupportedEncodingException {

        URL url = GUIForm.class.getProtectionDomain().getCodeSource().getLocation();
        String folderPath = URLDecoder.decode(url.getFile(), "UTF-8");
        folderPath = folderPath.replace("Project-snowfox-linux.jar", "");
        File directory = new File(folderPath + "data");

        File[] files = directory.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return name.toLowerCase().endsWith(".bmp");
            }
        });

        if (files != null) {
            for (File file : files){
                if (file.isFile()){
                    defMap.add(file.getName());
                    comboBox.addItem(file.getName());
                    currentMap = defMap.get(0); //Sets the default map to first one in the JComboBox
                    }
                }
            }
        }


    private void runErlang() throws IOException, InterruptedException {

        URL url = GUIForm.class.getProtectionDomain().getCodeSource().getLocation();
        String folderPath = URLDecoder.decode(url.getFile(), "UTF-8");
        folderPath = folderPath.replace("Project-snowfox-linux.jar", "");
        File directory = new File(folderPath);

        String inputIndividuals = "IND=" + numberOfIndividualsSpinner.getValue();
        String inputInfected = "INF=" + numberOfInfectedSpinner.getValue();
        String inputTics = "TICKS=" + numberOfTicsSpinner.getValue();
        String inputHealth = "LIFE=" + numberOfHealthSpinner.getValue();
        String inputRange = "RANGE=" + rangeOfDiseaseSpinner.getValue();
        String inputInfectionProbability = "PROB=" + infectionProbabilitySpinner.getValue();

        String[] ecommand = new String[]{ "xterm", "-e" , "make", "erun", inputIndividuals, inputInfected, inputTics, inputRange, inputInfectionProbability, inputHealth, "MAP=" + currentMap};
        Process eproc = new ProcessBuilder(ecommand).directory(directory).start();
    }

    private void runJava() throws IOException {

        URL url = GUIForm.class.getProtectionDomain().getCodeSource().getLocation();
        String folderPath = URLDecoder.decode(url.getFile(), "UTF-8");
        folderPath = folderPath.replace("Project-snowfox-linux.jar", "");
        File directory = new File(folderPath);

        String[] jcommand = new String[]{"xterm", "-e", "make", "jrun"};
        Process jproc = new ProcessBuilder(jcommand).directory(directory).start();
    }

    private GUIForm() throws UnsupportedEncodingException {
        super("Project Snowfox");
        setContentPane(mainFrame);
        pack();
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        getMapFiles();
        spinnerSetup();

        numberOfIndividualsSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                compareInd = (int) numberOfIndividualsSpinner.getValue();
                compareInf = (int) numberOfInfectedSpinner.getValue();
                if(compareInf > compareInd){
                    numberOfInfectedSpinner.setValue(compareInd);

                }
            }
        });

        numberOfInfectedSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                compareInd = (int) numberOfIndividualsSpinner.getValue();
                compareInf = (int) numberOfInfectedSpinner.getValue();
                if(compareInf > compareInd){
                    numberOfInfectedSpinner.setValue(compareInd);

                }
            }
        });

        comboBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox)e.getSource();
                currentMap = (String)cb.getSelectedItem();
            }
        });
        exitButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                dispose();
                System.exit(0);
            }
        });
        startButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if(!defMap.isEmpty()){
                    try {
                        runErlang();
                    } catch (IOException | InterruptedException e1) {
                        e1.printStackTrace();
                    }
                    try {
                        runJava();
                    } catch (IOException e1) {
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