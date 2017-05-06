package Menu;

import javax.swing.*;
import javax.swing.text.NumberFormatter;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
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
    private String currentMap;
    private List<String> defMap = new ArrayList<>();

    /**
     * Starting the Menu-GUI.
     */

    public static void main(String[] args) {
        GUIForm mainFrame = new GUIForm();
    }

    private void spinnerSetup(){
        SpinnerNumberModel probability = new SpinnerNumberModel(1.00,0.00,1.00,0.01 );
        infectionProbabilitySpinner.setModel(probability);
        ((JSpinner.DefaultEditor) infectionProbabilitySpinner.getEditor()).getTextField().setEditable(false);

        SpinnerNumberModel individuals = new SpinnerNumberModel(300,0,10000,1);
        numberOfIndividualsSpinner.setModel(individuals);
        JFormattedTextField individualNumbers = ((JSpinner.NumberEditor) numberOfIndividualsSpinner.getEditor()).getTextField();
        ((NumberFormatter) individualNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel tics = new SpinnerNumberModel(300, 1, 1000000, 1);
        numberOfTicsSpinner.setModel(tics);
        JFormattedTextField ticsNumbers = ((JSpinner.NumberEditor) numberOfTicsSpinner.getEditor()).getTextField();
        ((NumberFormatter) ticsNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel health = new SpinnerNumberModel(50,1,10000,1);
        numberOfHealthSpinner.setModel(health);
        JFormattedTextField healthlNumbers = ((JSpinner.NumberEditor) numberOfHealthSpinner.getEditor()).getTextField();
        ((NumberFormatter) healthlNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel infected = new SpinnerNumberModel(25,1,9999,1);
        numberOfInfectedSpinner.setModel(infected);
        JFormattedTextField infectedNumbers = ((JSpinner.NumberEditor) numberOfInfectedSpinner.getEditor()).getTextField();
        ((NumberFormatter) infectedNumbers.getFormatter()).setAllowsInvalid(false);

        SpinnerNumberModel range = new SpinnerNumberModel(10,1,1000,1);
        rangeOfDiseaseSpinner.setModel(range);
        JFormattedTextField rangeNumbers = ((JSpinner.NumberEditor) rangeOfDiseaseSpinner.getEditor()).getTextField();
        ((NumberFormatter) rangeNumbers.getFormatter()).setAllowsInvalid(false);
    }

    private void getMapFiles(){

        String folderPath = System.getProperty("user.dir") +"/data";
        File directory = new File(folderPath);

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

                }
            }
        }
    }

    private void runErlang() throws IOException, InterruptedException {

        String inputIndividuals = "IND=" + numberOfIndividualsSpinner.getValue();
        String inputInfected = "INF=" + numberOfInfectedSpinner.getValue();
        String inputTics = "TICKS=" + numberOfTicsSpinner.getValue();
        String inputHealth = "LIFE=" + numberOfHealthSpinner.getValue();
        String inputRange = "RANGE=" + rangeOfDiseaseSpinner.getValue();
        String inputInfectionProbability = "PROB=" + infectionProbabilitySpinner.getValue();
        String[] ecommand = new String[]{ "xterm", "-e" , "make", "erun", inputIndividuals, inputInfected, inputTics, inputHealth, inputRange, inputInfectionProbability, "MAP=" + currentMap};
        Process eproc = new ProcessBuilder(ecommand).start();
    }

    private void runJava() throws IOException {
        String[] jcommand = new String[]{"xterm", "-e", "make", "jrun"};
        Process jproc = new ProcessBuilder(jcommand).start();
    }

    private GUIForm() {
        super("Project Snowfox");
        setContentPane(mainFrame);
        pack();
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        getMapFiles();
        currentMap = defMap.get(0); //Sets the default map to first one in the JComboBox
        spinnerSetup();
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
        });
        setVisible(true);
    }
}