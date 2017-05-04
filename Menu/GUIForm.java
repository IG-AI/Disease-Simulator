package Menu;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class GUIForm extends JFrame {
    private JButton exitButton;
    private JButton startButton;
    private JTextField numberOfIndividualsTextField;
    private JTextField numberOfInfectedTextField;
    private JTextField numberOfTicsTextField;
    private JTextField healthInfected;
    private JTextField diseaseRange;
    private JTextField infectionProbability;
    private JPanel mainFrame;
    private JComboBox<String> comboBox;
    private String currentMap;
    private List<String> defMap = new ArrayList<String>();

    /**
     * Starting the Menu-GUI.
     */

    public static void main(String[] args) {
        GUIForm mainFrame = new GUIForm();
    }



    private void getMapFiles(){

        String folderPath = System.getProperty("user.dir") +"/data";
        File directory = new File(folderPath);

        File[] fList = directory.listFiles();
        if (fList != null) {
            for (File file : fList){
                if (file.isFile()){
                    defMap.add(file.getName());
                    comboBox.addItem(file.getName());

                }
            }
        }
    }

    private void runErlang() throws IOException, InterruptedException {


        String inputIndividuals = "IND=" + numberOfIndividualsTextField.getText();
        String inputInfected = "INF=" + numberOfInfectedTextField.getText();
        String inputTics = "TICKS=" + numberOfTicsTextField.getText();
        String inputHealth = "LIFE=" + healthInfected.getText();
        String inputRange = "RANGE=" + diseaseRange.getText();
        String inputInfectionProbability = "PROB=" + infectionProbability.getText();
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
        currentMap = defMap.get(0);
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