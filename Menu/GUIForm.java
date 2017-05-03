package Menu;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;

public class GUIForm extends JFrame {
    private JButton exitButton;
    private JButton startButton;
    private JButton openFileButton;
    private JTextField numberOfIndividualsTextField;
    private JTextField numberOfInfectedTextField;
    private JTextField numberOfTicsTextField;
    private JTextField healthInfected;
    private JTextField diseaseRange;
    private JTextField infectionProbability;
    private JLabel log;
    private JPanel mainFrame;
    private String fileName = "map_one.bmp";

    /**
     * Starting the Menu-GUI.
     */

    public static void main(String[] args) {
        GUIForm mainFrame = new GUIForm();
    }

    private void runErlang() throws IOException, InterruptedException {


        String inputIndividuals = "IND=" + numberOfIndividualsTextField.getText();
        String inputInfected = "INF=" + numberOfInfectedTextField.getText();
        String inputTics = "TICKS=" + numberOfTicsTextField.getText();
        String inputHealth = "LIFE=" + healthInfected.getText();
        String inputRange = "RANGE=" + diseaseRange.getText();
        String inputInfectionProbability = "PROB=" + infectionProbability.getText();
        fileName = "MAP=" + fileName;
        String[] ecommand = new String[]{ "xterm", "-e" , "make", "erun", inputIndividuals, inputInfected, inputTics, inputHealth, inputRange, fileName};
        Process eproc = new ProcessBuilder(ecommand).start();

    }

    public void runJava() throws IOException {
        String[] jcommand = new String[]{"xterm", "-e", "make", "jrun"};
        Process jproc = new ProcessBuilder(jcommand).start();
    }

    /**
     * Stores the input values.
     */


    private GUIForm() {
        super("Project Snowfox");
        setContentPane(mainFrame);
        pack();
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        JFileChooser chooser = new JFileChooser();
        FileNameExtensionFilter filter = new FileNameExtensionFilter("BMP Images", "bmp");
        chooser.setFileFilter(filter);
        openFileButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                chooser.setAcceptAllFileFilterUsed(false);
                int returnVal = chooser.showOpenDialog(mainFrame);

                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File file = chooser.getSelectedFile();
                    fileName = file.getName();
                    log.setText("You selected: " + file.getName() + ".\n");
                } else {
                    log.setText("Please select a BMP-file.");
                }
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
                } catch (IOException e1) {
                    e1.printStackTrace();
                } catch (InterruptedException e1) {
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