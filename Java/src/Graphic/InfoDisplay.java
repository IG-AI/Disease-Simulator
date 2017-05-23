package Graphic;

import com.ericsson.otp.erlang.OtpErlangRangeException;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;

public class InfoDisplay extends JPanel {
    private JFrame infoDisplay;
    private static JPanel scorePanel;
    private static JLabel totalUnitsLabel;
    private static JLabel unitsLabel;
    private static JLabel sickUnitsLabel;
    private static JLabel deadUnitsLabel;
    private static JLabel vaccinationLabel;
    private static JLabel ticksLabel;
    public static int startNumberOfUnits;

    /**
     * Method that makes the information display.
     *
     * @param maxNumberOfUnitsumberOfUnits the total amount for units in the simulation as an int.
     * @param numberOfUnits                the current number of units in the simulation as an int.
     * @param frame                        main frame as a JFrame.
     * @throws OtpErlangRangeException
     * @throws IOException
     */
    public InfoDisplay(int maxNumberOfUnitsumberOfUnits, int numberOfUnits, JFrame frame) throws OtpErlangRangeException, IOException {
        infoDisplay = new JFrame("Project Snowfox - Information Display");
        infoDisplay.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        infoDisplay.setResizable(false);
        infoDisplay.pack();
        infoDisplay.setSize(225, 230);
        infoDisplay.setLocation(frame.getX() + frame.getWidth(), frame.getY());
        GraphicDisplay.setIconImage(infoDisplay);

        scorePanel = new JPanel();
        scorePanel.setLayout(null);
        scorePanel.setLocation(10, 17);
        scorePanel.setBackground(Color.black);

        startNumberOfUnits = maxNumberOfUnitsumberOfUnits;

        totalUnitsLabel = new JLabel("Total individuals: " + maxNumberOfUnitsumberOfUnits);
        totalUnitsLabel.setLocation(0, 25);
        totalUnitsLabel.setSize(225, 25);
        totalUnitsLabel.setHorizontalAlignment(0);
        totalUnitsLabel.setForeground(Color.white);
        scorePanel.add(totalUnitsLabel);

        unitsLabel = new JLabel("Live individuals: " + numberOfUnits);
        unitsLabel.setLocation(0, 50);
        unitsLabel.setSize(225, 25);
        unitsLabel.setHorizontalAlignment(0);
        unitsLabel.setForeground(Color.white);
        scorePanel.add(unitsLabel);

        sickUnitsLabel = new JLabel("Sick individuals: " + ImageComponents.sickUnits);
        sickUnitsLabel.setLocation(0, 75);
        sickUnitsLabel.setSize(225, 25);
        sickUnitsLabel.setHorizontalAlignment(0);
        sickUnitsLabel.setForeground(Color.white);
        scorePanel.add(sickUnitsLabel);

        deadUnitsLabel = new JLabel("Dead individuals: " + (startNumberOfUnits - numberOfUnits));
        deadUnitsLabel.setLocation(0, 100);
        deadUnitsLabel.setSize(225, 25);
        deadUnitsLabel.setHorizontalAlignment(0);
        deadUnitsLabel.setForeground(Color.white);
        scorePanel.add(deadUnitsLabel);

        vaccinationLabel = new JLabel("Vaccinated individuals: " + ImageComponents.vaccinatedUnits);
        vaccinationLabel.setLocation(0, 125);
        vaccinationLabel.setSize(225, 25);
        vaccinationLabel.setHorizontalAlignment(0);
        vaccinationLabel.setForeground(Color.white);
        scorePanel.add(vaccinationLabel);

        ticksLabel = new JLabel("Ticks: " + ImageComponents.ticks);
        ticksLabel.setLocation(0, 150);
        ticksLabel.setSize(225, 25);
        ticksLabel.setHorizontalAlignment(0);
        ticksLabel.setForeground(Color.white);
        scorePanel.add(ticksLabel);

        infoDisplay.setContentPane(scorePanel);
        infoDisplay.setVisible(true);
    }

    /**
     * Updating the information display.
     *
     * @param numberOfUnits the current number of units in the simulation.
     */
    public static void updateLabel(int numberOfUnits) {
        Component[] components = scorePanel.getComponents();
        for (Component component : components) {
            if (component instanceof JLabel) {
                if (component.equals(totalUnitsLabel)) {
                    totalUnitsLabel.setText("Total individuals: " + startNumberOfUnits);
                } else if (component.equals(unitsLabel)) {
                    unitsLabel.setText("Live individuals: " + numberOfUnits);
                } else if (component.equals(sickUnitsLabel)) {
                    sickUnitsLabel.setText("Sick individuals: " + ImageComponents.tempSickUnits);
                } else if (component.equals(deadUnitsLabel)) {
                    deadUnitsLabel.setText("Dead individuals: " + (startNumberOfUnits - numberOfUnits));
                } else if (component.equals(vaccinationLabel)) {
                    vaccinationLabel.setText("Vaccinated individuals: " + ImageComponents.tempVacciantedUnits);
                } else if (component.equals(ticksLabel)) {
                    ticksLabel.setText("Ticks: " + ImageComponents.ticks);
                }
            }
        }
    }
}
