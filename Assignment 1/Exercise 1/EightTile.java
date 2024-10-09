/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Beans/Bean.java to edit this template
 */
package assignment;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.Serializable;
import javax.swing.JButton;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.beans.VetoableChangeSupport;
import javax.swing.Timer;

/**
 *
 * @author Francesco Kotopulos De Angelis
 */
public class EightTile extends JButton implements Serializable {    
    // Label of the tile
    private int label;
    
    // Position of the tile
    private final int position;
    
    
    private final PropertyChangeSupport pcs;
    private final VetoableChangeSupport vcs;

   // No-arguments constructor required from beans properties
   public EightTile() {
        this.pcs = new PropertyChangeSupport(this);
        this.vcs = new VetoableChangeSupport(this);
        this.position = 0;
    }
    
   // Constructor of the tile that takes as parameter the position and the initial label
   // Each tile has a PropertyChangeSupport and a VetoableChangeSupport to fire events
    public EightTile(Integer position, Integer initLabel)
    {
        super();
        this.pcs = new PropertyChangeSupport(this);
        this.vcs = new VetoableChangeSupport(this);
        this.label = initLabel;
        this.position = position;
        System.out.println("Label:" + this.label);
        System.out.println("Position: " + this.position);
        
        updateTile();
    }
    
    @Override
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        if(this.pcs != null){
            pcs.addPropertyChangeListener(listener);
        }
    }
        
    /**
     *
     * @param listener
     */
    @Override
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        pcs.removePropertyChangeListener(listener);
    }
    
    @Override
    public synchronized void addVetoableChangeListener(VetoableChangeListener listener) {
        this.vcs.addVetoableChangeListener(listener);
    }
    
    @Override
    public synchronized void removeVetoableChangeListener(VetoableChangeListener listener) {
        this.vcs.removeVetoableChangeListener(listener);
    }
    

    // Method called to make the tile flash with red color if the move is illegal
    private void flashTile(){        
        System.out.println("Cambio colore");
        setBackground(java.awt.Color.red);

        // Timer to go back to the previous color after 500 ms
        Timer timer = new Timer(500, new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateTile();
            }
        });
        timer.setRepeats(false);  
        timer.start();
        
    }
   
    // Method called to set the new label with and without veto
    public void setLabel(int newValue, boolean veto) {
        if (veto) {
            try {
                // Ask to the controller if next move is valid or not
                vcs.fireVetoableChange("label", this.label, newValue);
                
                // If next move is legal, update the tile
                int oldValue = this.label;
                this.label = newValue;
                updateTile();
                this.pcs.firePropertyChange("swap", newValue,oldValue);
                this.pcs.firePropertyChange("hole", newValue,oldValue);


            } catch (PropertyVetoException ex) {
                //Flash tile
                System.out.println("qui");
                flashTile();
            }
        }
        else{
            this.label = newValue;
            updateTile();
            System.out.println("Updating tile:" + getPosition() + " with new label:" + newValue);
        }
    }

    // Returns the label of the tile
    @Override
    public String getLabel() {
        return String.valueOf(this.label);
    }   
    
    // Returns the position of the tile
    public int getPosition()
    {
        return this.position;
    }
    
    // Updates the aspect (color and text label) of the tile
    private void updateTile() {
        System.out.println("Updating tile with label: " + label + " at position: " + position);

        if (label == 9) {
            setText("");
            setBackground(java.awt.Color.gray);
        } else {
            setText(String.valueOf(label));

            if (label == position) {
                setBackground(java.awt.Color.green);
            } else {
                setBackground(java.awt.Color.yellow);
            }
        }
    }


    
}
