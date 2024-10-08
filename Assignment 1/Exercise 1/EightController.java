package assignment;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import javax.swing.JLabel;
import java.util.HashMap;
import java.util.List;

public class EightController extends JLabel implements VetoableChangeListener, PropertyChangeListener {
    // Hole position
    private int currentHole;
    
    //Adjacency Map used to check if the next move is legal or not 
    private final HashMap<Integer, List<Integer>> adjacencyMap;

    // No-Arguments constructor for beans specification
    public EightController() {
        super("START");
        currentHole = 5;  // Inizialmente il buco è nella posizione 5
        adjacencyMap = new HashMap<>();
        initAdjacencyMap();
    }

    // Returns the position of the current hole
    public int getCurrentHole() {
        return currentHole;
    }

    // Method to initialize the adjacency map
    private void initAdjacencyMap() {
        adjacencyMap.put(1, List.of(2, 4));  
        adjacencyMap.put(2, List.of(1, 3, 5));  
        adjacencyMap.put(3, List.of(2, 6));  
        adjacencyMap.put(4, List.of(1, 5, 7)); 
        adjacencyMap.put(5, List.of(2, 4, 6, 8)); 
        adjacencyMap.put(6, List.of(3, 5, 9));  
        adjacencyMap.put(7, List.of(4, 8));  
        adjacencyMap.put(8, List.of(5, 7, 9)); 
        adjacencyMap.put(9, List.of(6, 8));  
    }

    // Method to listen to vetoable change events (in this case the one named "label")
    @Override
    public void vetoableChange(PropertyChangeEvent evt) throws PropertyVetoException {
        // Listening for "label" vetoable change events
        if ("label".equals(evt.getPropertyName())) {
            int newLabel = (int) evt.getNewValue();
            int clickedPosition = ((EightTile) evt.getSource()).getPosition();            
            
            // Case 1: selection of the current hole
            if(clickedPosition == currentHole){
                setText("KO");  
                throw new PropertyVetoException("Selected tile is the current hole", evt);  
            }

            System.out.println("Tile cliccato: " + clickedPosition + ", Nuovo label: " + newLabel);
            System.out.println("Posizione del buco: " + currentHole);

            // Case 2: tile is not adjacent to the current hole. Next move is vetoed
            if (!isAdjacent(clickedPosition, currentHole)) {
                System.out.println("Mossa non valida: il tile cliccato non è adiacente al buco.");
                setText("KO");  
                throw new PropertyVetoException("Mossa non valida", evt);  
            } else {
            // Case 3: tile is adjacent to the current hole. Next move is not vetoed 
                System.out.println("Mossa valida: il tile cliccato è adiacente al buco.");
                setText("OK");              
        }
    }
}

    // Method to update the current hole position in the controller
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if ("hole".equals(evt.getPropertyName())) {
            currentHole = ((EightTile) evt.getSource()).getPosition();
        }
    }

    // Method to check if two positions are adjacent or not
    private boolean isAdjacent(int pos1, int pos2) {
        return adjacencyMap.get(pos2).contains(pos1);
    }
}
