package mit.cadlab.dome3.plugin.catalog.ui;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

class MouseListenerForCellSelection extends MouseAdapter {
    BaseCell cell;
    ComponentReference compRef;
    boolean isEnabled = true;
    
    
    MouseListenerForCellSelection(BaseCell cell) {
        this.cell = cell;
        this.compRef = cell.getComponentReference();
    }
    
    public void mousePressed(MouseEvent event) {
        if (isEnabled) {
            boolean isDoubleClicked = (event.getClickCount() > 1);
            if (isDoubleClicked) {
                System.out.println("add this cell to interface");
            }
            cell.cellClicked(event.isControlDown());
        }
    }
    
    public void mouseClicked(MouseEvent event) {
        if (MouseEvent.BUTTON1 == event.getButton()) {
            if (isEnabled) {
                if (! compRef.getRelationEditor().startDrag && ! cell.isSelected() && ! event.isControlDown()) {
                    cell.cellClicked(event.isControlDown());
                }
            }
        } else if (MouseEvent.BUTTON3 == event.getButton()) {
            compRef.getImplementationEditor().closeScriptEditor(true);
            compRef.getImplementationEditor().cellPopupMenu.show(event.getComponent(), event.getX(), event.getY());
        }
    }
    
    /** used when one wants to block this listener */
    public void setEnabled(boolean isEnabled) {
        this.isEnabled = isEnabled;
    }
}

