package mit.cadlab.dome3.plugin.catalog.ui;

import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;


class MouseListenerForCellDragging extends MouseAdapter {
    ComponentReference compRef;
    BaseCell cell;
    boolean isEnabled = true;
    
    MouseListenerForCellDragging(BaseCell cell) {
        this.cell = cell;
        this.compRef = cell.getComponentReference();
    }
    
    public void setEnabled(boolean isEnabled) {
        this.isEnabled = isEnabled;
    }
    
    public boolean isEnabled() {
        return isEnabled;
    }
    
    public void mouseReleased(MouseEvent event) {
        if (! isEnabled) {
            return;
        }
        
        if (compRef.getRelationEditor().startDrag) {
            compRef.getRelationEditor().startDrag = false;
            
            /* ignore accidental drag */
            if (compRef.getRelationEditor().startDragPoint.distance(event.getX(), event.getY()) < RelationEditor.ACCIDENTAL_MOVE_DISTANCE) {
                ignoreAccidentalDrag();
                return; // make it just like the current dragging motion didn't exist.
            }
            
            
            compRef.getImplementationEditor().hideGhostBar();
            compRef.getModelEditor().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            cell.setSelected(true);
            
            if (event.getButton() == MouseEvent.BUTTON1) {
                if (! event.isControlDown()) { // plain left button drag = prompted-for-what-to-do mode
                    tryJustMove(event);
                } else { // ctrl-down left button drag = no-prompt-just-copy mode
                    justCopy();
                }
            } else if (event.getButton() == MouseEvent.BUTTON3) {
                showCopyOptionPopupMenu(event);
            }
        }
    }
    
    private void justCopy() {
        int targetBarIdx = compRef.getImplementationEditor().getDragTargetBarIndex();
        BaseCell[] cells = compRef.getSelectedCells();
        if (compRef.getRelationEditor().cellInsertionIdx != -1) {
            compRef.getImplementationEditor().moveSelectedCells(cells, targetBarIdx, true, true);
        } else {
            for (int i = 0; i < cells.length; i++) {
                cells [i].setBorder(UIUtil.CELL_HL_BORDER);
            }
        }
        compRef.getImplementationEditor().hideInsertionMarker();
    }
    
    /** just move cell if target bar and source bar are the same. otherwise, show popup menu. */
    private void tryJustMove(MouseEvent event) {
        int targetBarIdx = compRef.getImplementationEditor().getDragTargetBarIndex();
        int sourceBarIdx = compRef.getImplementationEditor().getDragSourceBarIndex();
        BaseCell[] cells = compRef.getSelectedCells();
        if (compRef.getRelationEditor().cellInsertionIdx != -1) {
            if (targetBarIdx == sourceBarIdx) {
                compRef.getImplementationEditor().moveSelectedCells(cells, targetBarIdx, false, false);
                compRef.getImplementationEditor().hideInsertionMarker();
            } else {
                showCopyOptionPopupMenu(event);
            }
            
        } else {
            for (int i = 0; i < cells.length; i++) {
                cells [i].setBorder(UIUtil.CELL_HL_BORDER);
            }
        }
    }
    
    private void showCopyOptionPopupMenu(MouseEvent e) {
        compRef.getImplementationEditor().dragPopupMenu1.show(e.getComponent(), e.getX(), e.getY());
    }
    
    private void ignoreAccidentalDrag() {
        compRef.getImplementationEditor().hideGhostBar();
        compRef.getImplementationEditor().hideInsertionMarker();
        compRef.getModelEditor().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        
        BaseCell[] cells = compRef.getSelectedCells();
        for (int i = 0; i < cells.length; i++) {
            cells [i].setSelected(true);
        }
        //speedup Clog.debug("consider the last mouse move as a accidental move. keep the current selection");
    }
    
    
    //    private static String getUniqueName(String originParamName, BarSidePanel panel) {
    //        int compCount = panel.getComponentCount();
    //        boolean isUnique = false;
    //        int idx = 0;
    //        String tryThisName = originParamName;
    //        while (! isUnique) {
    //            isUnique = true;
    //            for (int i = 0; i < compCount; i++) {
    //                if (tryThisName.equals(((BaseCell) panel.getComponent(i)).getParamName())) {
    //                    tryThisName = originParamName + "_" + (++idx);
    //                    isUnique = false;
    //                    break;
    //                }
    //            }
    //
    //        }
    //        return tryThisName;
    //    }
}
