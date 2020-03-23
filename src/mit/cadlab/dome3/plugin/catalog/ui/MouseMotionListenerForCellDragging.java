package mit.cadlab.dome3.plugin.catalog.ui;

import java.awt.Cursor;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.Point;

import mit.cadlab.dome3.plugin.catalog.core.CRemoteRelation;


class MouseMotionListenerForCellDragging extends MouseMotionAdapter {
    int dragInsetX;
    int dragInsetY;
    Point barOrigin;
    Point cellOrigin;
    int cellWidth;
    int cellHeight;
    ComponentReference compRef;
    BaseCell cell;
    boolean isEnabled = true;
    private long lastComputedTime = 0;
    
    MouseMotionListenerForCellDragging(BaseCell cell) {
        this.cell = cell;
        this.compRef = cell.getComponentReference();
    }
    
    public void setEnabled(boolean isEnabled) {
        this.isEnabled = isEnabled;
    }
    
    public boolean isEnabled() {
        return isEnabled;
    }
    
    public void mouseDragged(MouseEvent event) {
        if (! isEnabled) {
            return;
        }
        
        if (! compRef.getRelationEditor().startDrag) {
            compRef.getRelationEditor().startDrag = true;
            compRef.getRelationEditor().startDragPoint = new Point(event.getX(), event.getY());
            
            System.out.println("startDragPoint = " + compRef.getRelationEditor().startDragPoint);
            
            if (compRef.getImplementationEditor().isScriptEditorShown()) {
                compRef.getImplementationEditor().closeScriptEditor(true);
                System.out.println("relation param cell clicked. save changes and close editor");
            }
            
            dragInsetX = event.getX();
            dragInsetY = event.getY();
            
            compRef.getModelEditor().setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
            
            //compRef.clearBarAndCellSelection();
            BaseCell[] cells = compRef.getSelectedCells();
            
            int identicalBarIdx = 0;
            if (cells.length > 0) {
                identicalBarIdx = (cells [0].getBar() instanceof InterfaceBar) ? -1 : UIUtil.indexOfComponent(cells [0].getBar());
            } else {
                identicalBarIdx = Short.MIN_VALUE;
            }
            
            compRef.getRelationEditor().fixedCellList.clear();
            for (int i = 0; i < cells.length; i++) {
                cells [i].setBorder(UIUtil.CELL_DR_BORDER);
                if (cells [i].getBar().getBarType() == BaseBar.REL_BAR && compRef.getCurrentCNamingService().getRelation(cells [i].getBar().getRelAlias()) instanceof CRemoteRelation) {
                    compRef.getRelationEditor().fixedCellList.add(cells [i].getParamName());
                }
                
                int tempBarIdx = (cells [0].getBar() instanceof InterfaceBar) ? -1 : UIUtil.indexOfComponent(cells [0].getBar());
                if (tempBarIdx != identicalBarIdx) {
                    identicalBarIdx = Short.MIN_VALUE;
                }
            }
            
            compRef.getImplementationEditor().setDragSourceBarIndex(identicalBarIdx);
            
            if (cell.getBar() instanceof RelationBar) {
                int barIdx = UIUtil.indexOfComponent(cell.getBar());
                barOrigin = UIUtil.getRelationBarOrigin(barIdx, compRef);
            } else {
                barOrigin = UIUtil.getInterfaceBarOrigin();
            }
            
            int[] rowAndColumnIdx = cell.getRowAndColumnIndex();
            cellOrigin = UIUtil.getCellOrigin(rowAndColumnIdx [0], rowAndColumnIdx [1]);
            
            if (((BarSidePanel) cell.getParent()).getPanelType() == BarSidePanel.ITF_RIGHT || ((BarSidePanel) cell.getParent()).getPanelType() == BarSidePanel.REL_RIGHT) {
                cellOrigin.x = cellOrigin.x + cell.getBar().getLeftPanelSize().width + cell.getBar().getCenterPanelSize().width;
            }
            
            cellWidth = cell.getWidth();
            cellHeight = cell.getHeight();
            int ghostX = barOrigin.x + cellOrigin.x + event.getX() - dragInsetX;
            int ghostY = barOrigin.y + cellOrigin.y + event.getY() - dragInsetY;
            compRef.getRelationEditor().numberOfSelectedCells = cells.length;
            
            compRef.getImplementationEditor().showGhostCell(ghostX, ghostY, cellWidth, cellHeight);
            //compRef.getImplementationEditor().updateDragInfoLabel(ghostX, ghostY, event.getX(), event.getY(), event.isControlDown() || isLeftClick);
        }
        
        if (compRef.getRelationEditor().startDrag) {
            int ghostX = barOrigin.x + cellOrigin.x + event.getX() - dragInsetX;
            int ghostY = barOrigin.y + cellOrigin.y + event.getY() - dragInsetY;
            compRef.getImplementationEditor().moveGhostCell(ghostX, ghostY, event.isControlDown());
            //compRef.getImplementationEditor().updateDragInfoLabel(ghostX, ghostY, event.getX(), event.getY(), event.isControlDown() || isLeftClick);
            int currentX = ghostX + cellWidth / 2;
            int currentY = ghostY + cellHeight / 2;
            //            int currentX = barOrigin.x + cellOrigin.x + event.getX();
            //            int currentY = barOrigin.y + cellOrigin.y + event.getY();
            
            compRef.implEditor.showTestMarker(currentX, currentY);
            
            int barIdx = UIUtil.findBarIndexOnWhichMouseIsOver(currentX, currentY, compRef);
            
            if (barIdx == -2) {
                compRef.implEditor.hideInsertionMarker();
                compRef.getRelationEditor().cellInsertionIdx = -1;
                return; // mouse is at an ignorable position
            }
            
            compRef.getImplementationEditor().setDragTargetBarIndex(barIdx);
            
            long currentTime = System.currentTimeMillis();
            long gap = currentTime - lastComputedTime;
            if (gap > 150) {
                lastComputedTime = currentTime;
                //                int currentBarIdx = -1;
                //                if (cell.getBar().barType == BaseBar.ITF_BAR) {
                //                    currentBarIdx = -1;
                //                } else {
                //                    currentBarIdx = UIUtil.indexOfComponent(cell.getBar());
                //                }
                int currentBarIdx = barIdx;
                //boolean isLeftPanel = (cell.cellType == BaseCell.ITF_INPUT) || (cell.cellType == BaseCell.REL_INPUT);
                int side = UIUtil.findWhichSideMouseIsOver(currentX, currentBarIdx, compRef);
                if (side == 0) {
                    compRef.getRelationEditor().cellInsertionIdx = -1;
                    return;
                }
                
                boolean isLeftPanel = (side == -1);
                int[] insertionYAndInsertionIdx = UIUtil.findTheClosestCellInsertionPoint(currentBarIdx, isLeftPanel, currentX, currentY, compRef);
                int insertionX = insertionYAndInsertionIdx [0];
                int insertionY = insertionYAndInsertionIdx [1];
                compRef.getRelationEditor().cellInsertionIdx = insertionYAndInsertionIdx [2]; // set the current insertion idx
                compRef.getRelationEditor().insertionIntoLeftPanel = isLeftPanel;
                if (insertionYAndInsertionIdx [2] != -1) {
                    compRef.getImplementationEditor().showCellInsertionMarker(insertionX, insertionY, cellHeight);
                }
            }
        }
    }
}


