// DomeModelDefinitionBuildPanel.java
package mit.cadlab.dome3.gui.objectmodel.model.plugin;

import mit.cadlab.dome3.gui.menu.MenuManager;
import mit.cadlab.dome3.gui.mode.ModeContexts;
import mit.cadlab.dome3.gui.mode.build.BuildFocusTracker;
import mit.cadlab.dome3.gui.objectmodel.NameTextField;
import mit.cadlab.dome3.gui.objectmodel.tools.XmlViewPanel;
import mit.cadlab.dome3.gui.objectmodel.model.dome.StandardViewBuildPanel;
import mit.cadlab.dome3.gui.objectmodel.modelobject.context.BuildContextTree;
import mit.cadlab.dome3.gui.objectmodel.modelobject.context.build.ContextTreeBuilderPanel;
import mit.cadlab.dome3.objectmodel.modelobject.context.DefaultContextBuilder;
import mit.cadlab.dome3.gui.dsm.DSMPanel;
import mit.cadlab.dome3.objectmodel.model.dome.DomeModel;
import mit.cadlab.dome3.objectmodel.util.solving.DirectedGraph;
import mit.cadlab.dome3.objectmodel.modelobject.parameter.Parameter;
import mit.cadlab.dome3.plugin.PluginModelBuilder;
import com.sun.java.CardLayout2;
import mit.cadlab.dome3.swing.Templates;
import mit.cadlab.dome3.swing.tree.ObjectTree;
import mit.cadlab.dome3.swing.tree.TreeHistoryEvent;
import mit.cadlab.dome3.swing.tree.TreeHistoryListener;
import mit.cadlab.dome3.util.MatrixUtils;

import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.plaf.basic.BasicComboPopup;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Iterator;

import com.touchgraph.graphlayout.GLPanel;
import cern.colt.matrix.DoubleMatrix2D;
import cern.colt.matrix.linalg.Algebra;

public class PluginModelDefinitionBuildPanel extends JPanel
{

	protected static GridBagConstraints gbc;
	protected static ImageIcon comboArrow = Templates.makeImageIcon("mit/cadlab/dome3/icons/arrow/comboArrow.gif");
	protected static ImageIcon comboArrowOver = Templates.makeImageIcon("mit/cadlab/dome3/icons/arrow/comboArrowOver.gif");
	protected static Color notEditableColor = new Color(105, 105, 105);
	protected static String[] visualizations = {"list", "graph", "dsm", "xml"};

	protected PluginModelBuilder modelBuilder;
	protected CardLayout2 modelViewsCards;
	protected JPanel modelViewsPanel;
	protected ContextTreeBuilderPanel buildViewPanel;
	protected JButton backButton;
	protected NameTextField nameField;
	protected boolean isNameFieldEditable = true;
	protected JComboBox viewChoice;
	protected JComboBox viewComboBox;
	protected boolean isBackButtonEnabled = false;
	protected DefaultComboBoxModel cbModel;
	protected BasicComboPopup contextPopup;
	protected JButton rootContextsButton;
	protected int tfHeight = 0; // initialize textfield height


	protected GLPanel graph;
	protected DSMPanel dsm;

	public PluginModelDefinitionBuildPanel(PluginModelBuilder modelBuilder,
	                                       int noCols, String[] colNames, int[] colWidths)
	{
		this.modelBuilder = modelBuilder;
		setBackground(Templates.DARKER_BACKGROUND_COLOR);
		createComponents(noCols, colNames, colWidths);
	}

	protected void createComponents(int noCols, String[] colNames, int[] colWidths)
	{
		modelViewsCards = new CardLayout2();
		modelViewsPanel = new JPanel();
		modelViewsPanel.setLayout(modelViewsCards);
		DefaultContextBuilder conBuilder = (DefaultContextBuilder) modelBuilder.getBuildContext();
		buildViewPanel = new ContextTreeBuilderPanel(conBuilder, noCols, colNames, colWidths);
		buildViewPanel.setBackground(Templates.DARKER_BACKGROUND_COLOR);
		buildViewPanel.setUpDownBackgroundColour(Templates.DARKER_BACKGROUND_COLOR);

		modelViewsPanel.add(DomeModel.BUILD_VIEW, buildViewPanel);
		modelViewsPanel.add(DomeModel.OBJECT_TYPE_VIEW,
		                    new StandardViewBuildPanel(modelBuilder, modelBuilder.getView(DomeModel.OBJECT_TYPE_VIEW), colNames, colWidths));
		modelViewsPanel.add(DomeModel.CAUSAL_VIEW,
		                    new StandardViewBuildPanel(modelBuilder, modelBuilder.getView(DomeModel.CAUSAL_VIEW), colNames, colWidths));

		graph = new GLPanel();
		modelViewsPanel.add("graph", graph);
		//dsm = createDSM();
		//modelViewsPanel.add("dsm", dsm);
		modelViewsPanel.add("xml", new XmlViewPanel(modelBuilder));

		backButton = Templates.makeImageButton("mit/cadlab/dome3/icons/backArrow16.gif");
		backButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				buildViewPanel.getContextTree().setRootContextBack();
			}
		});
		backButton.setBackground(Templates.DARKER_BACKGROUND_COLOR);
		backButton.setEnabled(false);

		nameField = new NameTextField();
		nameField.setDisabledTextColor(notEditableColor);
		nameField.setDomeObject(buildViewPanel.getContextTree().getRootContext());
		nameField.setEditable(false);
		isNameFieldEditable = false;
		nameField.setBackground(Templates.DARKER_BACKGROUND_COLOR);

		viewChoice = Templates.makeComboBox(visualizations);
		viewChoice.setBackground(Templates.DARKER_BACKGROUND_COLOR);

		// associate backbutton and namefield with tree root and history
		BuildContextTree contextTree = buildViewPanel.getContextTree();
		contextTree.addHistoryListener(new TreeHistoryListener()
		{
			public void historyChanged(TreeHistoryEvent event)
			{
				switch (event.getType()) {
					case TreeHistoryEvent.EVENT_QUEUE_NONEMPTY:
						backButton.setEnabled(true);
						isBackButtonEnabled = true;
						nameField.setEditable(true);
						isNameFieldEditable = true;
						nameField.setForeground(Color.black);
						break;
					case TreeHistoryEvent.EVENT_QUEUE_EMPTY:
						backButton.setEnabled(false);
						isBackButtonEnabled = false;
						nameField.setEditable(false);
						isNameFieldEditable = false;
						nameField.setForeground(notEditableColor);
				}
			}
		});
		contextTree.addPropertyChangeListener(ObjectTree.ROOT_PROPERTY, new PropertyChangeListener()
		{
			public void propertyChange(PropertyChangeEvent e)
			{
				String property = e.getPropertyName();
				if (property.equals(ObjectTree.ROOT_PROPERTY)) {
					nameField.setDomeObject(buildViewPanel.getContextTree().getRootContext());
				}
			}
		});

		layoutComponent();
		configViewChoice();
	}

	protected void layoutComponent()
	{
		JComponent[] comps = {makeControlPanel(), modelViewsPanel};
		// gridx, gridy, gridwidth, gridheight, weightx, weighty, anchor, fill, insets(t,l,b,r), ipadx, ipady
		GridBagConstraints[] gbcs = {// 25 inset
			new GridBagConstraints(0, 0, 1, 1, 1.0, 0.0, gbc.NORTH, gbc.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0),
			new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0, gbc.NORTH, gbc.BOTH, new Insets(2, 0, 0, 0), 0, 0),
		};
		Templates.layoutGridBagB(this, comps, gbcs);
	}

	protected JPanel makeControlPanel()
	{
		JPanel p = new JPanel();
		p.setBackground(Templates.DARKER_BACKGROUND_COLOR);
		cbModel = new DefaultComboBoxModel(modelBuilder.getViewNames().toArray());

		viewComboBox = Templates.makeComboBox(cbModel);
		viewComboBox.addItemListener(new ItemListener()
		{
			public void itemStateChanged(ItemEvent evt)
			{
				switchView();
				contextPopup.hide();
			}
		});
		contextPopup = new BasicComboPopup(viewComboBox);
		rootContextsButton = Templates.makeImageButton(comboArrow, comboArrow, comboArrowOver, comboArrow);
		rootContextsButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent event)
			{
				if (tfHeight == 0) {
					Dimension tfSize = nameField.getSize();
					tfHeight = tfSize.height;
					Dimension popupSize = new Dimension(tfSize.width, tfHeight * cbModel.getSize());
					Templates.setFixedSize(contextPopup, popupSize);
				}
				contextPopup.show(nameField, 0, tfHeight);
			}
		});
		rootContextsButton.setBackground(Templates.DARKER_BACKGROUND_COLOR);

		// filler panel size of button
		JButton listButton = Templates.makeListArrowButton("up");
		JPanel fillerPanel = new JPanel();
		fillerPanel.setBackground(Templates.DARKER_BACKGROUND_COLOR);
		Templates.setFixedSize(fillerPanel, listButton.getPreferredSize());

		JComponent[] comps = {backButton,
		                      nameField,
		                      rootContextsButton,
		                      Templates.makeLabel("visualization"),
		                      viewChoice,
		                      fillerPanel
		};
		// gridx, gridy, gridwidth, gridheight, weightx, weighty, anchor, fill, insets(t,l,b,r), ipadx, ipady
		GridBagConstraints[] gbcs = {
			new GridBagConstraints(0, 0, 1, 1, 0.0, 1.0, gbc.WEST, gbc.NONE, new Insets(0, 0, 0, 5), 0, 0),
			new GridBagConstraints(1, 0, 1, 1, 1.0, 1.0, gbc.WEST, gbc.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0),
			new GridBagConstraints(2, 0, 1, 1, 0.0, 1.0, gbc.WEST, gbc.NONE, new Insets(0, 0, 0, 0), 0, 0),
			new GridBagConstraints(3, 0, 1, 1, 0.0, 1.0, gbc.EAST, gbc.NONE, new Insets(0, 15, 0, 0), 0, 0),
			new GridBagConstraints(4, 0, 1, 1, 0.0, 1.0, gbc.EAST, gbc.NONE, new Insets(0, 5, 0, 0), 0, 0),
			new GridBagConstraints(5, 0, 1, 1, 0.0, 0.0, gbc.EAST, gbc.NONE, new Insets(0, 1, 0, 0), 0, 0)
		};
		Templates.layoutGridBag(p, comps, gbcs);

		return p;
	}


	public void setMenuContext()
	{
		PluginBuildMenus.menus.setPluginDataTypes(modelBuilder.getPluginTypeName());
		MenuManager.setContext(getMenuContext());
		JComponent comp = (JComponent) modelViewsCards.getActiveComponent();
		BuildFocusTracker.notifyInFocus(comp, modelBuilder);
		if (comp.equals(buildViewPanel))
			buildViewPanel.setPluginModelEditMenusForSelection();
	}

	protected String getMenuContext()
	{
		String currentView = modelViewsCards.getActiveName();
		if (currentView.equals(DomeModel.BUILD_VIEW)) {
			return ModeContexts.BUILD_PLUGINMODEL_DEFINITION;
		} else if (currentView.equals(DomeModel.OBJECT_TYPE_VIEW) || currentView.equals(DomeModel.CAUSAL_VIEW)) {
			return ModeContexts.BUILD_PLUGIN_STANDARD_VIEW;
		} else {
			return ModeContexts.BUILD_PLUGINMODEL_OTHERVISUALIZATION_VIEW;
		}
	}

	protected void switchView()
	{
		String newView = cbModel.getSelectedItem().toString();
		modelViewsCards.show(modelViewsPanel, newView);
		setMenuContext(); // do this before the next line!
		synchronizeViewControls(); // needs correct menu showing
	}

	protected void synchronizeViewControls()
	{
		String currentView = modelViewsCards.getActiveName();
		if (currentView.equals(DomeModel.BUILD_VIEW)) {
			// reset backbutton and nameField properties
			//buildViewPanel.setEditMenusForSelection();
			nameField.setDomeObject(buildViewPanel.getContextTree().getRootContext());
			nameField.setEditable(isNameFieldEditable);
			backButton.setEnabled(isBackButtonEnabled);
		} else {
			StandardViewBuildPanel svPanel = (StandardViewBuildPanel) modelViewsCards.getActiveComponent();
			nameField.setText(currentView);
			nameField.setEditable(false);
			nameField.setCurrent();
			backButton.setEnabled(false);
		}
	}

	protected void configViewChoice()
	{
		viewChoice.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				if (!(viewChoice.getSelectedItem().toString().equals("list")))
					rootContextsButton.setEnabled(false);
				else {
					if (!rootContextsButton.isEnabled()) rootContextsButton.setEnabled(true);
				}
				showSelectedView();
			}
		});
	}

	protected void showSelectedView()
	{
		if (viewChoice.getSelectedItem().toString().equals("list"))
			switchView();
		else if (viewChoice.getSelectedItem().toString().equals("dsm")) {
			if (dsm == null) {
				dsm = createDSM();
				modelViewsPanel.add("dsm", dsm);
			}
			updateDSM();
			modelViewsCards.show(modelViewsPanel, "dsm");
		} else if (viewChoice.getSelectedItem().toString().equals("graph")) {
			graph.updateGraph(modelBuilder);
			modelViewsCards.show(modelViewsPanel, "graph");
		} else
			modelViewsCards.show(modelViewsPanel, viewChoice.getSelectedItem().toString());
		setMenuContext();
	}

	public Object[] createDSMData()
	{
		DirectedGraph dg = modelBuilder.createModelGraph();
		java.util.List nodes = dg.getNodes();
		ArrayList names = new ArrayList();
		for (Iterator i = nodes.iterator(); i.hasNext();) {
			Parameter p = (Parameter) i.next();
			names.add(p.getName());
		}
		if (dg.getAdjMatrix() != null) {
			/*int[][] reach = dg.getAdjMatrix();
			//make a copy
			int n = reach.length;
			int[][] reachcopy = new int[n][n];
			int i,j;
			for (i = 0; i < n; i++) {
				for (j = 0; j < n; j++) {
					reachcopy[i][j] = reach[j][i];
					if (j == i)
						reachcopy[i][j] = 1;
				}
			}     */
            DoubleMatrix2D reach = dg.getAdjMatrix();
            Algebra alg=new Algebra();
            DoubleMatrix2D reachcopy= alg.transpose(reach).copy();
            int n=reachcopy.rows();
            for(int i=0;i<n;i++){
                reachcopy.setQuick(i,i,1);
            }
			return new Object[]{reachcopy, (String[]) names.toArray(new String[]{})};
		}
		return null;
	}

	public DSMPanel createDSM()
	{
		Object[] data = createDSMData();
		if (data != null) {
		//	return new DSMPanel((int[][]) data[0], (String[]) data[1]);
            return new DSMPanel(MatrixUtils.toIntArray((DoubleMatrix2D) data[0]), (String[]) data[1]);
		} else
			return DSMPanel.createEmptyMatrixDSMPanel();
	}


	public void updateDSM()
	{
		Object[] data = createDSMData();
		if (data != null) {
			//dsm.setData((int[][]) data[0], (String[]) data[1]);
             dsm.setData(MatrixUtils.toIntArray((DoubleMatrix2D)data[0]), (String[]) data[1]);
		} else {
			int[][] nMatrix = new int[][]{};
			String[] headings = new String[]{};
			dsm.setData(nMatrix, headings);
		}

	}


}

