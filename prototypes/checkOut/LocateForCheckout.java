package checkOut;

import mit.cadlab.dome.gui.serverPanel.ServerPanel;
import mit.cadlab.dome.gui.serverPanel.ServerPanelSelectionListener;
import mit.cadlab.dome.gui.swing.msg.OneButton1Msg;
import mit.cadlab.dome.network.client.connection.ServerConnection;
import mit.cadlab.dome.swing.DialogFactory;
import mit.cadlab.dome.swing.Templates;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Created by IntelliJ IDEA.
 * User: wallace
 * Date: Mar 1, 2003
 * Time: 3:01:32 PM
 * To change this template use Options | File Templates.
 */

/**
 * Class used select models for adding to something
 */
public class LocateForCheckout extends JPanel
{
	public static final Dimension DEFAULT_SIZE = new Dimension(600, 400);
	public static final GridBagConstraints gbc = null;

	private ServerPanel serverPanel;
	private JButton selectButton;
	private JButton cancelButton;

	private static Object selectedObjectId= null;	

	/**
	 * method to show selection panel in a dialog
	 * @param svr valid server connection
	 * @param checkoutType checkoutType defined in Checkout class
	 */
	public static Object showDialog(Component parentComponent, ServerConnection svr, int checkoutType)
	{
		LocateForCheckout p = new LocateForCheckout(svr, checkoutType);
		p.setPreferredSize(DEFAULT_SIZE);
		String type;
		if (checkoutType==Checkout.CHECKOUT_MODEL)
			type = "model";
		else
			type = "playspace";
		JDialog dialog = DialogFactory.createDialog(parentComponent, "Locate "+type+" for checkout", p, true, true);
		dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		dialog.show();
		return selectedObjectId;
	}


	private static ServerPanelSelectionListener selectionListener = new ServerPanelSelectionListener()
	{
		public void selectionChanged(String path, Object id, ServerConnection svr)
		{
			selectedObjectId = id;
		}
	};

	/**
	 * Constructor for panel to select what to checkout
	 * @param svr server connection
	 * @param checkoutType defined in Checkout class
	 */
	private LocateForCheckout(ServerConnection svr, int checkoutType)
	{
		//setup the server panel for the type of checkout
		if (checkoutType == Checkout.CHECKOUT_MODEL) {
			serverPanel = new ServerPanel(svr,ServerPanel.RUN_BROWSE, ServerPanel.Models_Projects_Filter_Tree_Selection_Model);
			serverPanel.setFilterTo(ServerPanel.MODEL);
			serverPanel.enableFilterCombo(false);
		}
		else {
			serverPanel = new ServerPanel(svr, ServerPanel.RUN_BROWSE, ServerPanel.Playspaces_Filter_Tree_Selection_Model);
			serverPanel.setFilterTo(ServerPanel.PLAYSPACE);
			serverPanel.enableFilterCombo(false);
		}
		serverPanel.addSelectionListeners(selectionListener);

		JComponent[] comps = {makePanel(serverPanel)};

		// gridx, gridy, gridwidth, gridheight, weightx, weighty, anchor, fill, insets(t,l,b,r), ipadx, ipady
		GridBagConstraints[] gbcs = {
			new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0, gbc.CENTER, gbc.BOTH, new Insets(0, 0, 0, 0), 0, 0)
		};
		Templates.layoutGridBag(this, comps, gbcs);
	}

	private JPanel makePanel(ServerPanel serverPanel)
	{
		JPanel p = new JPanel();
		selectButton = Templates.makeButton("select", new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				if (selectedObjectId!=null)
					dispose();
				else
					OneButton1Msg.showWarning(null,"Checkout selection warning", "Nothing in the table is selected", "ok", new Dimension());
			}
		});
		cancelButton = Templates.makeButton("cancel", new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				selectedObjectId = null;
				dispose();
			}
		});

		JComponent[] comps = {serverPanel, selectButton, cancelButton};

		// gridx, gridy, gridwidth, gridheight, weightx, weighty, anchor, fill, insets(t,l,b,r), ipadx, ipady
		GridBagConstraints[] gbcs = {
			new GridBagConstraints(0, 0, 2, 1, 1.0, 1.0, gbc.CENTER, gbc.BOTH, new Insets(5, 5, 0, 5), 0, 0),
			new GridBagConstraints(0, 1, 1, 1, 1.0, 0.0, gbc.EAST, gbc.NONE, new Insets(0, 5, 5, 0), 0, 0),
			new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, gbc.EAST, gbc.NONE, new Insets(0, 5, 5, 5), 0, 0)
		};
		Templates.layoutGridBag(p, comps, gbcs);

		return p;
	}

	protected void dispose()
	{
		SwingUtilities.windowForComponent(this).dispose();
	}
}
