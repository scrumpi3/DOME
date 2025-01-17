/*
 *  Copyright (c) 2000, Jacob Smullyan.
 *
 *  This is part of SkunkDAV, a WebDAV client.  See http://skunkdav.sourceforge.net/ 
 *  for the latest version.
 * 
 *  SkunkDAV is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as published
 *  by the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 * 
 *  SkunkDAV is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with SkunkDAV; see the file COPYING.  If not, write to the Free
 *  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
 *  02111-1307, USA.
*/

package org.skunk.swing.text;

import java.awt.Component;
import java.awt.Dimension;
import javax.swing.plaf.ComponentUI;
import javax.swing.JTextPane;
import javax.swing.text.StyledDocument;

/**
 *  trivial subclass of JTextPane that supports turning off word wrap.
 *  It will be extended to support other functionality when syntax
 *  highlighting is added.
 */
public class TextEditorPane extends JTextPane
{
    private boolean wrap=false;

    /**
     * constructs a TextEditorPane with a default document.
     */
    public TextEditorPane()
    {
	super();
    }

    /**
     * constructs a TextEditorPane with the specified document.
     * @param doc the document
     */
    public TextEditorPane(StyledDocument doc)
    {
	super(doc);
    }

    /**
     * sets word wrap on or off.
     * @param wrap whether the text editor pane should wrap or not
     */
    public void setWordWrap(boolean wrap)
    {
	this.wrap=wrap;
    }

    /**
     * returns whether the editor wraps text.
     * @return the value of the word wrap property
     */
    public boolean getWordWrap()
    {
	return this.wrap;
    }

    public boolean getScrollableTracksViewportWidth()
    {
	if (!wrap)
	{
	    Component parent=this.getParent();
	    ComponentUI ui=this.getUI();
	    int uiWidth=ui.getPreferredSize(this).width;
	    int parentWidth=parent.getSize().width;
	    boolean bool= (parent !=null)
		? (ui.getPreferredSize(this).width < parent.getSize().width)
		: true;	

	    return bool;
	}
	else return super.getScrollableTracksViewportWidth();
    }

    public void setBounds(int x, int y, int width, int height) 
    {
	if (wrap) 
	    super.setBounds(x, y, width, height);
	else
	{
	    Dimension size = this.getPreferredSize();
	    super.setBounds(x,y,Math.max(size.width, width),Math.max(size.height, height));
	}
    }
}

/* $Log: TextEditorPane.java,v $
/* Revision 1.1.1.1  2003/05/05 16:12:32  renu
/* Reformatted and restructered source tree
/*
/* Revision 1.1.1.1  2002/09/18 19:52:17  caoq
/* initial import
/*
/* Revision 1.4  2001/01/04 06:02:49  smulloni
/* added more javadoc documentation.
/*
/* Revision 1.3  2000/12/27 22:05:09  smulloni
/* work on syntax highlighting.
/* */

