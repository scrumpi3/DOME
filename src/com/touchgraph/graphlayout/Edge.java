/*
 * TouchGraph LLC. Apache-Style Software License
 *
 *
 * Copyright (c) 2002 Alexander Shapiro. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:  
 *       "This product includes software developed by 
 *        TouchGraph LLC (http://www.touchgraph.com/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "TouchGraph" or "TouchGraph LLC" must not be used to endorse 
 *    or promote products derived from this software without prior written 
 *    permission.  For written permission, please contact 
 *    alex@touchgraph.com
 *
 * 5. Products derived from this software may not be called "TouchGraph",
 *    nor may "TouchGraph" appear in their name, without prior written
 *    permission of alex@touchgraph.com.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL TOUCHGRAPH OR ITS CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE 
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 */

package com.touchgraph.graphlayout;

import java.awt.*;
import java.awt.geom.Line2D;

/**  Edge.
  *
  * @author   Alexander Shapiro
  * @version  1.20
  */
public class Edge {

    public static Color DEFAULT_COLOR = Color.decode("#AAAAAA");
    public static Color MOUSE_OVER_COLOR = Color.decode("#AAAAAA");
    public static int DEFAULT_LENGTH = 100;

	final static float dash1[] = {2.0f};
	final static BasicStroke dotted = new BasicStroke(1.0f,
	                                                  BasicStroke.CAP_BUTT,
	                                                  BasicStroke.JOIN_MITER,
	                                                  2.0f, dash1, 0.0f);

    public Node from; //Should be private, changing from effects "from" Node
    public Node to;   //Should be private, changing from effects "to" Node
    protected Color col;
    protected int length;
    protected boolean visible;

	protected boolean isConnectedEdge = true;

  // ............

	public static Edge createDisconnectedEdge(Node f, Node t) {
		Edge disconnectedEdge = new Edge(f,t);
		disconnectedEdge.isConnectedEdge = false;
		return disconnectedEdge;
	}

    /** Constructor with two Nodes and a length.
      */
    public Edge(Node f, Node t, int len) {
        from = f;
        to = t;
          length = len;
          col = DEFAULT_COLOR;
    }

    /** Constructor with two Nodes, which uses a default length.
      */
    public Edge(Node f, Node t) {
        this(f, t, DEFAULT_LENGTH);
    }

   // setters and getters ...............
   
    public static void setEdgeDefaultColor( Color color ) { DEFAULT_COLOR = color; }
    public static void setEdgeMouseOverColor( Color color ) { MOUSE_OVER_COLOR = color; }
    public static void setEdgeDefaultLength( int length ) { DEFAULT_LENGTH = length; }

   /** Returns the starting "from" node of this edge as Node. */
    public Node getFrom() { return from; }
    
   /** Returns the terminating "to" node of this edge as Node. */
    public Node getTo() { return to; }
    
   /** Returns the color of this edge as Color. */
    public Color getColor() {
        return col;
    }

   /** Set the color of this Edge to the Color <tt>color</tt>. */
    public void setColor( Color color ) {
        col = color;
    }

   /** Returns the length of this Edge as a double. */
    public int getLength() {
        return length;
    }

   /** Set the length of this Edge to the int <tt>len</tt>. */
    public void setLength(int len) {
        length=len;
    }

   /** Set the visibility of this Edge to the boolean <tt>v</tt>. */
    public void setVisible( boolean v) {
        visible = v;
    } 

   /** Return the visibility of this Edge as a boolean. */
    public boolean isVisible() {
        return visible;
    } 

    public Node getOtherEndpt(Node n) { //yields false results if Node n is not an endpoint
        if (to != n) return to;
        else return from;
    }

    /** Switches the endpoints of the edge */
    public void reverse() {
        Node temp = to;
        to = from;
        from = temp;
    }

    public boolean intersects(Dimension d) {
        int x1 = (int) from.drawx;
        int y1 = (int) from.drawy;
        int x2 = (int) to.drawx;
        int y2 = (int) to.drawy;

        return (((x1>0 || x2>0) && (x1<d.width  || x2<d.width)) &&
                  ((y1>0 || y2>0) && (y1<d.height || y2<d.height) ));

    }

    public double distFromPoint(double px, double py) {
        double x1= from.drawx;
        double y1= from.drawy;
        double x2= to.drawx;
        double y2= to.drawy;

        if (px<Math.min(x1, x2)-8 || px>Math.max(x1, x2)+8 ||
            py<Math.min(y1, y2)-8 || py>Math.max(y1, y2)+8)
            return 1000;

        double dist = 1000;
        if (x1-x2!=0) dist = Math.abs((y2-y1)/(x2-x1)*(px - x1) + (y1 - py));
        if (y1-y2!=0) dist = Math.min(dist, Math.abs((x2-x1)/(y2-y1)*(py - y1) + (x1 - px)));

        return dist;
    }

    public boolean containsPoint(double px, double py) {
        return distFromPoint(px,py)<10;
    }

	public static void paintArrow(Graphics g, int x1, int y1, int x2, int y2, Color c)
	{
		paintArrow(g, x1, y1, x2, y2, c, true);
	}

    private static void paintArrow(Graphics g, int x1, int y1, int x2, int y2, Color c, boolean isConnectedEdge)
    {
	    //Forget hyperbolic bending for now
	    g.setColor(c);
	    if (isConnectedEdge) {
		    double dist = Math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
		    double dx = (x1 - x2) / dist;
		    double dy = (y1 - y2) / dist;
		    g.drawLine(x1, y1, x2, y2);

		    // draw the arrow
		    int l = 15;
		    x1 = (x1 + x2) / 2 + (int) (dx / 2 * l);
		    y1 = (y1 + y2) / 2 + (int) (dy / 2 * l);
		    x2 = x1 - (int) (dx * l);
		    y2 = y1 - (int) (dy * l);

		    int x3 = x1 + (int) (dy * 4);
		    int y3 = y1 - (int) (dx * 4);
		    int x4 = x1 - (int) (dy * 4);
		    int y4 = y1 + (int) (dx * 4);
		    int[] x = {x2, x3, x4};
		    int[] y = {y2, y3, y4};

		    g.fillPolygon(x, y, 3);
	    }
	    else { // draw a dotted line between the two
		    Graphics2D g2 = (Graphics2D)g;
		    g2.setStroke(dotted);
		    g2.draw(new Line2D.Double(x1, y1, x2, y2));
	    }
    }

    public void paint(Graphics g, TGPanel tgPanel) {
        Color c = (tgPanel.getMouseOverE()==this) ? MOUSE_OVER_COLOR : col;

        int x1=(int) from.drawx;
        int y1=(int) from.drawy;
        int x2=(int) to.drawx;
        int y2=(int) to.drawy;
        if (intersects(tgPanel.getSize())) paintArrow(g, x1, y1, x2, y2, c, isConnectedEdge);
    }

} // end com.touchgraph.graphlayout.Edge
