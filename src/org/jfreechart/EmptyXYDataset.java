/* ======================================
 * JFreeChart : a free Java chart library
 * ======================================
 *
 * Project Info:  http://www.jfree.org/jfreechart/index.html
 * Project Lead:  David Gilbert (david.gilbert@object-refinery.com);
 *
 * (C) Copyright 2000-2003, by Object Refinery Limited and Contributors.
 *
 * This library is free software; you can redistribute it and/or modify it under the terms
 * of the GNU Lesser General Public License as published by the Free Software Foundation;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this
 * library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -------------------
 * EmptyXYDataset.java
 * -------------------
 * (C) Copyright 2001-2003, by Object Refinery Limited.
 *
 * Original Author:  David Gilbert (for Object Refinery Limited).
 * Contributor(s):   -;
 *
 * $Id: EmptyXYDataset.java,v 1.1 2005/04/20 16:02:04 sittha Exp $
 *
 * Changes
 * -------
 * 22-Nov-2001 : Version 1 (DG);
 * 10-Oct-2002 : Fixed errors reported by Checkstyle (DG);
 *
 */

package org.jfreechart;

import org.jfree.data.AbstractSeriesDataset;
import org.jfree.data.XYDataset;

/**
 * An empty dataset for testing purposes.
 *
 * @author David Gilbert
 */
public class EmptyXYDataset extends AbstractSeriesDataset implements XYDataset {

    /**
     * Default constructor.
     */
    public EmptyXYDataset() {
    }

    /**
     * Returns the x-value for the specified series and item.
     *
     * @param series  the series (zero-based index).
     * @param item  the item (zero-based index).
     *
     * @return  the x-value (always null for this class).
     */
    public Number getXValue(int series, int item) {
        return null;
    }

    /**
     * Returns the y-value for the specified series and item.
     *
     * @param series  the series (zero-based index).
     * @param item  the item (zero-based index).
     *
     * @return  the y-value (always null for this class).
     */
    public Number getYValue(int series, int item) {
        return null;
    }

    /**
     * Returns the number of series in the dataset.
     *
     * @return the series count (always zero for this class).
     */
    public int getSeriesCount() {
        return 0;
    }

    /**
     * Returns the name of the series.
     *
     * @param series  the series (zero-based index).
     *
     * @return the name of the series (always null in this class).
     */
    public String getSeriesName(int series) {
        return null;
    }

    /**
     * Returns the number of items in the specified series.
     *
     * @param series  the series (zero-based index).
     *
     * @return the item count (always zero in this class).
     */
    public int getItemCount(int series) {
        return 0;
    }

}
