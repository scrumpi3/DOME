// JarClassLoader.java
// Copyright (c) 2003 Massachusetts Institute of Technology. All rights reserved.
package com.javaworld;

import mit.cadlab.dome3.gui.guiutils.customGui.classLoader.JarResources;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	Copyright (c) Non, Inc. 1999 -- All Rights Reserved

PACKAGE:	JavaWorld
FILE:		JarClassLoader.java

AUTHOR:		John D. Mitchell, Mar  3, 1999

REVISION HISTORY:
	Name	Date		Description
	----	----		-----------
	JDM	99.03.03   	Initial version.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


/**
 ** JarClassLoader provides a minimalistic ClassLoader which shows how to
 ** instantiate a class which resides in a .jar file.
 <br><br>
 **
 ** @author	John D. Mitchell, Non, Inc., Mar  3, 1999
 **
 ** @version 0.5
 **
 **/

public class JarClassLoader extends MultiClassLoader
{
	private JarResources jarResources;

	public JarClassLoader(String jarName)
	{
		// Create the JarResource and suck in the .jar file.
		jarResources = new JarResources(jarName);
	}

	protected byte[] loadClassBytes(String className)
	{
		// Support the MultiClassLoader's class name munging facility.
		className = formatClassName(className);

		// Attempt to get the class data from the JarResource.
		return (jarResources.getResource(className));
	}


	/*
	 * Internal Testing application.
	 */
	public static class Test
	{
		public static void main(String[] args) throws Exception
		{
			if (args.length != 2) {
				System.err.println
				        ("Usage: java JarClassLoader " +
				         "<jar file name> <class name>");
				System.exit(1);
			}

			/*
			 * Create the .jar class loader and use the first argument
			 * passed in from the command line as the .jar file to use.
			 */
			JarClassLoader jarLoader = new JarClassLoader(args[0]);

			/* Load the class from the .jar file and resolve it.	*/
			Class c = jarLoader.loadClass(args[1], true);

			/*
			 * Create an instance of the class.
			 *
			 * Note that created object's constructor-taking-no-arguments
			 * will be called as part of the objects creation.
			 */
			Object o = c.newInstance();

		}
	}	// End of nested Class Test.

}	// End of Class JarClassLoader.
