SET YOUR_DOMEROOT=C:\dome3
rem SET JAVA_HOME=C:\j2sdk1.4.2_13
SET JAVA_HOME=C:\jdk1.5.0_07
cd %YOUR_DOMEROOT%\scripts\db
%JAVA_HOME%\bin\java -DDOMEROOT=%YOUR_DOMEROOT% -Xmx250M -cp %YOUR_DOMEROOT%\out;%YOUR_DOMEROOT%\lib\axis.jar;%YOUR_DOMEROOT%\lib\colt.jar;%YOUR_DOMEROOT%\lib\dom4j-full.jar;%YOUR_DOMEROOT%\lib\domehelp.jar;%YOUR_DOMEROOT%\lib\hsqldb.jar;%YOUR_DOMEROOT%\lib\jakarta-oro-2.0.6.jar;%YOUR_DOMEROOT%\lib\Jama-1.0.1.jar;%YOUR_DOMEROOT%\lib\jcommon-0.8.9.jar;%YOUR_DOMEROOT%\lib\jfreechart-0.9.14.jar;%YOUR_DOMEROOT%\lib\jh.jar;%YOUR_DOMEROOT%\lib\jython.jar;%YOUR_DOMEROOT%\lib\openideas13.jar;%YOUR_DOMEROOT%\lib\orbix2000.jar;%YOUR_DOMEROOT%\lib\ostermillerutils_1_02_24.jar;%YOUR_DOMEROOT%\lib\vecmath.jar;%YOUR_DOMEROOT%\lib\groovy-all-1.0-jsr-05.jar; mit.cadlab.dome3.plugin.catalog.ui.ModelEditorFrame -debug:50
