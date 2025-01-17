// FileSystemHandler.java
// Copyright (c) 2003 Massachusetts Institute of Technology. All rights reserved.
package mit.cadlab.dome3.network.server.handlers;

import mit.cadlab.dome3.network.server.db.DbConstants;
import mit.cadlab.dome3.network.server.db.DbErrors;
import mit.cadlab.dome3.network.server.functions.DeployFileSystemDbFunctions;
import mit.cadlab.dome3.network.server.functions.FileSystemDbFunctions;
import mit.cadlab.dome3.network.server.Debug;

import org.apache.xmlrpc.XmlRpcException;

import java.util.Vector;

/**
 * The XML-RPC handler for functions related to virtual file management.
 */
public class DeployFileSystemHandler extends FileSystemHandler
{

	/**
	 * This is the only method that an XMLRPC handler has to implement.
	 *
	 * @param methodName - name of the method on the sever that the client wants to invoke
	 * @param params - arguments to the method on the server
	 *
	 * @return results of the method execution on the server.
	 * @throws org.apache.xmlrpc.XmlRpcException wraps up any exceptions thrown by the method on the server or
	 * 					if a particular method is not found on the server.
	 */
	public Object execute(String methodName, Vector params) throws XmlRpcException
	{
		//Debug.trace(Debug.ALL, "DeployFileSystemHandler.execute: " + methodName);
		try {
			if (methodName.equals(DbConstants.GET_USER_SPACE))
				return getUserSpace(params);
			else if (methodName.equals(DbConstants.GET_USER_SPACE_FOR_SESSION))
				return getUserSpaceForSession(params);
			else if (methodName.equals(DbConstants.GET_USER_PLAYSPACE_FOR_SESSION))
				return getUserSpaceForSession(params);
			else if (methodName.equals(DbConstants.GET_GROUP_SPACE))
				return getGroupSpace(params);
			else if (methodName.equals(DbConstants.GET_SERVER_SPACE))
				return getServerSpace(params);
			else if (methodName.equals(DbConstants.GET_GROUP_SPACE_NO_MEMBERSHIP_CHECK))
				return getGroupSpaceNoMembershipCheck(params);
			else if (methodName.equals(DbConstants.GET_SERVER_SPACE_NO_MEMBERSHIP_CHECK))
				return getServerSpaceNoMembershipCheck(params);
			else if (methodName.equals(DbConstants.GET_USER_GROUP_SPACES_LIST))
				return getUserGroupSpacesList(params);
			else
				throw new XmlRpcException(DbErrors.XMLRPC_NO_SUCH_METHOD, methodName);
		} catch (XmlRpcException e) {
			e.printStackTrace();
			throw e;
		} catch (Exception e) {
			e.printStackTrace();
			throw new XmlRpcException(0, e.getMessage());
		}
	}


	private Vector getUserSpace(Vector params) throws XmlRpcException
	{

		if (params.size() != 3)
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS,
			                          DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS_MSG);
		if (!validateParameterTypes(params, new Class[]{PARAM_STR, PARAM_INT, PARAM_STR}))
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_ARGUMENT_LIST,
			                          "invalid arguments for getUserSpace(int userId, String sessionId, String type)");
		String sessionId = (String) params.get(0);
		int requestorId = FileSystemDbFunctions.getUserIdFromSession(sessionId).intValue();
		return FileSystemDbFunctions.getUserSpace(getInt(params.get(1)), requestorId, (String) params.get(2));
	}

	private Vector getUserSpaceForSession(Vector params) throws XmlRpcException
	{
		if (params.size() != 2)
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS,
			                          DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS_MSG);
		if (!validateParameterTypes(params, new Class[]{PARAM_STR, PARAM_STR}))
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_ARGUMENT_LIST,
			                          "invalid arguments for getUserSpaceForSession(String sessionId, String type)");
		String sessionId = (String) params.get(0);
		int requestorId = FileSystemDbFunctions.getUserIdFromSession(sessionId).intValue();
		return DeployFileSystemDbFunctions.getUserSpace(FileSystemDbFunctions.getUserTypeFromSession(sessionId), requestorId, requestorId, (String) params.get(1));
	}

	private Vector getGroupSpace(Vector params) throws XmlRpcException
	{
		//System.out.println(params);
		if (params.size() != 3)
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS,
			                          DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS_MSG);
		if (!validateParameterTypes(params, new Class[]{PARAM_STR, PARAM_INT, PARAM_STR}))
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_ARGUMENT_LIST,
			                          "invalid arguments for getGroupSpace(int groupId, String sessionId, String type)");
		String sessionId = (String) params.get(0);
		int requestorId = FileSystemDbFunctions.getUserIdFromSession(sessionId).intValue();
		return FileSystemDbFunctions.getGroupSpace(getInt(params.get(1)), requestorId, (String) params.get(2));
	}

	private Vector getGroupSpaceNoMembershipCheck(Vector params) throws XmlRpcException
	{
		//System.out.println(params);
		if (params.size() != 3)
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS,
			                          DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS_MSG);
		if (!validateParameterTypes(params, new Class[]{PARAM_STR, PARAM_INT, PARAM_STR}))
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_ARGUMENT_LIST,
			                          "invalid arguments for getGroupSpaceNoMembershipCheck(int groupId, String sessionId, String type)");
		String sessionId = (String) params.get(0);
		int requestorId = FileSystemDbFunctions.getUserIdFromSession(sessionId).intValue();
		return FileSystemDbFunctions.getGroupSpaceNoMembershipCheck(getInt(params.get(1)), requestorId, (String) params.get(2));
	}

	private Vector getServerSpace(Vector params) throws XmlRpcException
	{
		if (params.size() != 2)
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS,
			                          DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS_MSG);
		if (!validateParameterTypes(params, new Class[]{PARAM_STR, PARAM_STR}))
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_ARGUMENT_LIST,
			                          "invalid arguments for getServerSpace(String sessionId, String type)");
		String sessionId = (String) params.get(0);
		int requestorId = FileSystemDbFunctions.getUserIdFromSession(sessionId).intValue();
		return FileSystemDbFunctions.getServerSpace(requestorId, (String) params.get(1));
	}

	private Vector getServerSpaceNoMembershipCheck(Vector params) throws XmlRpcException
	{
		if (params.size() != 2)
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS,
			                          DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS_MSG);
		if (!validateParameterTypes(params, new Class[]{PARAM_STR, PARAM_STR}))
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_ARGUMENT_LIST,
			                          "invalid arguments for getServerSpaceNoMembershipCheck(String sessionId, String type)");
		String sessionId = (String) params.get(0);
		int requestorId = FileSystemDbFunctions.getUserIdFromSession(sessionId).intValue();
		return FileSystemDbFunctions.getServerSpaceNoMembershipCheck(requestorId, (String) params.get(1));
	}

	private Vector getUserGroupSpacesList(Vector params) throws XmlRpcException
	{
		if (params.size() != 3)
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS,
			                          DbErrors.XMLRPC_INVALID_NUMBER_OF_ARGUMENTS_MSG);
		if (!validateParameterTypes(params, new Class[]{PARAM_STR, PARAM_STR, PARAM_STR}))
			throw new XmlRpcException(DbErrors.XMLRPC_INVALID_ARGUMENT_LIST,
			                          "invalid arguments for getUserGroupSpacesList(String userOrGroup, String sessionId, String type)");
		return DeployFileSystemDbFunctions.getUserGroupSpacesList((String) params.get(0), FileSystemDbFunctions.getUserTypeFromSession((String) params.get(1)),
		                                                          FileSystemDbFunctions.getUserIdFromSession((String) params.get(1)), (String) params.get(2));
	}
}
