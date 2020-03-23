FROM tomcat:9.0.10-jre10

ENV ActiveMQdns 192.168.1.2
#ENV dome_server_user ceed
#ENV dome_server_pw ceed

COPY  dist/war/DOMEApiServicesV7.war /usr/local/tomcat/webapps/.

# need to make 2 environment variables read in DOME code
#echo "dome.server.user=$dome_server_user" >> config.properties
#echo "dome.server.pw=$dome_server_pw" >> config.properties

CMD ["catalina.sh", "run"]

