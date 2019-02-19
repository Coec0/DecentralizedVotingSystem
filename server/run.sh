echo "Compiling the code.."
mvn clean install
clear
echo -e "Done!\n Running the server at port 8080"
cd target
java -jar Server-0.6-jar-with-dependencies.jar