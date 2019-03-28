echo "Checking for necessary programs ..."

if ! [ -x "$(command -v mvn)" ]; then
  echo 'Error: maven is not installed.' >&2
  exit 1
fi

if ! [ -x "$(command -v java)" ]; then
  echo 'Error: java is not installed.' >&2
  exit 1
fi

echo "Compiling the code..."
mvn clean install > /dev/null
echo -e "Done!\nTrying to start the server"
cd target
java -jar Server-0.6-jar-with-dependencies.jar $1 $2 $3