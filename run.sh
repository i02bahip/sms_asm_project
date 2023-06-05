clear
if [ -z "$1" ]
then
    echo "Wrong param: %1"
    echo "The correct format is: run param"
    echo "Possible params:"
    echo "----------------------------"
    echo "0: complete example"
    echo "1: hello world"
    echo "----------------------------"
    echo "Example for run example 0:"
    echo "./run.sh 0"
else
      java -jar environment/emulators/emulicious/Emulicious.jar out/example-$1.sms
fi