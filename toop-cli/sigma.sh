#!/bin/bash                                                                                                    
input=""
if [ $# -gt 0 ]
then
  case "$1" in 
    -c|--compile)
        echo "Compiler option is not implemented yet"
        exit 0
        ;;
    -r|--repl)
        while [ "$input" != "exit" ]
        do
            echo -n "sigmac > " && read input
            if [ "$input" = "global.exit" ]
            then
                break
            elif [ "$input" = "config" ] # call core to get the error or add to the environment
            then 
                echo "Invalid"
            fi
        done
        exit 0
        ;;
    *)
        echo "No such argument is provided"
  esac
fi
