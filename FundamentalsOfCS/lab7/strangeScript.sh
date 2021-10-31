#! /bin/bash

folder="myscript"
anotherScript=$(realpath $1)
name="script_$(echo $anotherScript | sed -r "s!/!.!g")"
newFolder="$folder/$name.d"
errors="$folder/$name.error"
mkdir $folder 2> /dev/null
running=false
while true;
do
    if mkdir $newFolder 2> /dev/null
    then
        echo "Launch $name"
        (bash $1 2>$errors; rm -rf $newFolder)&
        echo "Sleep $2 min"
        sleep $(($2 * 1))
        running=false
    elif ! $running
    then
        echo "Wait!"
        running=true
    fi
done
