#! /bin/sh
# A simple bash script to find all the files with a given extension
# in a particular directory and move them to another directory 
# with a sequential naming scheme 

FILE_EXTENSION=".csv"
SEARCH_DIR="."
DESTINATION_PATH="."

# Create a list of all files in SEARCH_DIR with extension FILE_EXTENSION
FILES=$(find $SEARCH_DIR -type f -name "*$FILE_EXTENSION")
count=0

# For each of the files found copy it to DESTINATION_PATH with a new name
for f in $FILES;
do cp -fv "$f" "${DESTINATION_PATH}game${count}${FILE_EXTENSION}"
((count++))
done
