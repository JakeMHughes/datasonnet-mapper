#!/bin/bash

SCALA_FILE="./src/main/scala/com/datasonnet/DS.scala"
DOCS_ROOT="./docs/modules/ROOT/pages/"
MODULES="Core|DateTime|Strings|Objects|Numbers|Binaries|Arrays|Math|URL|Jsonpath|Crypto|Period"
FILE_REGEX=".*\/\*\* (${MODULES}) \*\/.*"

FOUND_DOC=false;
CURRENT_FILE=null;

while IFS= read -r line
do
  #If the file regex is found updte the current file variable
  # This variable reflects the section being operated on
  if [[ "$line" =~ $FILE_REGEX ]]; then
    name=$(echo "${BASH_REMATCH[1]}" | awk '{print tolower($0)}')
    CURRENT_FILE="${DOCS_ROOT}libraries-${name}.adoc"
    printf "## ${name}\n\n" > $CURRENT_FILE
    echo "Working on ${name}..."
  fi

  #If found doc is true, we are inside a documentation block
  if [[ $FOUND_DOC == true ]]; then
    #Check for end of comment block, therwise add to file
    if [[ "$line" =~ .*"*/".* ]]; then
      FOUND_DOC=false
      echo "" >> $CURRENT_FILE
    else
      updatedLine=$(echo "$line" | sed -E 's/\s+\*\s?//')
      echo "$updatedLine" >> $CURRENT_FILE
    fi
  # check for Documentation block if found doc is false
  elif [[ "$line" =~ .*"/** Documentation".* ]]; then
    FOUND_DOC=true
  fi
done < "$SCALA_FILE"
