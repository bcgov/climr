#!/bin/bash

# Check if both input and output folders are provided
if [ "$#" -ne 4 ]; then
  echo "Usage: $0 <input_folder> <output_folder> <tbase> <tupper>"
  exit 1
fi

input_folder="$1"
output_folder="$2"

# Check if the input folder exists
if [ ! -d "$input_folder" ]; then
  echo "Input folder does not exist: $input_folder"
  exit 1
fi

# Check if the output folder exists, create it if not
if [ ! -d "$output_folder" ]; then
  mkdir -p "$output_folder"
fi

# Loop through all files in the input folder
for file in "$input_folder"/*; do
  # Check if the file is a regular file
  if [ -f "$file" ]; then
    # Extract the file name without the path and extension
    file_name=$(basename "$file")
    file_name_no_ext="${file_name%.*}"

    # Perform some operation on the file, for example, let's just copy it
    output_file="$output_folder/$file_name_no_ext"_clipped.nc
    cdo -expr,"tmax = ((tmax > $3)) ? tmax : $3; tmax = ((tmax < $4)) ? tmax : $4;" $file $output_file

    echo "Processed: $file -> $output_file"
  fi
done

echo "Finished Clipping Tmax"
