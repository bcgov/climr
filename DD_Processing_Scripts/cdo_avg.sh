#!/bin/bash

# Check if both input and output folders are provided
if [ "$#" -ne 3 ]; then
  echo "Usage: $0 <tminfolder> <tmaxfolder> <output_folder>"
  exit 1
fi

input_tmin="$1"
input_tmax="$2"
output_folder="$3"

# Check if the output folder exists, create it if not
if [ ! -d "$output_folder" ]; then
  mkdir -p "$output_folder"
fi

# Loop through all files in the input folder
for file in "$input_tmin"/*; do
  # Check if the file is a regular file
  if [ -f "$file" ]; then
    # Extract the file name without the path and extension
    file_name=$(basename "$file")
    file_name_no_ext="${file_name%.*}"
    tmax_file="${file//Tmin/Tmax}"
    # Perform some operation on the file, for example, let's just copy it
    output_file="$output_folder/$file_name_no_ext"_avg.nc
    echo "$file $tmax_file"
    cdo -ensmean [ $file $tmax_file ] $output_file

    echo "Processed: $file -> $output_file"
  fi
done

echo "Script completed."