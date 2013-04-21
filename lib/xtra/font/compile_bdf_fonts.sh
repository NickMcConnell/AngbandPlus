#
# This script compiles all the BDF files in a directory. The resulting
# PCF files are compressed. It also creates a fonts.dir file.
#
# Diego Gonzalez - dieiggon@yahoo.com.ar
#

echo "Compiling BDF files in $PWD ..."

for BDF_FILE in *.bdf
do
	# Get the name of the compiled font
	PCF_FILE=`echo $BDF_FILE | sed -e 's/bdf$/pcf/'`
	
	echo "Creating $PCF_FILE.gz"
	
	# Compile the BDF file
	bdftopcf -o $PCF_FILE $BDF_FILE
	
	# Compress the PCF file
	gzip -9 -f $PCF_FILE
done

echo "Creating fonts.dir"

mkfontdir

echo "Done."
