filename="plot.ppm"

echo "Enter a polynomial:"
read name
./Main "$name" $filename
xdg-open $filename 
