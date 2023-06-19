for i in *.xml; do
	[ -f "$i" ] || break
	beast -seed 1234 $i
   	echo "$i"
done