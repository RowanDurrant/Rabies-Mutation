for i in *.fasta; do
	[ -f "$i" ] || break
	beastgen -date_order 2 -date_prefix "_" beast_example.template $i $i.xml
   	echo "$i"
done