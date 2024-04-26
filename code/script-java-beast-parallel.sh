# initialize a semaphore with a given number of tokens
open_sem(){
    mkfifo pipe-$$
    exec 3<>pipe-$$
    rm pipe-$$
    local i=$1
    for((;i>0;i--)); do
        printf %s 000 >&3
    done
}

# run the given command asynchronously and pop/push tokens
run_with_lock(){
    local x
    # this read waits until there is something to read
    read -u 3 -n 3 x && ((0==x)) || exit $x
    (
     ( "$@"; )
    # push the return code of the command to the semaphore
    printf '%.3d' $? >&3
    )&
}

task() {
	i=$1
	beast -seed 1234 $i
	echo $i
}

N=4
open_sem $N

for i in *.xml; do
	[ -f "$i" ] || break
	run_with_lock task $i
done

echo "Waiting"
wait
echo "Done"
