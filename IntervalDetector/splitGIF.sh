#!/bin/bash
fn=$1
odir=$2
SKIP=100
echo $2


# ID of the sequence must be the field below
FIELD=2
temp_dir=$(mktemp -d)
echo $temp_dir
numevent=$(tail -n+2 $fn | cut -d, -f $FIELD | sort -u | wc -l)
#echo "Detected $numevent events across $numct camera traps."
#numevent=$(tail -1 $fn | cut -f $FIELD -d,)
#tail -n+2 $fn | cut -d, -f3,4 | sort -u > $on.sequences/unique.ct.txt;
ik=0
for i in $(tail -n+2 $fn | cut -d, -f $FIELD | sort -u); do
  numimage=$(awk -F, '$NF=='$i $fn | wc -l)
  ik=$(( ik + 1 ))
  if [[ $numimage -gt $SKIP ]]; then
    echo -ne "Event $(( ik + 1 )) of $numevent has $numimage images, greater than $SKIP. Downsampling.         \n"
    downs=$(echo $numimage / 100 | bc)
    j=0;awk -F, '($'$FIELD'=='$i' && NR % '$downs' == 1){print $1}' $fn | while read ff; do
      cp "$ff" $temp_dir/$j.jpg;
      j=$(( j + 1 ));
    done;
    ffmpeg -framerate 4 -i $temp_dir/%d.jpg -loop -1 -vf scale=-1:650 \
    -vf "drawtext=fontfile=Lato-Regular.ttf:text='downsampled video, one in every "$downs" image shown':fontcolor=red:fontsize=60:box=1:boxcolor=black@0.5:boxborderw=5:x=(w-text_w)/2:y=(h-text_h)/10" \
    "$odir/sequence.$i.gif";
    #exit
    continue;
  fi
  echo -ne "Processing event $(( ik + 1 )) of $numevent with $numimage images.            \r"
  j=0;awk -F, '$'$FIELD'=='$i'{print $1}' <(grep -w "$ct" $fn)| while read ff; do
    cp "$ff" $temp_dir/$j.jpg;
    j=$(( j + 1 ));
  done;
  ffmpeg -framerate 2 -i $temp_dir/%d.jpg -loop -1 -vf scale=-1:650 "$odir/sequence.$i.gif";
done
  echo
echo
echo Done.
