#!/bin/bash
fn=$1
on=$2
SKIP=100

if [[ ! -s "$fn" ]]; then
  echo -e "Error: specified file \"$fn\" does not exist."
  echo
  echo "Usage: ./makegifs.sh input.csv out.filename.base"
  echo
  exit 1
fi

if [[ -z "$on" ]]; then
  echo "Error: please specify an output filename base as second argument."
  echo "       It will prepend the output directory name."
  echo
  echo "Usage: ./makegifs.sh input.csv out.filename.base"
  echo
  exit 1
fi


# ID of the sequence must be the field below
FIELD=5
numct=$(tail -n+2 $fn | cut -d, -f3,4 | sort -u | wc -l)
numevent=$(tail -n+2 $fn | cut -d, -f3-5 | sort -u | wc -l)
echo "Detected $numevent events across $numct camera traps."
#numevent=$(tail -1 $fn | cut -f $FIELD -d,)
tail -n+2 $fn | cut -d, -f3,4 | sort -u > $on.sequences/unique.ct.txt;
for ct in $(cat $on.sequences/unique.ct.txt);
do
  ctnm=${ct/,/.}
  echo "Processing camera trap $ctnm ($ct)."
    mkdir -p $on.sequences/$ctnm
    eventct=$(grep -w "$ct" $fn | cut -d, -f $FIELD | sort -n | tail -1)
  for i in $(seq 0 $eventct); do
    numimage=$(grep -w "$ct" $fn | awk -F, '$NF=='$i | wc -l)
    if [[ -s "$on.sequences/$ctnm/sequence.$i.gif" ]]; then
      echo -ne "Event $(( i + 1 )) of $(( eventct + 1 )) for camera $ctnm with $numimage images exists. Skipping.         \r"
      continue;
    fi
    if [[ $numimage -gt $SKIP ]]; then
      echo -ne "Event $(( i + 1 )) of $(( eventct + 1 )) for camera $ctnm has $numimage images, greater than $SKIP. Downsampling.         \n"
      downs=$(echo $numimage / 100 | bc)
      if [[ ! -z "$(find tmp -type f)" ]]; then
        rm tmp/*;
      fi
      j=0;awk -F, '($'$FIELD'=='$i' && NR % '$downs' == 1){print $1}' <(grep -w "$ct" $fn)| while read ff; do
        cp "$ff" tmp/$j.jpg;
        j=$(( j + 1 ));
      done;
      ffmpeg -framerate 4 -i tmp/%d.jpg -loop -1 -vf scale=-1:650 \
      -vf "drawtext=fontfile=Lato-Regular.ttf:text='downsampled video, one in every "$downs" image shown':fontcolor=red:fontsize=60:box=1:boxcolor=black@0.5:boxborderw=5:x=(w-text_w)/2:y=(h-text_h)/10" \
      $on.sequences/$ctnm/sequence.$i.gif >/dev/null 2>&1;
      #exit
      continue;
    fi
    echo -ne "Processing event $(( i + 1 )) of $(( eventct + 1 )) for camera $ctnm with $numimage images.            \r"
    if [[ ! -z "$(find tmp -type f)" ]]; then
      rm tmp/*;
    fi
    j=0;awk -F, '$'$FIELD'=='$i'{print $1}' <(grep -w "$ct" $fn)| while read ff; do
      cp "$ff" tmp/$j.jpg;
      j=$(( j + 1 ));
    done;
    ffmpeg -framerate 2 -i tmp/%d.jpg -loop -1 -vf scale=-1:650 $on.sequences/$ctnm/sequence.$i.gif >/dev/null 2>&1;
  done
  echo
done
echo
echo Done.
