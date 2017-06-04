for dir in *-tests; do
  for file in $dir/*-test.ss
  do
    scheme --script $file
  done
done

