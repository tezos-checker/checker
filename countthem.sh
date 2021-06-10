
make distclean

CONTENT=$(make test |& tee report.txt)


COMPLETE=$(cat report.txt | grep -o 'Complete' report.txt | wc -l)
CLOSE=$(cat report.txt | grep -o 'Close' report.txt | wc -l)
PARTIAL=$(cat report.txt | grep -o 'Partial' report.txt | wc -l)
UNWARRANTED=$(cat report.txt | grep -o 'Unwarranted' report.txt | wc -l)

echo "Complete    : $COMPLETE"
echo "Close       : $CLOSE"
echo "Partial     : $PARTIAL"
echo "Unwarranted : $UNWARRANTED"

