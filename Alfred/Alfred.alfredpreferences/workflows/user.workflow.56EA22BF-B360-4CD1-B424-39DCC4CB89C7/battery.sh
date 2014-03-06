ioreg -l -n AppleSmartBattery -r > info.txt
CURRENT_CAPACITY=$(cat info.txt | grep CurrentCapacity | awk '{printf ("%i", $3)}')
MAX_CAPACITY=$(cat info.txt | grep MaxCapacity | awk '{printf ("%i", $3)}')
DESIGN_CAPACITY=$(cat info.txt | grep DesignCapacity | awk '{printf ("%i", $3)}')
TEMPERATURE=$(cat info.txt | grep Temperature | awk '{printf ("%.1f", $3/100.00)}')
MANUFACTURE_DATE=$(cat info.txt | grep ManufactureDate | awk '{printf ("%i", $3)}')
CHARGING=$(cat info.txt | grep -i ischarging | awk '{printf("%s",$3)}')


SERIAL=$(cat info.txt | grep BatterySerialNumber | awk '{printf ("%s", $3)}')
SERIAL="${SERIAL%\"}"
SERIAL="${SERIAL#\"}"

HEALTH=$(echo $MAX_CAPACITY $DESIGN_CAPACITY | awk '{printf ("%i", $1/$2 * 100)}')
CHARGE=$(echo $CURRENT_CAPACITY $MAX_CAPACITY | awk '{printf ("%i", $1/$2 * 100)}')
CELLS=$(python -c "f='●'*($CHARGE/10) + '○'*(10-$CHARGE/10);print f")

CYCLE_COUNT=$(cat info.txt | grep -e '"CycleCount" =' | awk '{printf ("%i", $3)}')

TIME_TO_EMPTY=$(cat info.txt | grep -i AvgTimeToEmpty | awk '{printf("%s", $3)}')
TIME_LEFT=Calculating…
if [ $TIME_TO_EMPTY -lt 15000 ]; then
TIME_LEFT=$(cat info.txt | grep -i AvgTimeToEmpty | awk '{printf("%i:%.2i", $3/60, $3%60)}')
fi

TIME_INFO=n
STATUS_INFO=Draining
BATT_ICON=icon.png


if [ $CHARGING == Yes ]; then
	TIME_FULL=$(cat info.txt | grep -i AvgTimeToFull | tr '\n' ' | ' | awk '{printf("%i:%.2i", $3/60, $3%60)}')
	TIME_INFO=$(echo $TIME_FULL until full)
	STATUS_INFO=Charging
	BATT_ICON=charging.png
else
	FULLY_CHARGED=$(cat info.txt | grep -i FullyCharged | awk '{printf("%s",$3)}')
	EXTERNAL=$(cat info.txt | grep -i ExternalConnected | awk '{printf("%s",$3)}')
	if [ $FULLY_CHARGED == Yes ]; then 
		if [ $EXTERNAL == Yes ]; then
			TIME_INFO=$(echo On AC power)
			STATUS_INFO=$(echo Fully Charged)
			BATT_ICON=power.png
		else
			TIME_INFO=$(echo $TIME_LEFT)
			BATT_ICON=full.png
		fi
	else
		TIME_INFO=$(echo $TIME_LEFT)
		BATT_ICON=critical.png
		if [ $CHARGE -gt 80 ]; then
			BATT_ICON=full.png
		elif [ $CHARGE -gt 50 ]; then
			BATT_ICON=medium.png
		elif [ $CHARGE -gt 10 ]; then
			BATT_ICON=low.png
		fi
	fi
fi

# Battery age
let "day=MANUFACTURE_DATE&31"
let "month=((MANUFACTURE_DATE>>5)&15)"
let "year=1980+(MANUFACTURE_DATE>>9)"
AGE=$(python -c "from datetime import date as D; d1=D.today(); d2=D($year, $month, $day); print ( (d1.year - d2.year)*12 + d1.month - d2.month )")

cat << EOB
<?xml version="1.0"?>
<items>
  <item>
    <title>$CHARGE% $CELLS</title>
	<subtitle>$STATUS_INFO</subtitle>
	<icon>$BATT_ICON</icon>
  </item>
  <item>
    <title>$TIME_INFO</title>
	<subtitle>Time Left</subtitle>
	<icon>clock.png</icon>
  </item>
  <item>
    <title>$TEMPERATURE° C</title>
	<subtitle>Temperature</subtitle>
	<icon>temp.png</icon>
  </item>
  <item>
    <title>$CYCLE_COUNT</title>
	<subtitle>Charge Cycles Completed</subtitle>
	<icon>cycles.png</icon>
  </item>
  <item>
    <title>$HEALTH%</title>
	<subtitle>Health</subtitle>
	<icon>health.png</icon>
  </item>
  <item>
    <title>$SERIAL</title>
	<subtitle>Serial</subtitle>
	<icon>serial.png</icon>
  </item>
  <item>
    <title>$AGE months</title>
	<subtitle>Age</subtitle>
	<icon>age.png</icon>
  </item>
</items>
EOB