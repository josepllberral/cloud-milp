#!/bin/bash
FILE=$1

LINE1="- - - - - - - - - -" #DBs
LINE3="- - - - - - - - - -"	#VMs

AUX=$(grep "Finished" $FILE | grep "CTRL VM-C")
if [ $? -eq 0 ]; then
	LINE1=`grep "Finished" $FILE | grep "CTRL VM-C" | awk '{sum+=($6-$7); if(($6-$7)>max)max=($6-$7); if (($6-$7)>0)count+=1 } END { if(count==0) { count=1; sum=3600; } print "Max_Time_Solve_DBs:",max,"Average_Time_Solve_DBs:",sum/count}'` ;
	LINE2=`grep "REPO" $FILE | grep "VM-C" | awk '{simg+=$9; sup+=$11; sdeg+=$13; sdup+=$15; srem+=$17  } END {print "Images:",simg,"ImagesUp:",sup,"ImagesDeg:",sdeg,"ImagesDegUp:",sdup,"Avg_Images:",simg/NR,"Avg_Images_Up:",sup/NR,"Avg_Degr:",sdeg/NR,"Avg_Degraded_Up:"sdup/NR,"Avg_Degr_Remaining:",srem/NR}'` ;
fi

AUX=$(grep "Finished" $FILE | grep "CTRL VM-S")
if [ $? -eq 0 ]; then
	LINE3=`grep "Finished" $FILE | grep "CTRL VM-S" | awk '{sum+=($6-$7); if(($6-$7)>max)max=($6-$7); if (($6-$7)>0)count+=1 } END { if(count==0) { count=1; sum=3600; } print "Max_Time_Solve_VMs:",max,"Average_Time_Solve_VMs: ",sum/count}'` ;
	LINE4=`grep "REPO" $FILE | grep "VM-S" | awk '{svm+=$9; sup+=$11;} END {print "VMs:",svm,"VMs_Up:",sup,"Avg_VMs:",svm/NR,"Avg_VMs_Moved:",sup/NR}'` ;
fi

LINE5=`grep "Info Schd" $FILE | awk '{ sneg+=$8; } END {print "Total_Negotiations: ",sneg,"Avg_Negotiations:",sneg/NR}'`
LINE6=""

AUX=$(grep "Info Ener" $FILE)
if [ $? -eq 0 ]; then
	LINE6=`grep "Info Ener" $FILE | awk '{ sener+=$3; dc1+=$6; dc2+=$8; dc3+=$10; dc4+=$12; } END { print "Total_Energy:",sener,"kWh Avg_Energy:",sener/NR,"kWh DC1:",dc1," DC2:",dc2," DC3:",dc3," DC4:",dc4,"(kWh)"}'`
fi

AUX=$(grep "Info Iter" $FILE)
if [ $? -eq 0 ]; then
	LINE7=`grep "Info Iter" $FILE | awk '{ if($8>$10){dc1+=$8-$10;} if($16>$18){dc2+=$16-$18;} if($24>$26){dc3+=$24-$26;} if($32>$34){dc4+=$32-$34;} } END { print "; Not_Served: ","DC1:",dc1,"DC2:",dc2,"DC3:",dc3,"DC4:",dc4,"Total:",dc1+dc2+dc3+dc4}'`
	LINE8=`grep "Info Iter" $FILE | awk '{ if($8<$10){dc1+=$8;}else{dc1+=$10} if($16<$18){dc2+=$16;}else{dc2+=$18;} if($24<$26){dc3+=$24;}else{dc3+=$26;} if($32<$34){dc4+=$32}else{dc4+=$34;} } END { print "; Served:","DC1:",dc1,"DC2:",dc2,"DC3:",dc3,"DC4:",dc4,"Total:",dc1+dc2+dc3+dc4}'`
fi



echo $FILE $LINE1 $LINE2 $LINE3 $LINE4 $LINE5 $LINE6 $LINE7 $LINE8

