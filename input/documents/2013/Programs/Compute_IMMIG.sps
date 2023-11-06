*** COUNTRY OF BIRTH ***.
mis values IS1G04A IS1G04B IS1G04C ().

*cros cntry by  IS1G04A IS1G04B IS1G04C .
recode  IS1G04A IS1G04B IS1G04C (1=1)(0,2 thru 6=2)(else=9) INTO SBORN MBORN FBORN.
formats  SBORN MBORN FBORN (F1.0).


compute S_IMMIG=(SBORN*100)+(MBORN*10)+FBORN.
freq S_IMMIG.
recode S_IMMIG(222,229,292=3)(122,129,192=2)
   (199,299,911 thru 999=9) (111,121,112,212,221,191,119,291,219,211=1).

if (IS1G04A=8 and IS1G04B=8 and IS1G04C=8) S_immig=8.
if (IS1G04A=9 and IS1G04B=9 and IS1G04C=9) S_immig=9.

formats S_immig (f1.0).
mis val S_immig IS1G04A IS1G04B IS1G04C (8,9) .
var lab S_immig 'Country of birth'.
val lab S_immig 
    1 'Non-immigrant students'
		2 'First-Generation students'
		3 'Immigrant students'
    7 'Invalid'
    8 'Not administered'
    9 'Missing'.
exec.

fre S_IMMIG.
