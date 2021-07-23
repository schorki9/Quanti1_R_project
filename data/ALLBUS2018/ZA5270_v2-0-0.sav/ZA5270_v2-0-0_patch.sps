* Encoding: UTF-8.
***********************************************************************.
* PATCHFILE FOR ALLBUS 2018 (ZA5270).
* FOR USE WITH RELEASE  2.0.0.
* This patch corrects an inaccuracy in the ESeG 
* (European Socio-economic Groups) classification.
***********************************************************************.

*Set working directory:.
cd "<PATH>".
get file = "ZA5270_v2-0-0.sav".

do repeat #ESEG = eseg sceseg peseg feseg meseg
    /#ISCO = isco08 scisco08 pisco08 fisco08 misco08.
if (range(#ISCO, 7100, 9129) or range(#ISCO, 9300, 9629)) and (#ESEG eq 40) #ESEG = 43.
end repeat.
execute.
add value labels eseg sceseg peseg feseg meseg 
    43 "CRAFT ETC. SELF-EMP." 
    54 "PROTECTIVE SERVICES".

save outfile = "ZA5270_v2-0-0_patched.sav".