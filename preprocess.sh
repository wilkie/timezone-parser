mkdir -p tzdata
cd tzdata
wget 'ftp://elsie.nci.nih.gov/pub/tz*.tar.gz'
gzip -dc tzdata*.tar.gz | tar -xf -
rm -f factory
rm -f leapseconds
rm -f solar*
rm -f *.sh
rm -f *.tab
rm -f systemv
rm -f etcetera
rm -f *.gz
grep -E "^Zone\s+\S+\s+\S+\s+\S+\s+|^\s\s\s+[-0-9]" * > zones
grep -E "^Rule\s+\S+\s+\S+\s+\S+\s+" * > rules
cd ..
mkdir -p tzcode/rules
mkdir -p tzcode/zones
