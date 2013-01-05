#! /bin/sh

for d in ` ls */SConstruct | sed 's/\/SConstruct//g'`
do
    echo "====================  $d  ===="
    (cd $d; python /home/users/russel/Repositories/Mercurial/Masters/SCons_D_Tooling/bootstrap.py $*)
done
