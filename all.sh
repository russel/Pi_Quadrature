#! /bin/sh

banner() {
    echo "========================  $1  ===="
}

for d in ` ls */SConstruct | sed 's/\/SConstruct//g'`
do
    if [ $# -gt 0 -a $1 = clean ]; then
        parameter=-c
    else
        parameter="$*"
    fi
    banner $d
    (cd $d; python /home/users/russel/Repositories/Mercurial/Masters/SCons_D_Tooling/bootstrap.py $parameter)
done

banner Java
(cd Java; gradlew $*)
