#!/bin/sh
# generate a path according to prefix location
# for used in Bind_Text_Domain

if [ "$#" != "1" ]; then
    echo "please enter prefix location."
    exit 1
fi

prefix=$1
locale_dir=$prefix/share/locale
share_dir=$prefix/share

path_ads=src/adaview-path.ads

function generate_path_ads() {
    echo "Generating $path_ads for $prefix"
    gen_time=`date +"%F %R%z"`
    cat > $path_ads <<EOF
-- This is an automatically generated file during compilation
-- Please don't edit it by hand.
-- Generated at $gen_time

package Adaview.Path is
   Locale : constant String := "${locale_dir}";
   Share  : constant String := "${share_dir}";
end Adaview.Path;
EOF
}

if [ -f "$path_ads" ]; then
    grep -w "\"${locale_dir}\"" $path_ads > /dev/null
    if [ "$?" != "0" ]; then
	generate_path_ads
    fi
else
    generate_path_ads
fi

