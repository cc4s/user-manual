#!/usr/bin/env bash
title=$1
folder=$2

echo "#+title: $title"
echo ""
echo ""

for i in $folder/*.org; do
  [[ $i =~ *sitemap.org ]] && continue
  echo "- [[file:./`basename $i`][`basename "$i" .org`]]"
done
