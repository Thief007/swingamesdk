#!/bin/sh

cp ../src/SGSDK.pas .
python sgsdk_pas_to_cs.py
rm SGSDK.pas