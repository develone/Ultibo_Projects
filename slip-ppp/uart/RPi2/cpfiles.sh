#!/bin/bash
cp uMySlip.pas uMySlip.pas.tmp
cp SlipDriverDev.lpr SlipDriverDev.lpr.tmp
md5sum uMySlip.pas;md5sum uMySlip.pas.tmp
md5sum SlipDriverDev.lpr; md5sum SlipDriverDev.lpr.tmp